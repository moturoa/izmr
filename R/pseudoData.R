#' R6 class for connection to anonymized database.
#' @param config_file Not used at the moment (data in SQLite).
#' @param schema Not used at the moment.
#' @param filename Full path to the SQLite database.
#' @param pool If TRUE, uses pool to connect.
#' @export
pseudoData <- R6::R6Class(
  
  public = list(
    
    con = NULL,  
    pool = NULL,
    schema = '',
    
    
    initialize = function(config_file = getOption("shintobag_conf", "conf/config.yml"), 
                          schema = "", 
                          filename = NULL,
                          pool = FALSE){
      self$schema <- schema
      
      self$pool <- pool
      
      if(!pool){
        self$con <- DBI::dbConnect(RSQLite::SQLite(),
                                   dbname = filename)  
      } else {
        self$con <- pool::dbPool(RSQLite::SQLite(),
                                   dbname = filename)  
      }
      
      
      
    },
    
    #----- Algemene methodes ------
    
    close = function() { 
      
      if(self$pool){
        pool::poolClose(self$pool)
      } else {
        dbDisconnect(self$con)
      }
      
    },
    
    query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      #flog.info(glue("query({txt})"), name = "DBR6")
      
      try(
        dbGetQuery(self$con, txt)
      )
      
    },
    
    listTables = function(){
      dbListTables(self$con)
    },
    
    
    
    #----- IZM specifieke methodes -----
    
    
    replace_na_char = function(data){
      data %>% mutate_if(is.character, list(~na_if(., "")))
    },
    
    
    
    #------ Family constructors -------
    
    # 1 functie om persoon/personen op te halen op basis van bsn, anummer of adres.
    # haalt alle nodige data uit bzsprsq00.
    get_person_brp = function(pseudo_id = NULL, what = c("bsn", "anr", "adres"), adres = NULL){
      
      # Is the pseudo_id a BSN or an ANR?
      what <- match.arg(what)

      # could be argument
      columns <- c("vwsgemeentevaninschrijvingomschrijving", 
                   "vwsdatuminschrijving", 
                   "prsgeslachtsaanduidingcode",  # as geslacht, 
                   "ovldatumoverlijden", #as overleden, 
                   "prsanummer", #as anr, 
                   "ou1anummer", #as  anrouder1, 
                   "ou2anummer", #as anrouder2, 
                   "prsburgerservicenummer", #as pseudo_bsn, 
                   "vblpostcode", 
                   "vblstraatnaam", 
                   "vblhuisnummer", 
                   "vblhuisletter", 
                   "vblhuisnummertoevoeging")
      sel_sql <- paste(columns, collapse= ", ")
      
      
      if(what %in% c("bsn", "anr")){
        
        # gegevens van POI
        search_col <- switch(what,
                             bsn = "prsburgerservicenummer",
                             anr = "prsanummer"
        )
        
        id_search <- private$to_sql_string(pseudo_id)
        
        out <- self$query(glue("select {sel_sql} from bzsprsq00 where",
                               " {search_col} IN {id_search};")) 
        
      } else if(what == "adres"){
        
        q7 <- glue("select {sel_sql} from bzsprsq00 where ",
                  "vblpostcode = '{adres$vblpostcode}' and ",
                  "vblhuisnummer = '{adres$vblhuisnummer}' and ",
                  "vblhuisletter = '{adres$vblhuisletter}' and ",
                  "vblhuisnummertoevoeging = '{adres$vblhuisnummertoevoeging}';")
        
        out <- self$query(q7)
        
      }
      
      
      # fill missing
      #out <- self$replace_na_char(out)
      
      # Rename cols.
      out <- dplyr::rename(out,
                           geslacht = prsgeslachtsaanduidingcode,
                           overleden = ovldatumoverlijden,
                           anr = prsanummer,
                           anrouder1 = ou1anummer,
                           anrouder2 = ou2anummer,
                           pseudo_bsn = prsburgerservicenummer
                           )
      
      out
    },
    
    set_relation = function(data, relation){
      
      data$relation <- as.character(relation)
      data
      
    },
    
    set_parent_relation = function(data){
      
      relation <- ifelse(data$geslacht == "M", "vader", "moeder")
      
      self$set_relation(data, relation)
    },
    
    set_kind_relation = function(data){
      relation <- ifelse(data$geslacht == 'M', 'zoon', 'dochter')
      self$set_relation(data, relation)
    },
    
    get_huwelijk = function(pseudo_id, what = "bsn"){
      
      # mag hier alleen bsn zijn!
      # match.arg dus niet nodig, alleen voor safety
      what <- match.arg(what)
      
      
      q_txt <- glue("select huwdatumsluitinghuwelijkpartnerschap as begindatum,", 
                "huwanummer as anr, huwdatumontbindinghuwelijkpartnerschap as einddatum, ",
                "huwburgerservicenummer as pseudo_bsn from bzshuwq00 where ",
                "prsburgerservicenummer = '{pseudo_id}';") 
      
      out <- self$query(q_txt)
      
      # replace NA with ""
      #out <- self$replace_na_char(out)
      
      # relation, datums
      out <- out %>% 
        mutate(relation = ifelse(is.na(einddatum) |  einddatum == '', 'partner','ex partner'),
               relation = as.character(relation))
      
               # begindatum = strftime(ymd(begindatum), '%d-%m-%Y'), 
               # einddatum = strftime(ymd(einddatum), '%d-%m-%Y'))
      
      # duplicate rows
      out %>% distinct(pseudo_bsn, .keep_all = TRUE)
      
      
    },
    

    
    get_kinderen = function(pseudo_id, what = "bsn"){
      
      # mag hier alleen bsn zijn!
      # match.arg dus niet nodig, alleen voor safety
      what <- match.arg(what)
      
      
      q_txt <- glue("select kndanummer from bzskinq00 where prsburgerservicenummer = '{pseudo_id}';")
      kids_poi_anr <- self$query(q_txt)
      
      out <- self$get_person_brp(kids_poi_anr$kndanummer, what = "anr")
      
      out <- self$set_kind_relation(out)
        
      out
      
    },
    
    
    get_family = function(pseudo_id, what = c("bsn", "anr")){
      
        what <- match.arg(what)
      
        poi <- self$get_person_brp(pseudo_id, what = what) %>%
          self$set_relation("persoon_poi")
      
        parent1 <- self$get_person_brp(poi$anrouder1, what = "anr") %>%
          self$set_parent_relation()
        parent2 <- self$get_person_brp(poi$anrouder2, what = "anr") %>%
          self$set_parent_relation()
        
        
        huwelijk <- self$get_huwelijk(poi$pseudo_bsn) 
        
        kinderen <- self$get_kinderen(poi$pseudo_bsn)
        
        
        return(bind_rows(
            list(
              poi,
              parent1, 
              parent2,
              huwelijk,
              kinderen
            )
          )
        )
        
    },
    
    #------ Bron constructor -----
    get_all_bronnen = function(pseudo_bsn) {
      
      bron <- reactive({
        req(pseudo_bsn())
        suite <- self$get_suite(pseudo_bsn()) 
        menscentraal <- self$get_menscentraal(pseudo_bsn()) 
        carel <- self$get_carel(pseudo_bsn()) 
        allegro <- self$get_allegro(pseudo_bsn()) 
        openwave <- self$get_openwave(pseudo_bsn()) 
        brp <- self$get_verhuizingen_depseudo(pseudo_bsn) 
      
        
        df_total <- bind_rows(list(
          suite,
          menscentraal, 
          carel,
          allegro,
          openwave,
          brp()
        )) %>% mutate(begindatum_formatted = strftime(begindatum, "%d-%m-%Y"), einddatum_formatted = strftime(einddatum, "%d-%m-%Y"))
 
      return( df_total)
      })
    },
     
    
    get_suite = function(pseudo_bsn){
      
      if(is.null(pseudo_bsn))return(NULL)
      
      q_suite <- glue("select 'Suite' as bron, * from suite where bsn = '{pseudo_bsn}';")
      suite <- self$query(q_suite)
      
      if(nrow(suite) > 0){
        suite <- self$replace_na_char(suite)
      }
      
      
      suite <- mutate(suite, 
                      begindatum = coalesce(
                        a_dd_begin,
                        b_dd_aanvr,
                        c_dd_begin,
                        d_dd_begin,
                        e_dd_aanvr,
                        f_dd_st_pe,
                        h_t_begin
                      ),
                      einddatum = coalesce(
                        a_dd_eind,
                        c_dd_eind,
                        d_dd_eind,
                        h_t_eind
                      )
                      ) %>%
        arrange(begindatum)
      
      
      suite <- mutate(suite, 
                      begindatum =  dmy(begindatum), 
                      einddatum =  dmy(einddatum),
                      bron = as.character(bron),
                      omschrijving = paste(
                        ifelse(!is.na(a_waarschuwingen), glue('Waarschuwing: {a_waarschuwingen} '), ''), 
                        ifelse(!is.na(b_huidige_behandelaar) | !is.na(b_regeling) | !is.na(b_soort_wp) | !is.na(b_reden), 'Afwijzing', ''),
                        ifelse(!is.na(b_huidige_behandelaar), glue(', behandelaar {b_huidige_behandelaar} '), ''),
                        ifelse(!is.na(b_regeling), glue(', regeling: {b_regeling} '), ''),
                        ifelse(!is.na(b_soort_wp), glue(', WP: {b_soort_wp} '), ''),
                        ifelse(!is.na(b_reden), glue(', reden: {b_reden} '), ''),
                        ifelse(!is.na(c_huidige_behandelaar) | !is.na(c_zorgaanbieder), 'WMO', ''),
                        ifelse(!is.na(c_huidige_behandelaar), glue(', behandelaar: {c_huidige_behandelaar} '), ''),
                        ifelse(!is.na(c_zorgaanbieder), glue('({c_zorgaanbieder})'), ''),
                        ifelse(!is.na(d_huidige_behandelaar) | !is.na(d_zorgaanbieder), 'Jeugd', ''), 
                        ifelse(!is.na(d_huidige_behandelaar), glue('Jeugd behandelaar: {d_huidige_behandelaar} '), ''),
                        ifelse(!is.na(d_zorgaanbieder), glue('({d_zorgaanbieder}) '), ''),
                        ifelse(!is.na(e_huidige_behandelaar) | !is.na(e_soort_wp) | !is.na(e_fase) | !is.na(e_status_aanvraag), 'Lopende voorziening', ''), 
                        ifelse(!is.na(e_huidige_behandelaar), glue(', behandelaar: {e_huidige_behandelaar}'), ''),                       
                        ifelse(!is.na(e_soort_wp), glue(', WP: {e_soort_wp} '), ''),
                        ifelse(!is.na(e_fase), glue(', fase: {e_fase} '), ''),
                        ifelse(!is.na(e_status_aanvraag), glue(', status: {e_status_aanvraag} '), ''),
                        ifelse(!is.na(f_regeling) | !is.na(f_huidige_behandelaar), 'Periodiek algemene regeling', ''),
                        ifelse(!is.na(f_regeling), glue(': {f_regeling} '), ''),
                        ifelse(!is.na(f_huidige_behandelaar), glue(', behandelaar: {f_huidige_behandelaar} '), ''),
                        ifelse(!is.na(h), glue('Participatie naar werk traject '), ''),
                        ifelse(!is.na(h_huidige_behandelaar), glue(', behandelaar: {h_huidige_behandelaar} '), ''),
                        ifelse(!is.na(h_a_begin), glue(', startdatum participatie activiteit voorziening: {h_a_begin} '), '')
                        ,sep = ''
                      )
      )
      
      suite
                                                                        
    },
    
    
    get_menscentraal = function(pseudo_id){
      
      q_mens <- glue("select 'Mens Centraal' as bron, 'Groepnummer ' || groepnr as omschrijving, ",
                    " zaaktype_id, begindatum	as begindatum, einddatum as einddatum, ",
                    "status, groepnr from menscentraal where klant_bsn = '{pseudo_id}';") 
      
      self$query(q_mens) %>% 
        mutate(bron = as.character(bron),
               omschrijving = as.character(omschrijving), 
               begindatum =  as_date(ymd_hms(begindatum)), 
               einddatum =  as_date(ymd_hms(einddatum)))  %>%
        arrange(desc(begindatum))
      
    },
    
    get_openwave = function(pseudo_id, what='bsn_nummer'){
       
      q_wave <- glue("select 'Open Wave' as bron, module, zaaksoort, omschrijving, ",
                     "aanvraagdatum as begindatum, besluitdatum, besluit as einddatum, ",
                     "bedrijfsnaaam as Bedrijfsnaam, handelsregister from openwave ",
                     " where {what} = '{pseudo_id}';")
      
      self$query(q_wave) %>% mutate(bron = as.character(bron),
                                    omschrijving = as.character(omschrijving),
                                    begindatum = ymd(begindatum), 
                                    einddatum = ymd(einddatum)) %>%
                                      arrange(desc(begindatum)) 
    },
    
    get_carel = function(pseudo_id){
       
      q_carel <- glue("select 'Carel' as bron, 'Melding school: ' || naam_school as omschrijving, ",
                      " behandelaar, melding, start_melding as begindatum, ",
                      " einde_melding as einddatum, naam_school from carel where bsn ='{pseudo_id}';")
      
      self$query(q_carel) %>% 
        mutate(bron = as.character(bron),
               omschrijving = as.character(omschrijving),
               begindatum = dmy(begindatum), 
               einddatum = dmy(einddatum)) %>%
        arrange(desc(begindatum))
      
    },
    
    get_allegro = function(pseudo_id){
       
      q_allegro <- glue("select 'Allegro' as bron, aanvraag_schulhulp as omschrijving, ",
                        "naam_consulent, aanvraag_schulhulp as aanvraag_schuldhulp, ",
                        "traject_schuld_hulp, start_datum as begindatum, ",
                        "eind_datum as einddatum from allegro where bsn = '{pseudo_id}';") 
      
      self$query(q_allegro) %>%
        mutate(bron = as.character(bron),omschrijving = as.character(omschrijving), begindatum = dmy(begindatum), einddatum = dmy(einddatum))
    },
    
    
    
    #------ Adres -----
    get_verhuizingen = function(pseudo_id) {

        q_brp_verh_hst = glue("select '' as vblhuisnummer, '' as vblhuisletter, '' as vblstraatnaam, '' as vblhuisnummertoevoeging, vblhstgemeentevaninschrijvingomschrijving as gemeente, vblhstadresopgemaakt, vblhstpostcode as vblpostcode, 'Verhuizing' as bron, 'Verhuisd' as omschrijving, vblhstdatumaanvangadreshouding as begindatum from bzsc58q00 where prsburgerservicenummer = '{pseudo_id}';") 
        brp_verh_hst <- dbGetQuery(self$con, q_brp_verh_hst)  
        
        
        # bzsprsq00 voor huidige adres binnen Ede (staat niet altijd in hst)
        q_brp_pers = glue("select vwsdatuminschrijving,vwsgemeentevaninschrijvingomschrijving, prsgeboortedatum, ovldatumoverlijden, vblgemeentevaninschrijvingomschrijving as gemeente, vblpostcode, vblstraatnaam, vblhuisnummer, vblhuisletter, vblhuisnummertoevoeging, 'BRP' as bron, vbldatumaanvangadreshouding as begindatum from bzsprsq00 where prsburgerservicenummer = '{pseudo_id}';") 
        brp_persoon <- dbGetQuery(self$con, q_brp_pers)   %>% mutate_if(is.character, list(~na_if(., "")))
        
        # if prs adres is not in hst; add this as verhuizing
        
        if( brp_persoon$begindatum %notin% brp_verh_hst$begindatum) {
          brp_verh <- brp_persoon %>% filter(begindatum %notin% brp_verh_hst$begindatum)   %>% mutate(omschrijving =  'is Verhuisd', bron = 'Verhuizing')
           
          
          brp_verh_hst <- dplyr::bind_rows(brp_verh_hst,brp_verh)
        }  
        if(!is.na(brp_persoon$ovldatumoverlijden)) { 
          brp_ovl <- data.frame(omschrijving =  'Overleden', bron = 'Overleden', begindatum = brp_persoon$ovldatumoverlijden) 
          brp_verh_hst <- dplyr::bind_rows(brp_verh_hst,brp_ovl)
        }
        if(!is.na(brp_persoon$vwsgemeentevaninschrijvingomschrijving) | !is.na(brp_persoon$vwsdatuminschrijving)) {
          brp_uitgeschreven <- data.frame(omschrijving =  paste('Verhuisd naar ', brp_persoon$vwsgemeentevaninschrijvingomschrijving) , bron = 'Uitgeschreven', begindatum = brp_persoon$vwsdatuminschrijving) 
          brp_verh_hst <-dplyr::bind_rows(brp_verh_hst,brp_uitgeschreven)
        }
        if(!is.na(brp_persoon$prsgeboortedatum)) {
          brp_geboren <- data.frame(omschrijving =  'Geboren', bron = 'Geboren', begindatum = brp_persoon$prsgeboortedatum) 
          brp_verh_hst <- dplyr::bind_rows(brp_verh_hst,brp_geboren)
        }
        
        brp_verh_hst <- brp_verh_hst %>% mutate(begindatum = ymd(begindatum)) %>% arrange(desc(begindatum)) 
        #print(brp_verh_hst)
        brp_verh_hst
     
    },
    
    get_brp_verh_hst = function(pseudo_id){
      
      q_brp_verh_hst <- glue("select '' as vblhuisnummer, '' as vblhuisletter, '' as vblstraatnaam, ",
                             "'' as vblhuisnummertoevoeging, vblhstgemeentevaninschrijvingomschrijving as gemeente,",
                             " vblhstadresopgemaakt, vblhstpostcode as vblpostcode, 'Verhuizing' as bron, ",
                             "'Verhuisd' as omschrijving, vblhstdatumaanvangadreshouding as begindatum ",
                             " from bzsc58q00 where prsburgerservicenummer = '{pseudo_id}';")
      
      self$query(q_brp_verh_hst)
      
    },
    
    
    
    
    #------ Depseudonimiseren -----
     
  
  get_family_depseudo = function(id_in){
    
    fam <- reactive({
      req(id_in())
      self$get_family(id_in(), what = "bsn")
    })
    
    fam_id <- reactive({
      fam() %>% pull(pseudo_bsn)
    })
    
    f_out <- callModule(restCallModule, "fam", pseudo_ids = fam_id, what = "lookup")
    
    
    reactive({
      
      req(fam())
      req(nrow(f_out()) > 0)
      
      left_join(fam(), f_out(), 
                by = "pseudo_bsn", 
                suffix = c(".y", ""))
      
    })
    
    },  
    get_verhuizingen_depseudo = function(id_in){
      
      verh <- reactive({
        req(id_in())
        self$get_verhuizingen(id_in())
      })
      
      # select the values to depsuedo
      depseu <- reactive({
        x <- c(verh() %>% pull(vblpostcode),   
               verh() %>% pull(vblhuisnummer), 
               verh() %>% pull(vblhstadresopgemaakt),
               verh() %>% pull(vblhuisnummertoevoeging),
               verh() %>% pull(vblstraatnaam))
        x[!is.na(x)] 
      })
      
      # calling REST service
      f_out <- callModule(restCallModule, "verh", pseudo_ids = depseu, what = "depseudo")
      
      # merge depseudo with pseudo!
      reactive({ 
        req(verh())
        req(nrow(f_out()) > 0)
        replaceAll(verh(), f_out()) 
        
      }) 
    },
  
  get_adres_depseudo = function(adres){
    
    
    adres_data <- reactive({
      
      req(adres())
      database_object$get_person_brp(what = "adres", adres = adres())
      
    })
    
    adres_id <- reactive({
      adres_data() %>% pull(pseudo_bsn)
    })
    
    rest_out <- callModule(restCallModule, "adres", pseudo_ids = adres_id, what = "lookup")
    
    
    reactive({
      
      req(adres_data())
      
      # dwz: depseudonimiseer is klaar.
      req(nrow(rest_out()) > 0)
      
      left_join(adres_data(), rest_out(), 
                by = "pseudo_bsn", 
                suffix = c(".y", ""))
      
    })
    
  }
  ), 
  
  private = list(
    to_sql_string = function(x){
      
      paste0(
        "('",
        paste(x, collapse="','"),
        "')"
      )
      
    }
  ) 
  
  
  
)
`%notin%` <- Negate(`%in%`)


# depseudonimiseer adres gegevens, postcode voor bronnen
replaceAll <- function(df, encoded){
  df_orig <- copy(df)
  out <- tryCatch(
    { 
      # create named list
      lookup = setNames(as.character(encoded$value), encoded$pseudo_value)
      
      # replace specific values from named list
      df <- transform(df, vblhstadresopgemaakt=ifelse(vblhstadresopgemaakt %in% names(lookup), str_replace(str_replace_all(lookup[vblhstadresopgemaakt], "(?!^)(?=[A-Z])", " "), "(?=\\d)", " "), vblhstadresopgemaakt),
                      vblpostcode=ifelse(vblpostcode %in% names(lookup), lookup[vblpostcode], vblpostcode), 
                      vblhuisnummer=ifelse(vblhuisnummer %in% names(lookup), lookup[vblhuisnummer], vblhuisnummer),
                      vblhuisletter=ifelse(vblhuisletter %in% names(lookup), lookup[vblhuisletter], vblhuisletter),
                      vblhuisnummertoevoeging=ifelse(vblhuisnummertoevoeging %in% names(lookup), lookup[vblhuisnummertoevoeging], vblhuisnummertoevoeging), 
                      vblstraatnaam=ifelse(vblstraatnaam %in% names(lookup), lookup[vblstraatnaam], vblstraatnaam), stringsAsFactors=FALSE)
      
      # create or update omschrijving
      df[is.na(df)] <- ""
      df <- transform(df, omschrijving=ifelse(  vblhstadresopgemaakt != '', glue('Verhuisd naar {vblhstadresopgemaakt}, {vblpostcode} {gemeente}'),omschrijving ))
      df <- transform(df, omschrijving=ifelse( vblstraatnaam != '' , glue('Verhuisd naar {vblstraatnaam} {vblhuisnummer} {vblhuisletter} {vblhuisnummertoevoeging}, {vblpostcode} {gemeente}'),omschrijving ))
      return(df)
    },
    error=function(cond) {   
      cat(file=stderr(), glue('error in replaceAll: {paste(cond, sep = ",")}'), sep= '\n')  
      return(df_orig)
    },
    warning=function(cond) {    
      cat(file=stderr(), glue('warning in replaceAll: {paste(cond, sep = ",")}'), sep= '\n')  
      return(df_orig)
    })
  return(out)
  #df <- transform(df, vblhstadresopgemaakt= lookup[vblhstadresopgemaakt] , stringsAsFactors=FALSE)
  
}
    
    