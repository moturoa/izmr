#' R6 class for connection to anonymized database.
#' @param config_file Not used at the moment (data in SQLite).
#' @param schema Not used at the moment.
#' @param filename Full path to the SQLite database.
#' @param pool If TRUE, uses pool to connect.
#' @export
pseudoData <- R6::R6Class(lock_objects = FALSE,
  
  public = list(
    
    #' @field con Database connection object
    con = NULL,  
    
    #' @field pool Logical, whether the connection is pool(ed) or not.
    pool = NULL,
    
    #' @field schema Database schema in use.
    schema = '',
    
    #' @description Create a new pseudoData (IZM) object
    #' @param config_file Location of the config file with database passwords.
    #' @param schema Schema of the DB where data is stored.
    #' @param filename If SQLite (so far, the default), path to the SQLite database file.
    #' @return A new pseudoData object
    initialize = function(config_file = NULL, 
                          schema = "", 
                          filename = NULL,
                          pool = FALSE){
      
      self$schema <- schema
      self$schema_sql <- if(self$schema == "")"" else paste0(self$schema, ".")
      
      self$pool <- pool
      
      # SQLite
      if(!is.null(filename)){
        if(!pool){
          self$con <- DBI::dbConnect(RSQLite::SQLite(),
                                     dbname = filename)  
        } else {
          self$con <- pool::dbPool(RSQLite::SQLite(),
                                   dbname = filename)  
        }  

      # Postgres        
      } else {
        
        self$con <- shintobag::shinto_db_connection(file = config_file, 
                                                    what = "ede-izm-data",
                                                    pool = pool)
        
      }
      
      
      
      
    },
    
    #----- Algemene methodes ------
    
    #' @description Close the database connection
    close = function() { 
      
      if(self$pool){
        pool::poolClose(self$pool)
      } else {
        dbDisconnect(self$con)
      }
      
    },
    
    #' @description Perform dbGetQuery on the DB connection
    #' @param txt The SQL query
    #' @param glue Logical, if TRUE glues the txt param.
    query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        dbGetQuery(self$con, txt)
      )
      
    },
    
    execute_query = function(txt, glue = TRUE){
      
      if(glue)txt <- glue::glue(txt)
      
      try(
        dbExecute(self$con, txt)
      )
      
    },
    
    
    read_table = function(table, lazy = FALSE){
      
      if(self$schema != ""){
        out <- tbl(self$con, in_schema(self$schema, table))  
      } else {
        out <- tbl(self$con, table)
      }
      
      
      if(!lazy){
        out <- collect(out)
      }
      
      out
      
    },
    
    
    #----- IZM specifieke methodes -----
    
    #' @description Replace all NAs in a dataframe with ""
    #' @param data A dataframe (/tibble)
    replace_na_char = function(data){
      data %>% mutate_if(is.character, list(~na_if(., "")))
    },
    
    
    #------ Family constructors -------
    
    #' @description Read personal data based on pseudo-id(s).
    #' @details Reads pseudo data from bzsprsq00.
    #' @param pseudo_id Vector of pseudo-ids.
    #' @param what The pseudo-id is either a (pseudo) BSN, A-nummer, or adres.
    #' @param adres If what="adres", provide the address as a list with components vblpostcode, etc.
    #' @return A dataframe
    get_person_brp = function(pseudo_id = NULL, what = c("bsn", "anr", "adres"), adres = NULL){
      
      # Is the pseudo_id a BSN or an ANR?
      what <- match.arg(what)
      
      # missing IDs
      pseudo_id[pseudo_id == ""] <- NA_character_

      # could be argument
      columns <- c("vwsgemeentevaninschrijvingomschrijving", 
                   "vwsdatuminschrijving", 
                   "prsgeslachtsaanduidingcode",  # as geslacht, 
                   "ovldatumoverlijden", #as overleden, 
                   "prsanummer", #as anr, 
                   "ou1anummer", #as anrouder1, 
                   "ou2anummer", #as anrouder2, 
                   "prsburgerservicenummer", #as pseudo_bsn, 
                   "prsgeboortedatum",
                   "prsgeboortelandcode",
                   "prsgeboortelandomschrijving",
                   "prsburgerlijkestaat",
                   "vblpostcode", 
                   "vblstraatnaam", 
                   "vblhuisnummer", 
                   "vblhuisletter", 
                   "vblhuisnummertoevoeging",
                   "vblwoonplaatsnaam")
                   
      sel_sql <- paste(columns, collapse= ", ")
      
      
      if(what %in% c("bsn", "anr")){
        
        # gegevens van POI
        search_col <- switch(what,
                             bsn = "prsburgerservicenummer",
                             anr = "prsanummer"
        )
        
        id_search <- private$to_sql_string(pseudo_id)
        
        out <- self$query(glue("select {sel_sql} from {self$schema_sql}bzsprsq00 where",
                               " {search_col} IN {id_search};")) 
        
      } else if(what == "adres"){
        
        q7 <- glue("select {sel_sql} from {self$schema_sql}bzsprsq00 where ",
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
                           geboortedatum = prsgeboortedatum,
                           overleden = ovldatumoverlijden,
                           anr = prsanummer,
                           anrouder1 = ou1anummer,
                           anrouder2 = ou2anummer,
                           pseudo_bsn = prsburgerservicenummer
                           ) %>%
        mutate(geboortedatum = as.Date(geboortedatum, "%Y%m%d"))
      
      out
    },
    
    #' @description  Find (pseudo) A-nummer from a (pseudo) BSN
    #' @param pseudo_id Vector of pseudo-ids.
    #' @return A vector
    anummer_from_bsn = function(pseudo_id){
      
      pseudo_id <- pseudo_id[pseudo_id != "" & !is.na(pseudo_id)]
      
      bsn_string <- private$to_sql_string(pseudo_id)
      self$query(glue("select prsanummer from {self$schema_sql}bzsprsq00 where prsburgerservicenummer IN {bsn_string}")) %>%
        pull(prsanummer)
      
    },
    
    
    #' @description Set relation column in a dataframe
    #' @param data A dataframe
    #' @param relation The relation to be set
    #' @return Dataframe
    set_relation = function(data, relation){
      
      data$relation <- as.character(relation)
      data
      
    },
    
    #' @description Write parent relation to dataframe
    #' @details If Geslacht = "M", set relation = "vader", otherwise "moeder"
    #' @param Dataframe with at least 'geslacht' column
    #' @return Dataframe
    set_parent_relation = function(data){
      
      relation <- ifelse(data$geslacht == "M", "vader", "moeder")
      
      self$set_relation(data, relation)
    },
    
    #' @description Write child relation to dataframe
    #' @details If Geslacht = "M", set relation = "zoon", otherwise "dochter"
    #' @param Dataframe with at least 'geslacht' column
    #' @return Dataframe
    set_kind_relation = function(data){
      relation <- ifelse(data$geslacht == 'M', 'zoon', 'dochter')
      relation[is.na(relation)] <- "kind (geslacht onbekend)"
      self$set_relation(data, relation)
    },
    
    #' @description Read huwelijk data
    #' @details Read from bzshuwq00 table, based on provided pseudo-ids.
    #' @param pseudo_id Vector of pseudo-ids (BSN)
    #' @return Dataframe
    get_huwelijk = function(pseudo_id, what = "bsn"){
      
      # mag hier alleen bsn zijn!
      # match.arg dus niet nodig, alleen voor safety
      what <- match.arg(what)
      
      
      q_txt <- glue("select huwdatumsluitinghuwelijkpartnerschap as begindatum,", 
                "huwanummer as anr, huwdatumontbindinghuwelijkpartnerschap as einddatum, ",
                "huwburgerservicenummer as pseudo_bsn from {self$schema_sql}bzshuwq00 where ",
                "prsburgerservicenummer = '{pseudo_id}';") 
      
      out <- self$query(q_txt)
      
      # replace NA with ""
      #out <- self$replace_na_char(out)
      
      # relation, datums
      out <- out %>% 
        mutate(relation = ifelse(is.na(einddatum) |  einddatum == '', 'partner','ex partner'),
               relation = as.character(relation),
               begindatum = as.Date(begindatum, "%Y%m%d"),
               einddatum = as.Date(einddatum, "%Y%m%d"))
      
               # begindatum = strftime(ymd(begindatum), '%d-%m-%Y'), 
               # einddatum = strftime(ymd(einddatum), '%d-%m-%Y'))
      
      # duplicate rows
      out %>% distinct(pseudo_bsn, .keep_all = TRUE)
      
      
    },
    

    #' @description Read children data
    #' @details Read from bzskinq00 table, based on provided pseudo-ids.
    #' @param pseudo_id Vector of pseudo-ids (BSN)
    #' @return Dataframe
    get_kinderen = function(pseudo_id, what = "bsn"){
      
      # mag hier alleen bsn zijn!
      # match.arg dus niet nodig, alleen voor safety
      what <- match.arg(what)
      
      anr <- self$anummer_from_bsn(pseudo_id)
      q_txt <- glue("select prsanummer as anr, kndanummer, kndgeboortedatum from {self$schema_sql}bzskinq00 where prsanummer = '{anr}';")
      kids_poi_anr <- self$query(q_txt)
      
      if(nrow(kids_poi_anr) == 0)return(NULL)
      
      out <- self$get_person_brp(kids_poi_anr$kndanummer, what = "anr")
      
      out <- left_join(kids_poi_anr, out, by = "anr") %>%
        mutate(geboortedatum = as.Date(kndgeboortedatum, "%Y%m%d"))
      
      
      out <- self$set_kind_relation(out)
        
      out
      
    },
    
    #' @description Read (pseudo) family data for provided pseudo-ids.
    #' @details Read BRP data for provided pseudo-id, 
    #' @param pseudo_id A single pseudo-id
    #' @return Dataframe
    get_family = function(pseudo_id, what = c("bsn", "anr")){
      
        what <- match.arg(what)
      
        stopifnot(length(pseudo_id) == 1)
        
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
    
    #' @description Filter a table based on 'after this date'
    #' @details SQLite does not have a DATE class, so we need this special method.
    #' @param startdatum Date after (and including), must be Date class.
    #' @param table Name of table to filter
    #' @param column Name of column with date
    #' @return Dataframe
    get_sinds_char_column = function(startdatum, table, column){
  
      dt <- format(startdatum, "%Y-%m-%d")
      
      dbGetQuery(self$con, 
                 glue(
                   "select * from {self$schema_sql}{table} where DATE({column}) >= DATE('{dt}')")
      )
      
      
    },
    
    
    get_adreswijzigingen_sinds = function(startdatum){
      
      self$get_sinds_char_column(startdatum, "bzsc58q00", "vblhstdatuminschrijving")
      
    },
    
    get_geboortes_sinds = function(startdatum){
      
      self$get_sinds_char_column(startdatum, "bzsprsq00", "prsgeboortedatum")

    },
    
    get_huwelijken_sinds = function(startdatum){
      
      self$get_sinds_char_column(startdatum, "bzshuwq00", "huwhstdatumsluitinghuwelijkpartnerschap")
      
    },
    
    get_scheidingen_sinds = function(startdatum){
      
      self$get_sinds_char_column(startdatum, "bzshuwq00", "huwdatumontbindinghuwelijkpartnerschap")
      
    },
    
    get_overlijdens_sinds = function(startdatum){
      
      self$get_sinds_char_column(startdatum, "bzsprsq00", "ovldatumoverlijden")
      
    },
    

    
    #------ Bron constructor -----
    
    #' @description Retrieve all 'bronnen' for a person based on pseudo-id
    #' @param pseudo_bsn A single pseudo-id (BSN) (not vectorized!)
    #' @return A reactive dataframe
    get_all_bronnen = function(pseudo_bsn) {
      
      # Reactive met de-pseudo gegevens.
      brp <- self$get_verhuizingen_depseudo(pseudo_bsn) 
      
      
      reactive({
        
        req(pseudo_bsn())
        
        # Pseudo bronnen
        suite <- self$get_suite(pseudo_bsn()) 
        menscentraal <- self$get_menscentraal(pseudo_bsn()) 
        carel <- self$get_carel(pseudo_bsn()) 
        allegro <- self$get_allegro(pseudo_bsn()) 
        openwave <- self$get_openwave(pseudo_bsn()) 
      
        bind_rows(list(
          suite,
          menscentraal, 
          carel,
          allegro,
          openwave,
          brp()
        )) %>% 
         mutate(begindatum_formatted = strftime(begindatum, "%d-%m-%Y"), 
                einddatum_formatted = strftime(einddatum, "%d-%m-%Y"))
 
      })
      
    },
    
    #' @description Retrieve only depseudo bronnen (no BRP) for multiple persons
    #' @param pseudo_bsn Vector of pseudo-ids, *not* reactive
    get_pseudo_bronnen = function(pseudo_bsn) {

      person_function <- function(id){
        
        # Pseudo bronnen
        suite <- self$get_suite(id) 
        menscentraal <- self$get_menscentraal(id) 
        carel <- self$get_carel(id) 
        allegro <- self$get_allegro(id) 
        openwave <- self$get_openwave(id) 
        
        bind_rows(list(
          suite,
          menscentraal, 
          carel,
          allegro,
          openwave
        )) %>% 
          mutate(begindatum_formatted = strftime(begindatum, "%d-%m-%Y"), 
                 einddatum_formatted = strftime(einddatum, "%d-%m-%Y"))
        
      }

      lapply(pseudo_bsn, person_function) %>%
        setNames(pseudo_bsn)
    
      
    },
     
    
    get_suite = function(pseudo_bsn){
      
      if(is.null(pseudo_bsn))return(NULL)
      
      bsns <- private$to_sql_string(pseudo_bsn)
      q_suite <- glue("select 'Suite' as bron, * from {self$schema_sql}suite where bsn in {bsns};")
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
    
    
    
    
    get_openwave = function(pseudo_id, what='bsn_nummer'){
       
      q_wave <- glue("select 'Open Wave' as bron, module, besluit, zaaksoort, omschrijving, ",
                     "aanvraagdatum as begindatum, besluitdatum as einddatum, ",
                     "bedrijfsnaaam as Bedrijfsnaam, handelsregister from {self$schema_sql}openwave ",
                     " where {what} = '{pseudo_id}';")
      
      self$query(q_wave) %>% mutate(bron = as.character(bron),
                                    omschrijving = as.character(omschrijving),
                                    begindatum = ymd_hms(begindatum), 
                                    einddatum = ymd_hms(einddatum)) %>%
                                      arrange(desc(begindatum)) 
    },
    
    get_carel = function(pseudo_id){
       
      q_carel <- glue("select 'Carel' as bron, 'Melding school: ' || naam_school as omschrijving, ",
                      " behandelaar, melding, start_melding as begindatum, ",
                      " einde_melding as einddatum, naam_school from {self$schema_sql}carel where bsn ='{pseudo_id}';")
      
      self$query(q_carel) %>% 
        mutate(bron = as.character(bron),
               omschrijving = as.character(omschrijving),
               begindatum = dmy(begindatum), 
               einddatum = dmy(einddatum)) %>%
        arrange(desc(begindatum))
      
    },
    
    
    get_allegro = function(pseudo_id){
      
      self$read_table("allegro", lazy = TRUE) %>%
        filter(bsn %in% !!pseudo_id) %>%
        select(
          omschrijving = aanvraag_schulhulp,
          naam_consulent,
          aanvraag_schuldhulp = aanvraag_schulhulp,
          traject_schuld_hulp,
          begindatum = start_datum,
          einddatum = eind_datum,
          pseudo_bsn = bsn
        ) %>%
        collect %>%
        mutate(bron = "Allegro",
               einddatum = as.Date(einddatum, format = "%d-%m-%Y"),
               begindatum = as.Date(begindatum, format = "%d-%m-%Y")
               ) %>%
        relocate(bron)
      
    },
    
    
    get_menscentraal = function(pseudo_id){
      
      out <- self$read_table("menscentraal", lazy = TRUE) %>%
        filter(klant_bsn %in% !!pseudo_id) %>%
        select(
          omschrijving = groepnr,
          zaaktype_id,
          begindatum,
          einddatum,
          status,
          pseudo_bsn = klant_bsn
        ) %>%
        collect 
      
      if(nrow(out) > 0){
        out <- out %>% 
          mutate(bron = "Menscentraal",
                 begindatum = as.Date(begindatum),
                 einddatum = as.Date(einddatum)
          ) %>%
          relocate(bron) %>%
          arrange(desc(begindatum))
      } else {
        out <- out %>%
          mutate(bron = "Menscentraal",
                 begindatum = as.Date(NA),
                 einddatum = as.Date(NA)
          ) %>%
          relocate(bron)
      }
      
      out
      
    },
    
    
    
    #------ Adres -----
    get_verhuizingen = function(pseudo_id) {

        q_brp_verh_hst <- glue("select '' as vblhuisnummer, '' as vblhuisletter, '' as vblstraatnaam, '' as vblhuisnummertoevoeging, vblhstgemeentevaninschrijvingomschrijving as gemeente, vblhstadresopgemaakt, vblhstpostcode as vblpostcode, 'Verhuizing' as bron, 'Verhuisd' as omschrijving, vblhstdatumaanvangadreshouding as begindatum from {self$schema_sql}bzsc58q00 where prsburgerservicenummer = '{pseudo_id}';") 
        brp_verh_hst <- dbGetQuery(self$con, q_brp_verh_hst)  
        
        # alle kolommen naar char - dit voorkomt een (wat zeldzame) bug:
        brp_verh_hst <- dplyr::mutate_all(brp_verh_hst, as.character)
        
        # bzsprsq00 voor huidige adres binnen Ede (staat niet altijd in hst)
        q_brp_pers <- glue("select vwsdatuminschrijving,vwsgemeentevaninschrijvingomschrijving, prsgeboortedatum, ovldatumoverlijden, vblgemeentevaninschrijvingomschrijving as gemeente, vblpostcode, vblstraatnaam, vblhuisnummer, vblhuisletter, vblhuisnummertoevoeging, 'BRP' as bron, vbldatumaanvangadreshouding as begindatum from {self$schema_sql}bzsprsq00 where prsburgerservicenummer = '{pseudo_id}';") 
        brp_persoon <- dbGetQuery(self$con, q_brp_pers)   %>% mutate_if(is.character, list(~na_if(., "")))
        
        # if prs adres is not in hst; add this as verhuizing
        if( brp_persoon$begindatum %notin% brp_verh_hst$begindatum) {
          brp_verh <- brp_persoon %>% filter(begindatum %notin% brp_verh_hst$begindatum)   %>% mutate(omschrijving =  'is Verhuisd', bron = 'Verhuizing')
           
          
          brp_verh_hst <- dplyr::bind_rows(brp_verh_hst,brp_verh)
        }  
        if(!is.na(brp_persoon$ovldatumoverlijden)) { 
          brp_ovl <- data.frame(omschrijving =  'Overleden', bron = 'Overleden', 
                                begindatum = brp_persoon$ovldatumoverlijden) 
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
        
        
        brp_verh_hst <- brp_verh_hst %>% 
          mutate(begindatum = ymd(begindatum)) %>% 
          arrange(desc(begindatum)) 
        #print(brp_verh_hst)
        brp_verh_hst
     
    },
    
    get_brp_verh_hst = function(pseudo_id){
      
      q_brp_verh_hst <- glue("select '' as vblhuisnummer, '' as vblhuisletter, '' as vblstraatnaam, ",
                             "'' as vblhuisnummertoevoeging, vblhstgemeentevaninschrijvingomschrijving as gemeente,",
                             " vblhstadresopgemaakt, vblhstpostcode as vblpostcode, 'Verhuizing' as bron, ",
                             "'Verhuisd' as omschrijving, vblhstdatumaanvangadreshouding as begindatum ",
                             " from {self$schema_sql}bzsc58q00 where prsburgerservicenummer = '{pseudo_id}';")
      
      self$query(q_brp_verh_hst)
      
    },
    
    
    
    
  #------ Depseudonimiseren -----

  #' Only perform the lookup method
  #' Returns a reactive
  rest_lookup = function(pseudo_ids){

    callModule(restCallModule, 
                        uuid::UUIDgenerate(), 
                        pseudo_ids = pseudo_ids, what = "lookup")

  },


  #' Depseudo a table, merge with original data, format adres
  table_depseudo = function(data = reactive(NULL), pseudo_bsn_column = "pseudo_bsn"){
    
    pseudo_ids <- reactive({
      
      data() %>%
        pull(!!pseudo_bsn_column)
      
    })
    
    f_out <- callModule(restCallModule, 
                        uuid::UUIDgenerate(), 
                        pseudo_ids = pseudo_ids, what = "lookup",
                        parse_result = TRUE)
    
    reactive({
      
      j <- setNames("pseudo_bsn", pseudo_bsn_column)
      
      left_join(
        data(),
        f_out(),
        by = j) %>% 
        mutate(adres_display = paste(straatnaam,
                                     huisnummer,
                                     huisletter,
                                     #huisnummertoevoeging,
                                     postcode)
        )
      
    })
    
  },
  
  get_family_depseudo = function(id_in = reactive(NULL)){
    
    fam <- reactive({
      req(id_in())
      self$get_family(id_in(), what = "bsn")
    })
    
    fam_id <- reactive({
      req(fam())
      bsns <- fam() %>% 
        pull(pseudo_bsn)
      bsns[!is.na(bsns)]
    })
    
    f_out <- callModule(restCallModule, 
                        uuid::UUIDgenerate(), 
                        pseudo_ids = fam_id, what = "lookup")
    
    reactive({
      
      req(fam())
      req(nrow(f_out()) > 0)
      
      out <- left_join(fam(), f_out(), 
                by = "pseudo_bsn", 
                suffix = c(".y", ""))  # <- duplicate kolomnamen krijgen van rechts voorrang
      
      out %>%
        mutate(
          adres_display = paste(straatnaam,
                                huisnummer,
                                huisletter,
                                #huisnummertoevoeging,
                                postcode),
          vwsdatuminschrijving = as.Date(vwsdatuminschrijving, "%y%m%d"),
          overleden = as.Date(overleden, "%y%m%d"),
          geboortedatum = as.Date(geboortedatum, "%y%m%d"),
          begindatum = as.Date(begindatum, "%y%m%d"),
          einddatum = as.Date(einddatum, "%y%m%d"),
          naam_tooltip = format_naam_tooltip(naam, overleden),
          adres_tooltip = format_adres_tooltip(
            vwsdatuminschrijving,
            vwsgemeentevaninschrijvingomschrijving,
            straatnaam,
            huisnummer,
            huisletter,
            postcode
          )
        )
      
    })
    
    },
  
    # Use /dev endpoint to search an address.
    get_personen_adres_depseudo = function(adres = reactive(NULL)){
      
      callModule(restAddressCallModule, 
                 uuid::UUIDgenerate(), adres = adres)
      
    },
  
  
    get_verhuizingen_depseudo = function(id_in = reactive(NULL)){
      
      verh <- reactive({
        req(id_in())
        self$get_verhuizingen(id_in())
      })
      
      # select the values to depseudo
      depseu <- reactive({
        x <- c(verh() %>% pull(vblpostcode),   
               verh() %>% pull(vblhuisnummer), 
               verh() %>% pull(vblhstadresopgemaakt),
               verh() %>% pull(vblhuisnummertoevoeging),
               verh() %>% pull(vblstraatnaam))
        x[!is.na(x)] 
      })
      
      # calling REST service
      f_out <- callModule(restCallModule, 
                          uuid::UUIDgenerate(), 
                          pseudo_ids = depseu, what = "depseudo")
      
      # merge depseudo with pseudo!
      reactive({ 
        req(verh())
        #req(nrow(f_out()) > 0)
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
      
      rest_out <- callModule(restCallModule, 
                           uuid::UUIDgenerate(), 
                           pseudo_ids = adres_id, what = "lookup")
      
      
      reactive({
        
        req(adres_data())
        
        # dwz: depseudonimiseer is klaar.
        req(nrow(rest_out()) > 0)
        
        left_join(adres_data(), rest_out(), 
                  by = "pseudo_bsn", 
                  suffix = c(".y", ""))
        
      })
      
    },
  
    #------ Wijzigingen -----
    
    # Wordt niet gebruikt in frontend 
    meldingConstructor = function(days_ago=42){
      
      # PAS OP: niet alle datasets zijn meer beschikbaar
      # Getting BRP mutaties                                                                                                               
      q_brp_huw <-  dbGetQuery(self$con, glue("select 'huwelijk' as bron, 'is getrouwd' as omschrijving, huwhstdatumsluitinghuwelijkpartnerschap as begindatum, prsburgerservicenummer as pseudo_bsn from {self$schema_sql}bzsc55q00 where  DATE(NULLIF(huwhstdatumsluitinghuwelijkpartnerschap, '')) > DATETIME('now','-{days_ago} day');"))
      q_brp_gesch <-  dbGetQuery(self$con, glue("select 'huwelijk' as bron, 'is gescheiden' as omschrijving,  huwhstdatumontbindinghuwelijkpartnerschap as begindatum, prsburgerservicenummer as pseudo_bsn from {self$schema_sql}bzsc55q00 where  DATE(NULLIF(huwhstdatumontbindinghuwelijkpartnerschap, '')) > DATETIME('now','-{days_ago} day');"))
      q_brp_verh <-  dbGetQuery(self$con, glue("select 'verhuizing' as bron,'is verhuisd' as omschrijving,  vblhstdatuminschrijving as begindatum,  prsburgerservicenummer as pseudo_bsn from {self$schema_sql}bzsc58q00 where DATE(NULLIF(vblhstdatuminschrijving, '')) > DATETIME('now','-{days_ago} day');"))
      q_brp_kind <-  dbGetQuery(self$con, glue("select 'kind' as bron, 'heeft een kind gekregen' as omschrijving,  kndgeboortedatum as begindatum, prsburgerservicenummer as pseudo_bsn from {self$schema_sql}bzskinq00 where DATE(NULLIF(kndgeboortedatum, '')) > DATETIME('now','-{days_ago} day');"))
      q_brp_cura <-  dbGetQuery(self$con, glue("select 'curatele' as bron, 'is onder curatele gesteld' as omschrijving, gzvhstdatumvanopneming as begindatum, prsburgerservicenummer as pseudo_bsn from {self$schema_sql}bzsc61q00 where  DATE(NULLIF(gzvhstdatumvanopneming, '')) > DATETIME('now','-{days_ago} day');"))
      q_brp_overl <-  dbGetQuery(self$con, glue("select 'overleden' as bron, 'is overleden' as omschrijving, ovlhstdatumoverlijden as begindatum, prsburgerservicenummer as pseudo_bsn from {self$schema_sql}bzsc56q00 where DATE(NULLIF(ovlhstdatumoverlijden, '')) > DATETIME('now','-{days_ago} day');"))
      
      
      returnableList <- list(q_brp_huw,q_brp_gesch, q_brp_verh, q_brp_kind, q_brp_overl,q_brp_cura) 
      
      as.data.frame(data.table::rbindlist(returnableList, idcol = TRUE, fill=TRUE)) %>% arrange(desc(begindatum))
      
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



# depseudonimiseer adres gegevens, postcode voor bronnen
replaceAll <- function(df, encoded){
  
  df_orig <- df
  
  if(nrow(encoded) == 0){
    return(df_orig)
  }
  
  out <- tryCatch(
    { 
      # create named list
      lookup <- setNames(as.character(encoded$value), encoded$pseudo_value)
      
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

}
    
    