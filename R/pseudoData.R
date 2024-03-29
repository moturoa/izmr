#' R6 class for connection to anonymized database.
#' @importFrom shintodb databaseClass
#' @param config_file Not used at the moment (data in SQLite).
#' @param schema Not used at the moment.
#' @param filename Full path to the SQLite database.
#' @param pool If TRUE, uses pool to connect.
#' @export
pseudoData <- R6::R6Class(
  inherit = shintodb::databaseClass, 
  lock_objects = FALSE,
  
  public = list(
    

    #' @description Create a new pseudoData (IZM) object
    #' @param config_file Location of the config file with database passwords.
    #' @param schema Schema of the DB where data is stored.
    #' @param filename If SQLite (so far, the default), path to the SQLite database file.
    #' @return A new pseudoData object
    initialize = function(config_file = NULL, 
                          schema = "", 
                          filename = NULL,
                          pool = FALSE,
                          secret = ""){
      
      self$secret <- secret
      
      super$initialize(what = "ede-izm-data",
                       config_file = config_file, schema = schema, sqlite = filename, 
                       pool = pool)
      
      self$schema_sql <- if(self$schema == "")"" else paste0(self$schema, ".")
      
      # burgerlijke staat
      self$burgstaat_key <- tibble::tribble(
        ~code, ~label,
        "0","Onbekend",
        "1","Ongehuwd en nooit gehuwd geweest",
        "2","Gehuwd",
        "3","Gescheiden",
        "4","Weduwe/weduwnaar",
        "5","Partnerschap",
        "6","Partnerschap beëindigd",
        "7","Achtergebleven partner")
      
      
      
    },
    
    
    #' @description Symmetric encrypt, using the secret 
    encrypt = function(x){
      if(self$secret != ""){
        out <- shintodb::encrypt(x, secret = self$secret)
        out[is.na(x)] <- NA_character_
      } else {
        out <- x
      }
      out
    },
    
    #' @description Symmetric decrypt, using the secret 
    decrypt = function(x){
      if(self$secret != ""){
        shintodb::decrypt(x, secret = self$secret)
      } else {
        x
      }
      
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
                   "vblgemeentevaninschrijvingomschrijving",
                   "vwsdatuminschrijving", 
                   "prsgeslachtsaanduidingcode",  # as geslacht, 
                   "prsgeslachtsaanduidingomschrijving",  # as geslacht, 
                   "prsvoornamen",
                   "prsgeslachtsnaam",
                   "prsvoorvoegselgeslachtsnaam",
                   "prsnaamopgemaakt",
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
                   "vblwoonplaatsnaam",
                   "ou1voornamen",
                   "ou1geslachtsnaam",
                   "ou2voornamen",
                   "ou2geslachtsnaam"
                   )
                   
      sel_sql <- paste(columns, collapse= ", ")
      
      if(what %in% c("bsn", "anr")){
        
        # gegevens van POI
        search_col <- switch(what,
                             bsn = "prsburgerservicenummer",
                             anr = "prsanummer"
        )
        
        if(all(is.na(pseudo_id)) || length(pseudo_id[1]) == 0){
          # no ID provided, return an empty table
          out <- self$query(glue("select {sel_sql} from {self$schema_sql}bzsprsq00 where false"))
        } else {
          
          pseudo_id <- pseudo_id[!is.na(pseudo_id)]
          id_search <- private$to_sql_string(pseudo_id)
          
          out <- self$query(glue("select {sel_sql} from {self$schema_sql}bzsprsq00 where",
                                 " {search_col} IN {id_search};")) 
        }
        
        
        
      } else if(what == "adres"){
        
        adres$vblpostcode[is.na(adres$vblpostcode)] <- ""
        adres$vblhuisnummer[is.na(adres$vblhuisnummer)] <- ""
        adres$vblhuisletter[is.na(adres$vblhuisletter)] <- ""
        adres$vblhuisnummertoevoeging[is.na(adres$vblhuisnummertoevoeging)] <- ""
        
        if(all(adres$vblpostcode == "")){
          return(NULL)
        }
        if(all(adres$vblhuisnummer == "")){
          return(NULL)
        }
        
        q7 <- glue("select {sel_sql} from {self$schema_sql}bzsprsq00 where ",
                  "vblpostcode = '{adres$vblpostcode}' and ",
                  "vblhuisnummer = '{adres$vblhuisnummer}' and ",
                  "vblhuisletter = '{adres$vblhuisletter}' and ",
                  "vblhuisnummertoevoeging = '{adres$vblhuisnummertoevoeging}';")
        
        out <- self$query(q7)
        
      }
      
      # Rename cols.
      out <- dplyr::rename(out,
                           pseudo_bsn = prsburgerservicenummer,
                           voornamen = prsvoornamen,
                           geslachtsnaam = prsgeslachtsnaam,
                           voorvoegsel = prsvoorvoegselgeslachtsnaam,
                           geslacht = prsgeslachtsaanduidingomschrijving,
                           geboortedatum = prsgeboortedatum,
                           overleden = ovldatumoverlijden,
                           anr = prsanummer,
                           anrouder1 = ou1anummer,
                           anrouder2 = ou2anummer,
                           geboorteland = prsgeboortelandomschrijving, 
                           straatnaam = vblstraatnaam,
                           huisnummer = vblhuisnummer,
                           huisletter = vblhuisletter,
                           huisnummertoevoeging = vblhuisnummertoevoeging,
                           postcode = vblpostcode,
                           woonplaatsnaam = vblwoonplaatsnaam
                           ) %>%
        # Fill semi-missing geboortedatum
        mutate(geboortedatum = gsub("0000$", "0701", geboortedatum),  # maand/dag onbekend = 1 Juli
               geboortedatum = gsub("00$", "15", geboortedatum)) %>%  # dag onbekend = 15
        mutate(geboortedatum = as.Date(geboortedatum, "%Y%m%d"),
               overleden = as.Date(lubridate::ymd_hms(overleden)),
               ou1geslachtsnaam = na_if(ou1geslachtsnaam, "."),
               ou2geslachtsnaam = na_if(ou2geslachtsnaam, "."))
      
      out
    },
    
    #' @description Util to add 'naam' column to depseudonized form of $get_person_brp,
    #' for example in $get_family --> callModule(depseudo...)
    add_naam_column = function(data, fill_na = NULL){
      
      na_val <- ifelse(is.null(fill_na), NA_character_, as.character(fill_na))
      
      vv <- data$voorvoegsel
      vv[is.na(vv)] <- ""
      
      data$naam <- trimws(paste(vv, data$geslachtsnaam))
      data$naam[is.na(data$geslachtsnaam) | data$geslachtsnaam == ""] <- na_val
      
      data
    },
    
    #' @description Get columns from bzsprsq00 for a vector of BSNs
    get_person_kenmerk = function(pseudo_id, columns){
      
      cols <- unique(c("prsburgerservicenummer", columns))
      
      self$read_table("bzsprsq00", lazy = TRUE) %>%
        filter(prsburgerservicenummer %in% !!pseudo_id) %>%
        select(all_of(cols)) %>%
        collect
      
    },
    
    
    #' @description  Find (pseudo) A-nummer from a (pseudo) BSN
    #' @param pseudo_id Vector of pseudo-ids.
    #' @return A vector
    anummer_from_bsn = function(pseudo_id){
      
      if(is.null(pseudo_id)){
        return(NULL)
      }
      
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
      
      relation <- ifelse(data$prsgeslachtsaanduidingcode == "M", "vader", "moeder")
      self$set_relation(data, relation)
    },
    
    #' @description Write child relation to dataframe
    #' @details If Geslacht = "M", set relation = "zoon", otherwise "dochter"
    #' @param Dataframe with at least 'geslacht' column
    #' @return Dataframe
    set_kind_relation = function(data){
      relation <- ifelse(data$geslacht == 'Man', 'zoon', 'dochter')
      relation[is.na(relation)] <- "kind (geslacht onbekend)"
      relation[data$geslacht == "O"] <- "kind (geslacht onbekend)"
      self$set_relation(data, relation)
    },
    
    #' @description Read huwelijk data
    #' @details Read from bzshuwq00 table, based on provided pseudo-ids.
    #' @param pseudo_id Vector of pseudo-ids (BSN)
    #' @return Dataframe
    get_huwelijk = function(pseudo_id, what = "bsn"){
      
      flog.info("get_huwelijk")
      
      # mag hier alleen bsn zijn!
      # match.arg dus niet nodig, alleen voor safety
      what <- match.arg(what)
      
      
      q_txt <- glue("select huwdatumsluitinghuwelijkpartnerschap as begindatum,", 
                "huwanummer as anr, huwdatumontbindinghuwelijkpartnerschap as einddatum, ",
                "huwburgerservicenummer as pseudo_bsn from {self$schema_sql}bzshuwq00 where ",
                "prsburgerservicenummer = '{pseudo_id}';") 
      
      out <- self$query(q_txt)
      
      # relation, datums
      out <- out %>% 
        mutate(relation = ifelse(is.na(einddatum) |  einddatum == '', 'partner','ex partner'),
               relation = as.character(relation),
               
               begindatum = gsub("0000$", "0701", begindatum),  # maand/dag onbekend = 1 Juli
               begindatum = gsub("00$", "15", begindatum),
               
               einddatum = gsub("0000$", "0701", einddatum),  # maand/dag onbekend = 1 Juli
               einddatum = gsub("00$", "15", einddatum),
  
               begindatum = as.Date(begindatum, "%Y%m%d"),
               einddatum = as.Date(einddatum, "%Y%m%d"))
      
      # duplicate rows
      out <- out %>% distinct(pseudo_bsn, .keep_all = TRUE)
      
      # join person data
      p_brp <- self$get_person_brp(out$pseudo_bsn)  %>%
        select(-anr)
      
      left_join(out, p_brp, by = "pseudo_bsn")
      
    },
    

    #' @description Read children data
    #' @details Read from bzskinq00 table, based on provided pseudo-ids.
    #' @param pseudo_id Vector of pseudo-ids (BSN)
    #' @return Dataframe
    get_kinderen = function(pseudo_id){
      
      flog.info("get_kinderen")
      
      if(length(pseudo_id) > 1){
        warning("$get_kinderen: pass 1 pseudo_id at a time")
        pseudo_id <- pseudo_id[1]
      }
      
      q_txt <- glue("select kndburgerservicenummer from {self$schema_sql}bzskinq00 where prsburgerservicenummer = '{pseudo_id}';")
      kids_poi <- self$query(q_txt)
      
      if(nrow(kids_poi) == 0)return(NULL)
      
      self$get_person_brp(kids_poi$kndburgerservicenummer) %>%
        self$set_kind_relation(.)
      
    },
    
    #' @description Read (pseudo) family data for provided pseudo-ids.
    #' @details Read BRP data for provided pseudo-id, 
    #' @param pseudo_id A single pseudo-id
    #' @return Dataframe
    get_family = function(pseudo_id, what = c("bsn", "anr")){
      
      flog.info("get_family")
      
        what <- match.arg(what)
      
        stopifnot(length(pseudo_id) == 1)
        
        poi <- self$get_person_brp(pseudo_id, what = what) %>%
          self$set_relation("persoon_poi")
      
        parents <- self$get_person_brp(c(poi$anrouder1,poi$anrouder2), what = "anr") %>%
          self$set_parent_relation()
        
        huwelijk <- self$get_huwelijk(poi$pseudo_bsn) 
        
        kinderen <- self$get_kinderen(poi$pseudo_bsn)
        
        return(bind_rows(
            list(
              poi,
              parents, 
              huwelijk,
              kinderen
            )
          )
        )
        
    },
    

    label_burgerlijke_staat = function(code){
      code <- as.character(code)
      ii <- match(code, self$burgstaat_key$code)
      out <- self$burgstaat_key$label[ii]
      out[is.na(out)] <- "Onbekend"
      out
    },
    
    
    
    #------ Bron constructor -----
    
    #' @description Retrieve all 'bronnen' for a person based on pseudo-id
    #' @param pseudo_bsn A single pseudo-id (BSN) (not vectorized!)
    #' @return A reactive dataframe
    get_all_bronnen = function(pseudo_bsn) {

        lis <- list(
          suite = self$get_suite(pseudo_bsn),
          menscentraal = self$get_menscentraal(pseudo_bsn),
          carel = self$get_carel(pseudo_bsn),
          allegro = self$get_allegro(pseudo_bsn),
          openwave = self$get_openwave(pseudo_bsn),
          brp = self$get_verhuizingen(pseudo_bsn)
        )
        
        lis$n_rows <- sum(sapply(dropNulls(lis), nrow))
        lis
      
    },
    
    
    #' @description Retrieve only depseudo bronnen (no BRP) for multiple persons
    #' @param pseudo_bsn Vector of pseudo-ids, *not* reactive
    get_person_bronnen = function(pseudo_bsn) {
      
      person_function <- function(id){
        
        self$get_all_bronnen(id)
        
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
       
      self$read_table("openwave", lazy = TRUE) %>%
        filter(!!sym(what) == !!pseudo_id) %>%
        collect %>%
        mutate(bron = "OpenWave",
               aanvraagdatum = as.Date(ymd_hms(aanvraagdatum)),
               besluitdatum = as.Date(ymd_hms(besluitdatum)))
    },
    
    get_carel = function(pseudo_id){
       
      self$read_table("carel", lazy = TRUE) %>%
        filter(bsn %in% !!pseudo_id) %>%
        collect %>%
        mutate(bron = "Carel")
    },
    
    
    get_allegro = function(pseudo_id){
      
      self$read_table("allegro", lazy = TRUE) %>%
        filter(bsn %in% !!pseudo_id) %>%
        rename(
          pseudo_bsn = bsn
        ) %>%
        collect %>%
        mutate(bron = "Allegro")
      
    },
    
    
    get_menscentraal = function(pseudo_id){
      
      self$read_table("menscentraal", lazy = TRUE) %>%
        filter(klant_bsn %in% !!pseudo_id) %>%
        rename(
          pseudo_bsn = klant_bsn
        ) %>%
        mutate(bron = "Menscentraal") %>%
        collect 
      
    },
    
    get_menscentraal_zaaktypes = function(){
      
      query <- glue("select distinct zaaktype from {self$schema}.menscentraal")
      
      out <- self$get_query(query)
      
      sort(out[[1]])
      
    },
    

    # Aantal bronnen per persoon (voor Linkit, uiteindelijk misschien in IZM nuttig)
    # Zie repos `ede_izm_db_filler` waar deze tabel wordt gemaakt
    get_bronnen_count = function(pseudo_id){
      
      
      self$read_table("bronnen_count_by_person", lazy = TRUE) %>%
        filter(pseudo_bsn %in% !!pseudo_id) %>%
        collect
      
    },
    
    
    
    
    #------ Adres -----
    get_verhuizingen = function(pseudo_id) {

      adres_historie <- self$read_table("bzsc58q00", lazy = TRUE) %>%
        dplyr::filter(prsburgerservicenummer == !!pseudo_id) %>%
        dplyr::select(
          vblstraatnaam = vblhststraatnaam,
          vblhuisnummer = vblhsthuisnummer,
          vblhuisletter = vblhsthuisletter,
          vblhuisnummertoevoeging = vblhsthuisnummertoevoeging,
          vblpostcode = vblhstpostcode,
          vblwoonplaatsnaam = vblhstwoonplaatsnaam,
          vbldatuminschrijving = vblhstdatuminschrijving,
          vbldatumaanvangadreshouding = vblhstdatumaanvangadreshouding,
          vblgemeentevaninschrijvingomschrijving = vblhstgemeentevaninschrijvingomschrijving,
          landvanwaaringeschrevenomschrijving = vblhstlandvanwaaringeschrevenomschrijving) %>%
        collect
      
      
      brp_current <- self$read_table("bzsprsq00", lazy = TRUE) %>%
        dplyr::filter(prsburgerservicenummer == !!pseudo_id) %>%
        dplyr::select(
          vblstraatnaam = vblstraatnaam,
          vblhuisnummer = vblhuisnummer,
          vblhuisletter = vblhuisletter,
          vblhuisnummertoevoeging = vblhuisnummertoevoeging,
          vblpostcode = vblpostcode,
          vblwoonplaatsnaam = vblwoonplaatsnaam,
          vbldatuminschrijving = vbldatuminschrijving,
          vbldatumaanvangadreshouding = vbldatumaanvangadreshouding,
          vblgemeentevaninschrijvingomschrijving = vblgemeentevaninschrijvingomschrijving) %>%
        #mutate(landvanwaaringeschrevenomschrijving = "Nederland") %>%  # staat niet in deze tabel maar elke prs is in nederland (denk ik)
        collect
      
      suppressWarnings({
        tab <- dplyr::bind_rows(
          adres_historie,
          brp_current
        ) %>%
          dplyr::mutate(vbldatumaanvangadreshouding = ymd(vbldatumaanvangadreshouding),
                 vbldatuminschrijving = ymd(vbldatuminschrijving)) %>%
          dplyr::arrange(desc(vbldatumaanvangadreshouding))
          #dplyr::filter(vblstraatnaam != "")  # <- soms lege adressen
      })
      
      tab
     
    },
    

    get_adreswijzigingen_sinds = function(datum, pseudo_id){
      
      tab1 <- self$read_table("bzsc58q00", lazy = TRUE) %>%
        select(bsn = prsburgerservicenummer, 
               straatnaam = vblhststraatnaam,
               huisnummer = vblhsthuisnummer,
               huisletter = vblhsthuisletter,
               huisnummertoevoeging = vblhsthuisnummertoevoeging,
               postcode = vblhstpostcode,
               woonplaatsnaam = vblhstwoonplaatsnaam,
               datum_adres = vblhstdatumaanvangadreshouding) %>%
        filter(datum_adres != "" & 
               datum_adres != "0" &
               as.Date(datum_adres) >= !!datum,
               bsn %in% !!pseudo_id) %>%
        collect
      
      tab2 <- self$read_table("bzsprsq00", lazy = TRUE) %>%
        select(bsn = prsburgerservicenummer, 
               naam = prsgeslachtsnaam,
               voornamen = prsvoornamen,
               straatnaam = vblstraatnaam,
               huisnummer = vblhuisnummer,
               huisletter = vblhuisletter,
               huisnummertoevoeging = vblhuisnummertoevoeging,
               postcode = vblpostcode,
               woonplaatsnaam = vblwoonplaatsnaam,
               datum_adres = vbldatumaanvangadreshouding) %>%
        filter(datum_adres != "" & 
                 datum_adres != "0" & 
                 as.Date(datum_adres) >= datum,
               bsn %in% !!pseudo_id) %>%
        
        collect
      
      tab1$naam <- tab2$naam[match(tab1$bsn, tab2$bsn)]
      
      rbind(tab2, tab1) %>% 
        arrange(desc(datum_adres)) %>%
        mutate(adres_display = self$make_adres_display(straatnaam, huisnummer, 
                                                       huisletter, huisnummertoevoeging, 
                                                       postcode, woonplaatsnaam),
               datum_adres = as.Date(datum_adres, format = "%Y%m%d"))
      
      
    },
    
    
    get_geboortes_sinds = function(datum, ouder_anr){
      
      self$read_table("bzsprsq00", lazy = TRUE) %>%
        select(bsn = prsburgerservicenummer, 
               naam = prsgeslachtsnaam,
               voornamen = prsvoornamen,
               prsgeboortedatum,
               ou1anummer,
               ou2anummer) %>%
        filter(prsgeboortedatum != "0" & 
               !prsgeboortedatum %like% "%00" & 
               prsgeboortedatum != "" & 
               as.Date(prsgeboortedatum) >= datum,
               ou1anummer %in% !!ouder_anr | ou2anummer %in% !!ouder_anr) %>%
        collect %>%
        mutate(prsgeboortedatum = as.Date(prsgeboortedatum, format = "%Y%m%d")) %>%
        arrange(desc(prsgeboortedatum))
      
    },
    
    
    get_huwelijken_sinds = function(datum, bsn){
      
      self$read_table("bzshuwq00", lazy = TRUE) %>%
        filter(prsburgerservicenummer %in% !!bsn) %>%
        collect %>%
        mutate(huwdatumsluitinghuwelijkpartnerschap = as.Date(ymd(huwdatumsluitinghuwelijkpartnerschap)),
               huwdatumontbindinghuwelijkpartnerschap =  as.Date(ymd(huwdatumontbindinghuwelijkpartnerschap)))
      
    },
    
    
    get_scheidingen_sinds = function(datum, bsn){
      
      self$read_table("bzshuwq00", lazy = TRUE) %>%
        filter(prsburgerservicenummer %in% !!bsn,
               huwdatumontbindinghuwelijkpartnerschap != "" & 
                 as.Date(huwdatumontbindinghuwelijkpartnerschap) > datum) %>%
        collect %>%
        mutate(huwdatumsluitinghuwelijkpartnerschap = as.Date(ymd(huwdatumsluitinghuwelijkpartnerschap)),
               huwdatumontbindinghuwelijkpartnerschap =  as.Date(ymd(huwdatumontbindinghuwelijkpartnerschap)))
      
    },
    
    
    get_overlijdens_sinds = function(datum, bsn){
      
      self$read_table("bzsprsq00", lazy = TRUE) %>%
        select(bsn = prsburgerservicenummer, 
               naam = prsgeslachtsnaam,
               voornamen = prsvoornamen,
               prsgeboortedatum,
               ovldatumoverlijden) %>%
        filter(bsn %in% !!bsn, 
               ovldatumoverlijden != "" & 
               as.Date(ovldatumoverlijden) >= datum) %>%
        collect %>%
        mutate(ovldatumoverlijden = as.Date(ovldatumoverlijden)) %>%
        arrange(desc(ovldatumoverlijden))
      
    },
    

    
    
    #------ Depseudonimiseren -----
  
    #' @description Only perform the lookup method. Returns a reactive.
    rest_lookup = function(pseudo_ids){
      
      callModule(restCallModule, 
                 uuid::UUIDgenerate(), 
                 pseudo_ids = pseudo_ids, what = "lookup")
      
    },
    
    #' @description Depseudonimiseer een hele tabel
    #' @details Gepseudonimiseerde kolommen worden automatisch bepaald.
    table_depseudo2 = function(table_data = reactive(NULL), columns = NULL){
      
      pseudo_columns <- reactive({
        cols <- columns
        
        if(is.null(cols)){
          cols <- self$find_pseudo_columns(table_data())
        }
        
        cols
      })
      
      ids <- reactive({
        
        dat <- table_data()
        if(is.null(dat))return(NULL)
        
        columns <- pseudo_columns()
        u <- unique(unlist(dat[,columns]))
        u[!is.na(u) & u != ""]
      })
      
      f_out <- callModule(restCallModule, 
                          uuid::UUIDgenerate(), 
                          pseudo_ids = ids, 
                          what = "depseudo",
                          parse_result = TRUE)
      
      out <- reactive({
        req(f_out())
        dat <- table_data()
        
        req(nrow(f_out()) > 0)
        
        columns <- pseudo_columns()
        vals <- f_out()
        
        # store pseudo_bsn separately unencrypted
        if("pseudo_bsn" %in% columns){
          dat$bsn <- dat$pseudo_bsn
          columns[columns == "pseudo_bsn"] <- "bsn"
        }
        
        # store address columns separately unencrypted
        if(all(c("postcode","huisnummer","huisletter","huisnummertoevoeging") %in% names(dat))){
          dat$pseudo_postcode <- dat$postcode
          dat$pseudo_huisnummer <- dat$huisnummer
          dat$pseudo_huisletter <- dat$huisletter
          dat$pseudo_huisnummertoevoeging <- dat$huisnummertoevoeging
        }
        
        dat[columns] <- lapply(dat[columns], function(col){
          
          ii <- match(col, vals$pseudo_value)
          if(!all(is.na(ii))){
            vals$value[ii]  
          } else {  # als geen matches: vervang niets. In geval dat de kolom wel 9char is maar niet gepseudonimiseerd.
            col
          }
          
        })
        
        dat
      })
      
      
      out
      
    },
    

    find_pseudo_columns = function(data, length_pseudo = 9){
      
      data <- data[sapply(data, class) == "character"]
      
      nc <- lapply(data, function(x)setdiff(unique(nchar(x[!is.na(x)])),0))
      i_col <- which(sapply(nc, function(x)length(x) == 1  & x[1] == length_pseudo))
      names(data)[i_col]
      
    },
    
    
    make_adres_display = function(straatnaam, huisnummer, huisletter, huisnummertoevoeging, postcode, woonplaatsnaam){
      
      ifelse(is.na(straatnaam), "", 
             paste0(straatnaam,
                    " ",
                    huisnummer,
                    ifelse(is.na(huisletter),"",huisletter),
                    " ",
                    ifelse(is.na(huisnummertoevoeging) | huisnummertoevoeging == "","", paste0(" ", huisnummertoevoeging, " ")),
                    postcode, " ",
                    woonplaatsnaam))
      
    },
    
    
    get_family_depseudo = function(id_in = reactive(NULL)){
      
      flog.info("get_family_depseudo")
      
      fam <- reactive({
        req(id_in())
        self$get_family(id_in(), what = "bsn")
      })
      
      fam_d <- self$table_depseudo2(table_data = fam)
        

      reactive({
        
        req(fam_d())
        
        fam_d() %>%
          self$add_naam_column(fill_na = "Onbekend") %>%
          mutate(
            adres_display = self$make_adres_display(straatnaam, huisnummer, huisletter, huisnummertoevoeging, postcode, woonplaatsnaam),
            
            vwsdatuminschrijving = as.Date(vwsdatuminschrijving, "%y%m%d"),

            ouder1_naam = paste(ou1voornamen, ou1geslachtsnaam),
            ouder2_naam = paste(ou2voornamen, ou2geslachtsnaam),
            
            naam_tooltip = format_naam_tooltip(naam, overleden),
            
            adres_tooltip = format_adres_tooltip(
              vwsdatuminschrijving,
              vwsgemeentevaninschrijvingomschrijving,
              straatnaam,
              huisnummer,
              huisletter,
              huisnummertoevoeging,
              postcode,
              woonplaatsnaam
            )
          ) %>%
          mutate(adres_tooltip = na_if(adres_tooltip, "NA NA NA NA"),
                 adres_tooltip = na_if(adres_tooltip, "NA NA NA"),
                 adres_tooltip = na_if(adres_tooltip, "NA NANA NA"),
                 bsn = replace_na(bsn, ""),
                 voornamen = replace_na(voornamen, ""),
                 geslacht = replace_na(geslacht, "Onbekend"),
                 geboorteland = replace_na(geboorteland, "Onbekend"),
                 
                 adres_display = replace_na(adres_display, ""),
                 adres_tooltip = replace_na(adres_tooltip, "")
          )
        
      })
    
    },
    
    save_user_search_history = function(pseudo_bsn, userid){
      
      tab <- data.frame(
        pseudo_bsn = pseudo_bsn,
        userid = userid,
        timestamp = format(Sys.time())
      )
      
      self$append_data("search_history", tab)
      
    },
    
    get_user_search_history_today = function(userid){
      
      self$read_table("search_history", lazy = TRUE) %>%
        filter(userid == !!userid,
               as.Date(timestamp) == !!Sys.Date()) %>%
        collect
      
    },
    
    
    #' @description Most recent entry in `pseudolog` for each file
    get_pseudolog_files = function(){
      
      self$read_table("pseudolog", lazy = TRUE) %>% 
        group_by(file) %>% 
        filter(date == max(date, na.rm=TRUE)) %>% 
        collect
      
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

