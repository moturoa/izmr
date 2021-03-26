

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
    get_person_brp = function(pseudo_id = NULL, what = c("bsn", "anr", "adres"), adres = NULL){
      
      # Is the pseudo_id a BSN or an ANR?
      what <- match.arg(what)
      
      if(what == "adres"){
        stopifnot(is.list(adres))
        stopifnot(all.equal(names(adres),
                            c("postcode","huisnummer","huisletter","huisnummertoevoeging")))
      }
      
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
                  "vblpostcode = '{adres$postcode}' and ",
                  "vblhuisnummer = '{adres$huisnummer}' and ",
                  "vblhuisletter = '{adres$huisletter}' and ",
                  "vblhuisnummertoevoeging = '{adres$huisnummertoevoeging}';")
        
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
      out <- self$replace_na_char(out)
      
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
    
    
    