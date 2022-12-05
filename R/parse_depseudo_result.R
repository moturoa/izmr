

parse_result <- function(res, column_names, func){
  
  n_columns <- length(column_names)
  empty_result <- empty_dataframe(column_names)
  
  if(is.numeric(res) | is.null(res)){
    return(empty_result)
  }
  
  if("status" %in% names(res) && res$status == 404){
    flog.info("Error in REST Call - 404 !")
    return(empty_result)
  }  
  out <- tryCatch({
    
    dfr <- as.data.frame(t(matrix(res, ncol = length(res)/n_columns))) %>%
      setNames(column_names)
    
    if("value" %in% names(dfr)){
      dfr <- dfr %>%
        mutate(value = pm_decrypt(value))  
    }
    
    dfr
  },
  error=function(cond) {  
    cat(file=stderr(), glue('Error in {func}: {paste(cond, sep = ",")}'), sep= '\n')  
    return(empty_result)
  },
  warning=function(cond) {
    cat(file=stderr(), glue('Warning in parseResDepseudo: {paste(cond, sep = ",")}'), sep= '\n')  
    return(empty_result)
  })
  
  return(out)
  
}



parse_depseudo_result <- function(res) { 
  
  # evt. aan te passen, te checken, of te configureren
  column_names <- c("key", "value", "pseudo_value")
  
  # Dit is tijdelijk - voor backwards comp.
  keys <-  c("CL_BSN","Adres","Achternaam","Telefoonnummer","E-mailadres",
             "POSTKOD","CL_A_NR","HUISNUMMER","HUISLETTER","HUISNR_TOEV",
             "ADRES","STRAATNAAM","NAAM","VOORVOEGSEL","GESLACHTSNAAM")
  
  # oude rest api geeft de key kolom niet terug
  if(!is.null(res) && !res[1] %in% keys){
    column_names <- c("value", "pseudo_value")
  } 

  parse_result(res, column_names, func="deps")
  
}


parse_lookup_result <- function(res) { 
  
  column_names <- c("pseudo_bsn","bsn", "naam", "voornamen", 
                    "geboortedatum", "straatnaam", "huisnummer","huisletter",
                    "huisnummertoevoeging","postcode")
  
  n <- length(column_names)
  
  if(length(res) %% n > 0){
    column_names <- column_names[-length(column_names)]
  }
  
  parse_result(res, column_names, func="lookup")
}



parse_adres_result <- function(res) { 
  
  column_names <- c("pseudo_bsn","bsn","naam","voornamen",
                    "geboortedatum","straatnaam","huisnummer",
                    "huisletter","huisnummertoevoeging", "postcode")
  
  parse_result(res, column_names, func="adres")
  
}

