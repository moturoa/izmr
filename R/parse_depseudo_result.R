

parse_result <- function(res, column_names){
  
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
    
    cat(file=stderr(), glue('Error in parseResDepseudo: {paste(cond, sep = ",")}'), sep= '\n')  
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
  
  parse_result(res, column_names)
  
}


parse_lookup_result <- function(res, format=FALSE) { 
  
  column_names <- c("pseudo_bsn","bsn", "naam", "geboortedatum",  
                    "straatnaam", "huisnummer","huisletter","postcode")
  
  parse_result(res, column_names)
}

