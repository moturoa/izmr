
parse_depseudo_result <- function(res, format=FALSE) { 
  
  empty_result <- data.frame(value = character(),
                             pseudo_value = character())
  
  if(is.numeric(res) | is.null(res)){
    return(empty_result)
  }
  
  out <- tryCatch({
    
      as.data.frame(t(matrix(res, ncol = length(res)/3))) %>%
        setNames(c("key", "value", "pseudo_value")) %>%
        mutate(value = pm_decrypt(value))
      
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

