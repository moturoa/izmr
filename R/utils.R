get_search_path <- function(endpoint){
  url <- getOption("izm_rest_url")
  
  if(is.null(url)){
    stop("set options(izm_rest_url = 'http://....') first!")
  }
  
  file.path(url, endpoint)
}

empty_dataframe = function(nms){
  
  data.frame(matrix(vector(), 0, length(nms),
                    dimnames=list(c(), nms)))
  
}



  