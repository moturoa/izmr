get_search_path <- function(endpoint){
  url <- getOption("izm_rest_url")
  
  if(is.null(url)){
    stop("set options(izm_rest_url = 'http://....') first!")
  }
  
  file.path(url, endpoint)
}

