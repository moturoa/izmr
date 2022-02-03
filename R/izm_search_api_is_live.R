#' test
#' @export
izm_search_api_is_live <- function(){
  
  url <- get_search_path("test")
  r <- try(httr::GET(url), silent = TRUE)
  
  if(inherits(r, "try-error")){
    return(FALSE)
  }
  isTRUE(r$status_code == 200)
}
