

depseudoModule <- function(input, output, session, pseudo_ids = reactive(NULL)){
  
  url <- getOption("izm_rest_url")
  if(is.null(url)){
    stop("set options(izm_rest_url = 'http://....') first!")
  }
  url <- file.path(url, "depseudo")
  
  observe({
    
    ids <- pseudo_ids()
    
    session$sendCustomMessage("getRecordFromId",
                            list(rest_url = url, 
                                 pseudo_ids = ids, 
                                 id = session$ns('depseudo_result')))
    
  })
  
  reactive(
    parse_depseudo_result(input$depseudo_result)
  )
    
}





