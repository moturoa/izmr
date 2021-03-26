#' Wrapper around imzsearch API
#' @description A shiny module function (server only) that returns a reactive with parsed results
#' from either the 'depseudo' or the 'lookup' endpoint.
#' @param input Shiny module, don't use.
#' @param output Shiny module, don't use.
#' @param session Shiny module, don't use.
#' @param what Either 'depseudo' or 'lookup'.
#' @param pseudo_ids A reactive vector with pseudo BSNs to send to API.
#' @export
restCallModule <- function(input, output, session, 
                           what = c("depseudo","lookup"),  # endpoint
                           pseudo_ids = reactive(NULL)){
  
  what <- match.arg(what)
  
  url <- get_search_path(what)
  
  observe({
    
    ids <- pseudo_ids()
    
    session$sendCustomMessage("getRecordFromId",
                            list(rest_url = url, 
                                 pseudo_ids = ids, 
                                 id = session$ns('depseudo_result')))
    
  })
  
  reactive({
    
    if(what == "depseudo"){
      parse_depseudo_result(input$depseudo_result)  
    } else if (what == "lookup"){
      parse_lookup_result(input$depseudo_result)
    }
    
  })
    
}





