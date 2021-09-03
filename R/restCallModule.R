#' Wrapper around imzsearch API
#' @description A shiny module function (server only) that returns a reactive with parsed results
#' from either the 'depseudo' or the 'lookup' endpoint. For the 'dev' endpoint, use \code{\link{restAddressCallModule}}.
#' @param input Shiny module, don't use.
#' @param output Shiny module, don't use.
#' @param session Shiny module, don't use.
#' @param what Either 'depseudo', 'lookup', 'dev'
#' @param pseudo_ids A reactive vector with pseudo BSNs to send to API.
#' @rdname restCallModule
#' @export
restCallModule <- function(input, output, session, 
                           what = c("depseudo","lookup", "dev"),  # endpoint
                           pseudo_ids = reactive(NULL),
                           parse_result = TRUE){
  
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
    
    if(parse_result){
      if(what == "depseudo"){
        parse_depseudo_result(input$depseudo_result)  
      } else if (what == "lookup"){
        parse_lookup_result(input$depseudo_result)
      }  
    } else {
      return(input$depseudo_result)
    }
    
    
  })
    
}

#' @rdname restCallModule
#' @export
restAddressCallModule <- function(input, output, session,
                                  adres = reactive(NULL)){
  
  url <- get_search_path("dev")
  
  observe({
    
    data <- adres()
    
    session$sendCustomMessage("getRecordFromAdress",
                              list(rest_url = url, 
                                   postcode = data$postcode,
                                   huisnummer = data$huisnummer,
                                   huisletter = data$huisletter,
                                   id = session$ns('depseudo_result')))
    
  })
  
  reactive({
    req(input$depseudo_result)
    parse_adres_result(input$depseudo_result)
    
  })
  
  
}




