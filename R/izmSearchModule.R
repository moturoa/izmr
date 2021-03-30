#' De IZM als standalone shiny module
#' @description De Integrale ZoekMachine als module (UI, Server).
#' @param id **Moet zijn** : "izm", anders werkt de onderliggende JS code niet.
#' @param input Shiny input, dont use.
#' @param output Shiny input, dont use.
#' @param session Shiny input, dont use.
#' @export
#' @rdname izmSearchModule
izmSearchUI <- function(id){
  
  ns <- NS(id)

  url <- get_search_path("dev")
  timeout <- getOption("izm_search_timeout", 1000)
  
  
  fluidPage(
    
    tags$head(
      tags$script(src="https://cdn.datatables.net/1.10.20/js/jquery.dataTables.min.js")
    ),
    
    includeCSS("https://cdn.datatables.net/1.10.20/css/dataTables.bootstrap.min.css"),
    includeCSS("https://cdn.datatables.net/1.10.20/css/jquery.dataTables.min.css"),
    
    tags$form(name = "izmsearch",
              fluidRow(
                
                shinydashboard::box(width = 10,
                      splitLayout(cellWidths="25%",
                                  izm_search_input("achternaam", "Achternaam", url, timeout),
                                  izm_search_input("geboortedatum", "Geboortedatum", url, timeout),
                                  izm_search_input("bsn", "BSN", url, timeout),
                                  
                                  actionButton(ns("btn_reset"), 
                                               "Reset", 
                                               onclick = "resetform();"),
                                  htmlOutput(ns("zoeken_count"))
                      ),
                      splitLayout(cellWidths="25%",
                                  izm_search_input("straatnaam", "Straatnaam", url, timeout), 
                                  izm_search_input("huisnummer", "Huisnummer", url, timeout), 
                                  izm_search_input("postcode", "Postcode", url, timeout),
                                  box(icon("info-circle"), HTML("Wildcards: <b>_</b> voor een enkele positie,<br/> <b>%</b> voor 1 of meerdere posities"), width = 12)
                      ), 
                      hr(),
                      DTOutput(ns("zoeken_resultaten"))                               
                ) 
              )  
              
               
    ),
    
    
    actionButton(ns("btn_reset"), 
                 "Reset", 
                 onclick = "resetform();"),
    

    tags$section(
      tags$table(id = "searchresults", class = "display",
                 
                 tags$thead(
                   tags$tr(
                     # Als REST API geen 7 kolommen terugpast, hier instellen!
                     # TODO column names
                     tags$th(""),
                     tags$th(""),
                     tags$th(""),
                     tags$th(""),
                     tags$th(""),
                     tags$th(""),
                     tags$th("")
                   )
                 )
                 
      )
    ) 
  )
  
}



#' @export
#' @rdname izmSearchModule
izmSearchModule <- function(input, output, session){
  
  reactive(
    list(
      clicked = input$izmclickedid,
      n_results = input$izmnresults  
    )
    
  )  
  
  
}


#----- Utils ------
# not exported.
izm_search_input <- function(what, label, url, timeout){
  
  tagList(        
    tags$label(class="control-label", 
               `for` = what, 
               label
    ),
    tags$input(
      id = what,
      onkeyup = glue("autosearch('{url}', {timeout});"),
      type = "text",
      class = "form-control izmsearch",
      names = what,
      value = "")
  )
  
}



