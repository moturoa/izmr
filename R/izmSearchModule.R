



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



izmSearchUI <- function(id){
  
  ns <- NS(id)
  
  # Read Search URL from options.
  url <- getOption("izm_search_url")
  if(is.null(url)){
    stop("Set search URL with options(izm_search_url='http://...')")
  }
  timeout <- getOption("izm_search_timeout", 1000)
  
  
  fluidPage(
    
    tags$head(
      tags$script(src="https://cdn.datatables.net/1.10.20/js/jquery.dataTables.min.js"),
      #tags$script(src = "script.js", class = "init")
    ),
    
    includeCSS("https://cdn.datatables.net/1.10.20/css/dataTables.bootstrap.min.css"),
    includeCSS("https://cdn.datatables.net/1.10.20/css/jquery.dataTables.min.css"),
    
    tags$form(name = "izmsearch",
              fluidRow(
                column(4,
                       izm_search_input("achternaam", "Achternaam", url, timeout),  
                       izm_search_input("straatnaam", "Straatnaam", url, timeout)
                ),
                column(4,
                       izm_search_input("bsn", "BSN", url, timeout),
                       izm_search_input("huisnummer", "Huisnummer", url, timeout),
                ),
                column(4,
                       izm_search_input("geboortedatum", "Geboortedatum", url, timeout),
                       izm_search_input("postcode", "Postcode", url, timeout)
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




izmSearchModule <- function(input, output, session){
  
  reactive(
    list(
      clicked = input$izmclickedid,
      n_results = input$izmnresults  
    )
    
  )  
  
  
}




