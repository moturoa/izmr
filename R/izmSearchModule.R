#' De IZM als standalone shiny module
#' @description De Integrale ZoekMachine als module (UI, Server).
#' @param id **Moet zijn** : "izm", anders werkt de onderliggende JS code niet.
#' @param input Shiny input, dont use.
#' @param output Shiny input, dont use.
#' @param session Shiny input, dont use.
#' @param reset_button_class CSS Class to apply to the reset button (default: "btn-secondary")
#' @param reset_button_icon Icon name to apply to the reset button (default: "refresh")
#' @export
#' @rdname izmSearchModule
izmSearchUI <- function(id, 
                        reset_button_class = "btn-secondary", 
                        reset_button_icon = "refresh",
                        design = c("shinydashboard","softui")
                        ){
  
  design <- match.arg(design)
  ns <- NS(id)
  
  url <- get_search_path("dev")
  timeout <- getOption("izm_search_timeout", 1000)
  
  pagefun <- if(design == "shinydashboard")shiny::fluidPage else softui::fluid_page
  
  reset_button <- if(design == "shinydashboard"){
    shiny::actionButton(ns("btn_reset"), 
                        "Reset", 
                        icon = shiny::icon(reset_button_icon),
                        class = reset_button_class,
                        onclick = "resetform();")
  } else if(design == "softui"){
    softui::action_button(ns("btn_reset"), 
                        "Reset", 
                        status = "secondary",
                        icon = softui::bsicon("arrow-clockwise"),
                        onclick = "resetform();")
  }
  
  
  pagefun(
    
    tags$head(
      #tags$script(src = "https://cdn.datatables.net/1.10.20/js/jquery.dataTables.min.js"),
      #tags$script(src = "https://cdn.datatables.net/1.13.1/js/jquery.dataTables.min.js"),
      tags$script(src = "https://cdn.datatables.net/1.12.1/js/jquery.dataTables.min.js"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.29.2/moment.min.js")  
      
    ),
    
    includeCSS("https://cdn.datatables.net/1.12.1/css/dataTables.bootstrap.min.css"),
    includeCSS("https://cdn.datatables.net/1.12.1/css/jquery.dataTables.min.css"),
    #includeCSS("https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),

    # CSS for BS5; gives clash with softui :(    
    # includeCSS("https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/5.1.3/css/bootstrap.min.css"),
    # includeCSS("https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/5.1.3/css/bootstrap.min.css"),
    
    
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
                       izm_search_input("geboortedatum", "Geboortedatum", url, timeout, placeholder = 'dd-mm-jjjj/ddmmjjjj'),
                       izm_search_input("postcode", "Postcode", url, timeout, placeholder = '1234AA')
                )
              ) 
    ),
    
    tags$br(),
    
    tags$div(style = "width: 100%; padding-bottom: 40px;",
      
          tags$div(style = "float: left;",   
                   reset_button
          ),
          tags$div(style = "float: right;",
                
              helpMenuButton(ns("help_izmsearch"))
                   
          )
    ),
      
    
    tags$br(),
    
    tags$div(id = "searchresults_ui_wrapper",
      tags$table(id = "searchresults", class = "display")
    )
    
  )
  
}




#' @export
#' @rdname izmSearchModule
izmSearchModule <- function(input, output, session){
  
 
  callModule(helpMenu, "help_izmsearch", 
             
             tags$h4("Integrale Zoek Machine"),
             tags$p("Doorzoek het BRP voor de gemeente Ede op basis van naam, BSN nummer, geboortedatum, of adres informatie."),
             tags$p(HTML("Gebruik wildcards:")),
             tags$p(HTML("<b>*</b> voor een enkele positie, of <b>***</b> voor 1 of meerdere posities"))
             )
  
  
  reactive( 
    list(
      clicked = input$izmclickedid
      #n_results = input$izmnresults  
    )
    
  )  
  
  
}


#----- Utils ------
# not exported.
izm_search_input <- function(what, label, url, timeout, placeholder = ''){
  
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
      placeholder = placeholder,
      value = "")
  )
  
}



