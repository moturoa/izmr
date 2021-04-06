


moduleUI <- function(id){
  
  ns <- NS(id)
  
  verbatimTextOutput(ns("txt_out"))
  
  
}


moduleServer <- function(input, output, session){
  
  fam <- .pdb$get_family_depseudo(reactive("RKovBl5hW"))
  
  output$txt_out <- renderPrint({
    
    fam()
    
  })
  
}

library(shiny)
library(izmr)

options(
  izm_rest_url = "http://127.0.0.1",  # of on-premise "https://izm2-rest.ad.ede.nl" 
  izm_search_timeout = 1000,          # throttle op de search API
  pm_decrypt_secret = yaml::read_yaml("test/secret.yml")$secret  # voor pseudomaker decrypt.
)

.pdb <- pseudoData$new(
  filename = "c:/repos/ede/izm_frontend/data/ede_izm_postgres_copy.sqlite"
)


ui <- fluidPage(
  izmr::izmr_dependencies(),
  
  actionButton("btn1", "Go!"),
  
  moduleUI("tester"),
  
  tags$div(id = "placeholder")
)


server <- function(input, output, session) {
  
  observeEvent(input$btn1, {
    
    insertUI("#placeholder", where = "afterBegin",
             ui = moduleUI("one"))
    
    callModule(moduleServer, "one")
    
  })
  
  callModule(moduleServer, "tester")
  
  
}

shinyApp(ui, server)



