
#if(T){

library(shiny)

library(izmr)

source("R/pinnedPerson.R")
source("test/functionsDatatables.R")
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
  tags$h2("test"),
  tags$hr(),
  actionButton("btn1", "Go!"),
  tags$div(id = "placeholder")
  
)

server <- function(input, output, session) {
  
  observeEvent(input$btn1, {
    
    insertUI(selector = "#placeholder", where = "afterBegin", 
             ui = pinnedPersonUI("tester"))
    
    callModule(pinnedPerson, "tester", pseudo_bsn = reactive("RKovBl5hW"))
  })
  
  
}

shinyApp(ui, server)


#}

