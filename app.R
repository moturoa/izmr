


library(shiny)
library(jsonlite)
library(glue)
library(curl)
library(dplyr)
library(DT)

source("R/izmSearchModule.R")
source("R/voorbeeldModule.R")
source("R/depseudoModule.R")
source("R/pm_decrypt.R")
source("R/parse_depseudo_result.R")


# "https://izm2-rest.ad.ede.nl/dev" 

options(
  izm_rest_url = "http://127.0.0.1",
  izm_search_timeout = 1000,
  pm_decrypt_secret = yaml::read_yaml("test/secret.yml")$secret
)



ui <- fluidPage(
  
  # komt in package
  includeScript("inst/assets/izmsearch/izmsearch.js"),
  includeScript("inst/assets/restcalls/getRecordFromId.js"),
  
  # werkt alleen met id = 'izm' (vanwege module)
  izmSearchUI("izm"),
  
  tags$hr(),
  
  # output clicked id
  verbatimTextOutput("out1"),
  tags$hr(),
  tags$h4("Voorbeeld module"),
  voorbeeldModuleUI("voorbeeld")
  
)

server <- function(input, output, session) {
  
  out <- callModule(izmSearchModule, "izm")
  
  output$out1 <- renderPrint({
    out()
  })
  
  
  callModule(voorbeeldModule, "voorbeeld", clicked_id = out)
  
}

shinyApp(ui, server)
