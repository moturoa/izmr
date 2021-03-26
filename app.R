


library(shiny)
library(jsonlite)
library(glue)
library(curl)
library(dplyr)
library(DT)

library(futile.logger)
library(DBI)
library(pool)


source("R/izmSearchModule.R")
source("R/voorbeeldModule.R")
source("R/restCallModule.R")
source("R/pm_decrypt.R")
source("R/parse_depseudo_result.R")
source("R/utils.R")
source("R/get_family_depseudo.R")

source("R/pseudoData.R")


# "https://izm2-rest.ad.ede.nl" 

options(
  izm_rest_url = "http://127.0.0.1",
  izm_search_timeout = 1000,
  pm_decrypt_secret = yaml::read_yaml("test/secret.yml")$secret
)

.pdb <- pseudoData$new(
  filename = "c:/repos/ede/izm_frontend/data/ede_izm_postgres_copy.sqlite"
)



ui <- fluidPage(
  
  # komt in package
  includeScript("inst/assets/izmsearch/izmsearch.js"),
  includeScript("inst/assets/restcalls/getRecordFromId.js"),
  
  #izmr::izmr_dependencies(),
  
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
