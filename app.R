


library(shiny)
library(jsonlite)
library(glue)
library(curl)
library(dplyr)
library(DT)

source("R/izmSearchModule.R")

# "https://izm2-rest.ad.ede.nl/dev" 

options(
  izm_search_url = "http://127.0.0.1/dev",
  izm_search_timeout = 1000
)



ui <- fluidPage(
  
  # komt in package
  includeScript("inst/assets/izmsearch/izmsearch.js"),
  
  # werkt alleen met id = 'izm' (vanwege module)
  izmSearchUI("izm"),
  
  tags$hr(),
  
  # output clicked id
  verbatimTextOutput("out1")
  
)

server <- function(input, output, session) {
  
  out <- callModule(izmSearchModule, "izm")
  
  output$out1 <- renderPrint({
    out()
  })
  
}

shinyApp(ui, server)
