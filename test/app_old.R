


library(izmr) 

# Shiny packages

# Shiny packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(dqshiny)
library(shinyWidgets)
#library(shinyalert)
library(shinycssloaders)
#library(sortable)
library(htmltools)
library(shinytoastr)
library(shinyjqui)
library(shinybusy)
#library(whereami)
library(leaflet)  
library(safer)

#library(shinyfilterset)
#library(shintodashboard)
library(shintoanalytics)
library(shintobag)
#library(shinypasswordinput)
#library(shinyinbox)

# Logging / dev / code
library(futile.logger)
library(yaml)
#library(devtools)
library(config)
library(gert)
library(R6)
library(rlang)
library(uuid)
library(jsonlite)

# HTML tables
library(knitr)
library(kableExtra)
library(izmr)

# HTML Widgets
#library(rintrojs)
library(DT)
library(plotly)
library(scales)


# Data related 
library(tidyverse)
library(lubridate)
library(stringr)
library(glue)
library(textclean)
library(writexl)
library(rvest) 
library(data.table)

# Database related
library(DBI) 
library(RSQLite)
library(pool)
library(dbplyr)
#library(arrow)
#library(openxlsx)
library(forcats)
source("test/casusModule.R")
source("test/casusOverzichtModule.R")
source("test/casusBronModule.R")  
source("test/casusTijdlijn.R")  
source("test/functionsDatatables.R")


#---- Config ----
# !! Belangrijk !!
options(
  izm_rest_url = "http://127.0.0.1",  # of on-premise "https://izm2-rest.ad.ede.nl" 
  izm_search_timeout = 1000,          # throttle op de search API
  pm_decrypt_secret = yaml::read_yaml("test/secret.yml")$secret  # voor pseudomaker decrypt.
)

# Path (relative mag ook) naar SQLite met pseudo-data.
# Komt uiteindelijk op postgres.
.pdb <<- izmr::pseudoData$new(
  filename = "C:/Users/MartijnHeijstek/Documents/izm_frontend/data/ede_izm_postgres_copy.sqlite"
)

 
# voorbeeld applicatie waar IZM-search een deel van uitmaakt.
ui <- fluidPage(
  
  izmr::izmr_dependencies(),
   
  
  tabsetPanel(id = "main",
              tabPanel("Search", value = "search",
                       
                       # werkt alleen met id = 'izm' (vanwege namespacing die we lastig in JS kunnen zetten)
                       izmr::izmSearchUI("izm")
                       
              ),
              tabPanel("Casus", value = "casus",
                       
                       tags$h4("Voorbeeld module"),
                       casusModuleUI("casus_izm")
                       
              )
              
  )
  
  
)


server <- function(input, output, session) {
  
  # De module vult de datatable in de izmSearchUI, en
  # returns een list met 'clicked' (id, nonce), 'nresults'.
  izm_search <- callModule(izmr::izmSearchModule, "izm")
  
  clicked_id <- reactive(
    izm_search()$clicked$id
  )
  
  observeEvent(izm_search()$clicked$nonce, {
    updateTabsetPanel(session, "main", selected = "casus")
  })
  
  # De clicked_id doorsturen naar andere modules in je applicatie, 
  # als een reactive.
  callModule(casusModule, "casus_izm", clicked_id = clicked_id)
  
}

shinyApp(ui, server)



