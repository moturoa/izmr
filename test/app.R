


library(izmr) 

# Shiny packages

source("test/load_dependencies.R")


source("test/casusModule.R")
source("test/casusOverzichtModule.R")
source("test/casusBronModule.R")  
source("test/casusTijdlijn.R")  
source("test/functionsDatatables.R")

source("R/pinnedPerson.R")


#---- Config ----
# !! Belangrijk !!
options(
  izm_rest_url = "http://127.0.0.1",  # of on-premise "https://izm2-rest.ad.ede.nl" 
  izm_search_timeout = 1000,          # throttle op de search API
  pm_decrypt_secret = yaml::read_yaml("test/secret.yml")$secret  # voor pseudomaker decrypt.
  )

# Path (relative mag ook) naar SQLite met pseudo-data.
# Komt uiteindelijk op postgres.
.pdb <- izmr::pseudoData$new(
  filename = "c:/repos/ede/izm_frontend/data/ede_izm_postgres_copy.sqlite"
  #filename = "C:/Users/MartijnHeijstek/Documents/izm_frontend/data/ede_izm_postgres_copy.sqlite"
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



