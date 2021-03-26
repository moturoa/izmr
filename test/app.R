


library(izmr)


#---- Config ----
# !! Belangrijk !!
options(
  izm_rest_url = "http://127.0.0.1",  # of on-premise "https://izm2-rest.ad.ede.nl" 
  izm_search_timeout = 1000,          # throttle op de search API
  pm_decrypt_secret = yaml::read_yaml("secret.yml")$secret  # voor pseudomaker decrypt.
)

# Path (relative mag ook) naar SQLite met pseudo-data.
# Komt uiteindelijk op postgres.
.pdb <- pseudoData$new(
  filename = "c:/repos/ede/izm_frontend/data/ede_izm_postgres_copy.sqlite"
)



# Module zoals een casusModule oid zou zijn.
voorbeeldModuleUI <- function(id){
  
  
  ns <- NS(id)
  
  tagList(
    tableOutput(ns("txt_out"))
  )
  
}

voorbeeldModule <- function(input, output, session, clicked_id = reactive(NULL)){
  
  
  fam <- get_family_depseudo(clicked_id, .pdb)
  
  output$txt_out <- renderTable({
    fam()
  })
  
}



# voorbeeld applicatie waar IZM-search een deel van uitmaakt.
ui <- fluidPage(
  
  izmr::izmr_dependencies(),
  
  
  tabsetPanel(id = "main",
    tabPanel("Search", value = "search",
             
             # werkt alleen met id = 'izm' (vanwege namespacing die we lastig in JS kunnen zetten)
             izmSearchUI("izm")
             
    ),
    tabPanel("Casus", value = "casus",
             
             tags$h4("Voorbeeld module"),
             voorbeeldModuleUI("voorbeeld")
             
    )
    
  )
  
  
)

server <- function(input, output, session) {
  
  # De module vult de datatable in de izmSearchUI, en
  # returns een list met 'clicked' (id, nonce), 'nresults'.
  izm_search <- callModule(izmSearchModule, "izm")
  
  clicked_id <- reactive(
    izm_search()$clicked$id
  )
  
  observeEvent(izm_search()$clicked$nonce, {
    updateTabsetPanel(session, "main", selected = "casus")
  })
  
  # De clicked_id doorsturen naar andere modules in je applicatie, 
  # als een reactive.
  callModule(voorbeeldModule, "voorbeeld", clicked_id = clicked_id)
  
}

shinyApp(ui, server)



