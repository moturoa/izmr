
server <- function(input, output, session) { 
  
  # De module vult de datatable in de izmSearchUI, en
  # returns een list met 'clicked' (id, nonce), 'nresults'.
  izm_search <- callModule(izmr::izmSearchModule, "izm")
  
  clicked_id <- reactive(
    izm_search()$clicked$id
  )
 
  observeEvent(izm_search()$clicked$nonce, {
    
    print(paste("ID Search:",clicked_id() ))
    
    updateTabsetPanel(session, "main", selected = "casus")
  })
  
  # De clicked_id doorsturen naar andere modules in je applicatie, 
  # als een reactive.
  callModule(casusModule, "casus_izm", clicked_id = clicked_id)
  
  
  output$txt_out <- renderPrint({
    out <- .pdb$get_family_depseudo(reactive(input$txt_test))
    out()
  })
  
  
} 
