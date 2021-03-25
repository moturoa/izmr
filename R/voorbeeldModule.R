


voorbeeldModuleUI <- function(id){
  
  
  ns <- NS(id)
  
  
  verbatimTextOutput(ns("txt_out"))
  
  
}



voorbeeldModule <- function(input, output, session, clicked_id = reactive(NULL)){
  
  id_in <- reactive({
    clicked_id()$clicked$id
  })
  
  p_out <- callModule(depseudoModule, "depseudo", pseudo_ids = id_in)
  
    
  output$txt_out <- renderPrint({
    p_out()
  })
  
  
}
