


voorbeeldModuleUI <- function(id){
  
  
  ns <- NS(id)
  
  tagList(
    verbatimTextOutput(ns("txt_out")),
    verbatimTextOutput(ns("txt_out2"))
  )
  
}



voorbeeldModule <- function(input, output, session, clicked_id = reactive(NULL)){
  
  id_in <- reactive({
    clicked_id()$clicked$id
  })
  
  # id maakt hier niet uit!
  p_out <- callModule(restCallModule, "depseudo", pseudo_ids = id_in)
  
  fam <- get_family_depseudo(id_in, .pdb)
  
  output$txt_out <- renderPrint({
    fam()
  })
  
  output
  
  
}
