

helpModal <- function(...){
  
  modalDialog(
    easyClose = TRUE,
    title = tagList("Help", tags$span(tags$img(src="logoshintolabs.png", width = 70), 
                                      style = "display:inline-block; float:right;")),
    
    ...,
    footer = actionButton("xyz", "Sluiten", 
                          icon = icon("remove"), 
                          class= "btn-danger",
                          `data-dismiss` = "modal",
                          `data-bs-dismiss` = "modal")  # BS >= 4
    
  )
  
}


helpMenuButton <- function(id){
  
  ns <- NS(id)
  
  actionButton(ns("btn_show_help"), 
               tagList(shiny::icon("question-circle"), "Help"),
               class = "btn-info")
  
}

helpMenu <- function(input, output, session, ...){
  
  observeEvent(input$btn_show_help, {
    showModal(
      helpModal(...)
    )
  })  
  
}








