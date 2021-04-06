

pinnedPersonUI <- function(id){
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("ui_person"))
  )
  
}


  
pinnedPerson <- function(input, output, session, pseudo_bsn = reactive(NULL)){
  
  
  person_fam_data <- .pdb$get_family_depseudo(pseudo_bsn)
  expand_bron <- .pdb$get_all_bronnen(pseudo_bsn)
  
  output$txt_out <- renderPrint({
    person_fam_data()
  })
  
  person_data <- reactive({
    person_fam_data() %>%
      filter(relation == "persoon_poi")
  })
  
  fam_data <- reactive({
    person_fam_data() %>%
      filter(relation != "persoon_poi")
  })
  
  person_bron <- reactive({
    .pdb$get_all_bronnen(pseudo_bsn)
  })
  
  
  output$ui_person <- renderUI({
    
    person <- person_data()
    fam <- isolate(fam_data())

    req(person)
    req(person$naam != "NA")
    req(!is.na(person$bsn))

    print(paste("RENDERING:", isolate(pseudo_bsn())))
    
    tags$div(style = "border: 1px solid black; padding: 30px; margin: 30px;",
      shiny::fluidRow(
        shinydashboard::box(width=6, solidHeader=TRUE,
                            tags$table(
                              bold_table_row("Naam", person$naam),
                              bold_table_row("BSN", person$bsn),
                              bold_table_row("Adres", person$adres_display),
                              bold_table_row("Geboortedatum",  person$geboortedatum_formatted)
                            )),
        shinydashboard::box(width=6, solidHeader=TRUE,
                            actionButton(session$ns("verzameling_addthis"), 
                                         "Naar verzameling",
                                         icon = icon("plus")))
  
      ),
      
      tags$h4("Ouders"),
      DT::dataTableOutput(session$ns("dt_ouders")),
      tags$h4("Kinderen"),
      DT::dataTableOutput(session$ns("dt_kinderen")),
      tags$h4("Huwelijken"),
      DT::dataTableOutput(session$ns("dt_huwelijken"))
    
    )
      
  })
  
  
  output$dt_ouders <- DT::renderDataTable({
    
    fam_data() %>%
      filter(relation %in% c("vader","moeder")) %>%
      select(bsn, naam, geboortedatum, geslacht, adres_display) %>%
      datatable(options = list(dom = "t"))
    
  })
  
  
  output$dt_kinderen <- DT::renderDataTable({
    
    fam_data() %>%
      filter(relation %in% c("zoon","dochter")) %>%
      select(bsn, naam, geboortedatum, geslacht, adres_display) %>%
      datatable(options = list(dom = "t"))
    
  })
  
  output$dt_huwelijken <- DT::renderDataTable({
    
    fam_data() %>%
      filter(relation %in% c("partner","ex partner")) %>%
      select(bsn, naam, geboortedatum, geslacht, adres_display, begindatum, einddatum) %>%
      datatable(options = list(dom = "t"))
    
  })
    
    
   

}


