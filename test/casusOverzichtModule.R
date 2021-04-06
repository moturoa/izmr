
casusOverzichtUI <- function(id){ 
  
  ns <- NS(id)  
  
  tagList(
    fluidRow(
      tags$script(HTML('Shiny.addCustomMessageHandler("changetitle", function(x) {document.title=x});')),
      # Persoon details
      column(width=12,
             withSpinner(
               htmlOutput(ns("persoon_details"), width=10)
             )
      ),
      
      column(width=12, style='margin:10px',  
             
             div(id = ns('box_family'),
                 shinydashboard::box(collapsible = TRUE, 
                                     title = "Familie", 
                                     width = 10,
                                     style = "padding: 10px; margin:10px",
                                     DTOutput(ns("dt_family"))     
                 ))
      )
    ),
    tags$div(id = "pin_person_placeholder",
             style = "width: 80%; padding-left: 50px;")
  )
  
}


casusOverzichtModule <- function(input, output, session, family){
  

  print("called casusOverzichtModule")
  
  # get all details from POI
  output$persoon_details <- renderUI({
    
    if(!is.null(family())){
      print("Rendering persoon details")  
      
      # POI is altijd de eerste regel!
      dat <- head(family(),1)   
       
      # alter tab title to name!
      title <- if(!is.na(dat$naam)) {
        dat$naam
      } else {
        "No title yet."
      }
      session$sendCustomMessage("changetitle", title)
      
      span(
        fluidRow(box(width=6, solidHeader=TRUE,
                     tags$table(
                       bold_table_row("Naam", HTML(dat$naam)),
                       bold_table_row("BSN", HTML(dat$bsn)),
                       bold_table_row("Adres",   HTML(dat$adres_display)),
                       bold_table_row("Geboortedatum",  dat$geboortedatum)
                       
                    )
                 ),
                 box(width=6, solidHeader=TRUE, 
                     actionButton(session$ns("verzameling_addthis"), "Naar verzameling"))
            )
        ) 
    }
    else {
      box(width=12, solidHeader=TRUE,
          tagList( 
            tags$h1("Data wordt opgehaald."),
            tags$h2(glue("Een moment geduld astublieft.") )
          ))
    }
     
  }) 
  
  
  # TODO : koppelen aan verzamelingen
  # observeEvent(input$verzameling_addthis,ignoreInit = TRUE, { 
  #  req(returnable$pseudo_bsn[1])
  #  js$setInput(pid='added_person', data= returnable$pseudo_bsn[1])  
  #})

  # ---------- Family box rendering -----------------
  observeEvent(family(), {    
    shinyjs::toggle("box_family", condition = nrow(family() %>% filter(relation !='persoon_poi') ) > 0)
  })
     
  output$dt_family <- DT::renderDT({   
    req(family()) 
    print("making dt_family")
    familyTable(family, session=session)  
    
  })
  
  
  
  
  
  # ----------  Dynamisch personen uitklappen (PINNEN!) ----------------- 
  
  
  observeEvent(input$expandPseudoBsn, ignoreNULL = TRUE, { 
   
    print(paste("PIN:",input$expandPseudoBsn))
    new_id <- uuid::UUIDgenerate()
  
    insertUI(selector = "#pin_person_placeholder", where = "afterBegin",
             ui = pinnedPersonUI(session$ns(new_id)))

    callModule(pinnedPerson, new_id, pseudo_bsn = isolate(input$expandPseudoBsn))
    
  })
  
  
  

  
 
}

 