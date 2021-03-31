
casusOverzichtUI <- function(id){ 
  
  ns <- NS(id)  
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
               shinydashboard::box(  collapsible = TRUE, title ='Familie', width = 10,style=' padding: 10px; margin:10px',
                                     DTOutput(ns("dt_family"))     
               ))                      
    ),
    tags$div(id = 'placeholder')   
  )
}

casusOverzichtModule <- function(input, output, session, family){
  

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
                       bold_table_row("Adres",   HTML(paste(dat$straatnaam, dat$huisnummer, dat$huisletter, dat$postcode))),
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
    familyTable(family, session=session)  
  })
  
  
  
  # ----------  Dynamisch personen uitklappen (PINNEN!) ----------------- 
  inserted <- c()
  observeEvent(input$expandPseudoBsn, ignoreInit = FALSE, { 
 
    expand_fam <- .pdb$get_family_depseudo(reactive(input$expandPseudoBsn))
    expand_bron <- .pdb$get_all_bronnen(reactive(input$expandPseudoBsn))
   
    dat <- head(expand_fam(), 1) 
    dat2 <- expand_fam() %>% filter(relation != 'person_poi')
    
    if (dat$naam != 'NA' & !is.na(dat$bsn) ){
      print(dat)
      btn <- input$personDetails
      
      id <- paste0('txt', btn)
      id2 <- paste0('txt2', btn)
      insertUI(
        selector = '#placeholder',
        ## wrap element in a div with id for ease of removal
        ui = tags$div(id = id,
                      tags$hr(), 
                      span(
                        fluidRow(box(width=6, solidHeader=TRUE,
                                     tags$table(
                                       bold_table_row("Naam", HTML(dat$naam)),
                                       bold_table_row("BSN", HTML(dat$bsn)),
                                       bold_table_row("Adres",  HTML(paste(dat$straatnaam, dat$huisnummer, dat$huisletter, dat$postcode))),
                                       bold_table_row("Geboortedatum",  dat$geboortedatum_formatted), 
                                       
                                     )),
                                 box(width=6, solidHeader=TRUE,
                                    
                                     actionButton(session$ns("verzameling_addthis"), "Naar verzameling"))
                        ) 
                      ),
                      output[[id]] <- renderDataTable(dat2 %>% select(naam, relation)), 
                      output[[id2]] <- renderDataTable(expand_bron() %>% select(bron, begindatum, omschrijving)),           
        )
      ) 
      inserted <<- c(id, inserted)
    }
  })
  
  
  
  
 
}

 