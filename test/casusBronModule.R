
casusBronnenUI <- function(id){ 
  
  ns <- NS(id)  
  fluidRow(
    shinydashboard::box( id=ns("box1"), title ='Filters', width = 12,style=' padding: 10px; margin:10px', 
                         solidHeader = TRUE,  
                         collapsible = TRUE, collapsed = TRUE,
                         background = 'light-blue',
                         dateRangeInput(ns('daterange_bron'), label='Datum', language = "nl")
    ),
    
    # Persoon details 
    tabBox(width=12,
           tabPanel(title = textOutput(ns("txt_suite")),
                    fluidRow(
                      DTOutput(ns("bron_suite"))
                    )
           ),
           tabPanel(title = textOutput(ns("txt_mens")),
                    fluidRow(
                      DTOutput(ns("bron_menscentraal"))
                    )
           ), 
           tabPanel(title = textOutput(ns("txt_allegro")),
                    fluidRow(
                      DTOutput(ns("bron_allegro"))
                    )
           ),
           tabPanel(title = textOutput(ns("txt_carel")),
                    fluidRow(
                      DTOutput(ns("bron_carel"))
                    )
           ),
           tabPanel(title = textOutput(ns("txt_wave")), 
                    fluidRow(
                      DTOutput(ns("bron_wave"))
                    )
           ),
           tabPanel(title = textOutput(ns("txt_brp")),
                    fluidRow(  
                      div(id=ns('box_huw'),
                          shinydashboard::box(  title ='Huwelijken', width = 10,style=' padding: 10px; margin:10px',  
                                                DTOutput(ns("bron_brp_huw"))
                          )),
                      div(id=ns('box_verh'),
                          shinydashboard::box(  title ='Verhuizingen', width = 10,style=' padding: 10px; margin:10px',  
                                                DTOutput(ns("bron_brp_verh"))
                          )),
                      div(id=ns('box_kind'),
                          shinydashboard::box(  title ='Kinderen', width = 10,style=' padding: 10px; margin:10px',  
                                                DTOutput(ns("bron_brp_kind"))
                          )
                      ),
                      div(id=ns('box_status'),
                          shinydashboard::box(  title ='Statuswijziging', width = 10,style=' padding: 10px; margin:10px',  
                                                DTOutput(ns("bron_brp_status"))
                          )
                      ),
                      div(id=ns('box_cura'),
                          shinydashboard::box(  title ='Curatele', width = 10,style=' padding: 10px; margin:10px',  
                                                DTOutput(ns("bron_brp_cura"))
                          )
                      ) 
                    ) 
           )
           
    )
    
  )
}



casusBronnenModule <- function(input, output, session, data){
  
  
  # UPDATE datrangeinput with minimum date!
  observeEvent(data, {   
    
    if(!is.null(data$begindatum)) {
      minDat <- min(structure(data$begindatum[!is.na(data$begindatum)], class= "Date"))
    } else {
      minDat <- NULL
    }
    #minDat<- ifelse(!is.null(all_begin), min(structure(all_begin[!is.na(all_begin)], class= "Date")), NULL)
    
    updateDateRangeInput(session, 'daterange_bron', start=minDat)  
  })
  
  
  # filter dates accoring to daterange
  filterDat <- function(dat){    
    if(nrow(dat) > 0){
      
      if ('einddatum' %in% colnames(dat)) {
        return (filter(dat, begindatum >=  ymd(input$daterange_bron[1]) & 
                         einddatum <=  ymd(input$daterange_bron[2]) | is.na(begindatum)| is.na(einddatum) ))
      } else {
        return (filter(dat, begindatum >= ymd(input$daterange_bron[1]) | is.na(begindatum)))
        
      }
    } else {
      return(dat)
    }
  }
  
  # filtering on daterangeinput
  observe({
    req(data)
    
    data <- filterDat(data)  
    
    
    # showing/ hiding boxes around BRP data 
    shinyjs::toggle("box_kind", condition = nrow(filter(data, bron == 'Kind')) > 0)  
    shinyjs::toggle("box_huw", condition = nrow(filter(data, bron == 'Huwelijk')) > 0)  
    shinyjs::toggle("box_verh", condition = nrow(filter(data, bron == 'Verhuizing')) > 0)  
    shinyjs::toggle("box_cura", condition = nrow(filter(data, bron == 'Curatele')) > 0)  
    shinyjs::toggle("box_status", condition = nrow(filter(data, bron == 'Overleden' | bron == 'Uitgeschreven')) > 0)  
    
  }) 
  
  
  # --------------- Rendering tab titles -------------------
  output$txt_suite <- renderText({ 
    glue("Suite ({nrow(filter(data, bron == 'Suite'))})")
  })
  output$txt_mens <- renderText({ 
    glue("Mens Centraal ({nrow(filter(data, bron == 'Mens Centraal'))})")
  })
  output$txt_allegro <- renderText({ 
    glue("Allegro ({nrow(filter(data, bron == 'Allegro'))})")
  })
  output$txt_carel <- renderText({ 
    glue("Carel ({nrow(filter(data, bron == 'Carel'))})")
  }) 
  output$txt_wave <- renderText({ 
    glue("Open Wave ({nrow(filter(data, bron == 'Open Wave'))})")
  })
  output$txt_brp <- renderText({ 
    glue("BRP ({nrow(filter(data, bron %in% c('Huwelijk','Kind','Verhuizing' ,'Uitgeschreven' ,'Curatele', 'Overleden')))})")
  })
  
  # --------------- Rendering tables -------------------
  output$bron_suite <- DT::renderDT({    
    req(data)
    datatableBron(data= filter(data, bron == 'Suite') , extra_selectie = 'einddatum_formatted')
  }) 
  
  output$bron_carel <- DT::renderDT({   
    req(data)
    datatableBron(data= filter(data, bron == 'Carel'), extra_selectie = 'einddatum_formatted')   
  }) 
  
  output$bron_wave <- DT::renderDT({   
    req(data)
    datatableBron(data= filter(data, bron == 'Open Wave'), extra_selectie = 'zaaksoort' )
  }) 
  
  output$bron_allegro <- DT::renderDT({   
    req(data)
    datatableBron(data= filter(data, bron == 'Allegro')  , extra_selectie ='naam_consulent')   
  }) 
  output$bron_menscentraal <- DT::renderDT({   
    req(data)
    datatableBron(data= filter(data, bron == 'Mens Centraal'), extra_selectie = c('status', 'groepnr'))
  }) 
  
  output$bron_brp_huw <- DT::renderDT({    
    req(data)
    datatableBron(data= filter(data, bron == 'Huwelijk'), extra_selectie = 'einddatum_formatted' )   
  }) 
  output$bron_brp_verh <- DT::renderDT({    
    
    datatableBron(data= filter(data, bron == 'Verhuizing'))   
  })
  
  output$bron_brp_status <- DT::renderDT({    
    
    datatableBron(data= filter(data, bron == 'Overleden' | bron == 'Uitgeschreven') %>% arrange(desc(begindatum)))   
  }) 
  output$bron_brp_kind <- DT::renderDT({    
    
    datatableBron(data= filter(data, bron == 'Kind') )   
  }) 
  output$bron_brp_cura <- DT::renderDT({    
    
    datatableBron(data= filter(data, bron == 'Curatele') )   
  }) 
  
  return(NULL)  
  
}

