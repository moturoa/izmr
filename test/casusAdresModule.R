
casusAdresUI <- function(id){ 
  
  ns <- NS(id)
  
  fluidPage( 
    tabBox(width=12,
           tabPanel(title = "Overzicht",
                    
                    div(
                      fluidRow(column(4,
                                      # shinydashboard::box(  title = 'Zoek adres', width = 10,style=' padding: 10px; margin:10px',
                                      
                                      #                     bagSelectUI(ns(.id_bagselect), 
                                      #                                woonplaats_multiple = FALSE, 
                                      #                               reset_button = TRUE,
                                      #                              ui_straat = "selectInput") 
                                      
                                      #                ),
                                      shinydashboard::box(  title = textOutput(ns("txt_adr_info")), width = 10,style=' padding: 10px; margin:10px',
                                                            uiOutput(ns("ui_pand_info"))
                                      ),
                                      shinydashboard::box(  title = 'Externe bronnen', width = 10,style=' padding: 10px; margin:10px',
                                                            uiOutput(ns("ui_pand_externe_bronnen"))
                                      )
                      ),
                      column(8,
                             fluidRow(
                               column(12,
                                      div(style = "float:right; width:100%; height: 420px; border-style: solid;",
                                          
                                          leafletOutput(ns("leaflet_bagobject"), width = '100%', height = '100%')
                                      ) 
                               )
                               
                               
                             ) 
                      )
                      
                      )
                    )), 
           tabPanel(title = "Op adres",
                    fluidRow( 
                      shinydashboard::box(collapsible = TRUE,  title ='Bedrijven op adres', width = 10,style=' padding: 10px; margin:10px',
                                          DTOutput(ns("dt_kvk"))    
                      ),
                      
                      
                      shinydashboard::box(collapsible = TRUE,  title = textOutput(ns("txt_adr_pc")), width = 10,style=' padding: 10px; margin:10px',
                                          #materialSwitch(ns("toggle_kvk_only"), "Alleen voor adres",
                                          #               value = FALSE, right = TRUE, status = "primary"),
                                          DTOutput(ns("dt_openwave_pc"))    
                      ),
                      shinydashboard::box(collapsible = TRUE,  title ='Personen op adres', width = 12,style=' padding: 10px; margin:10px',
                                          DTOutput(ns("dt_people_at_adress"))    
                      )
                    )) 
           
           
    )
    
    
  )
}

casusAdresModule <- function(input, output, session, bagId, peopleData, kvkData){
  
  rv <- reactiveValues(
    peopleAtAdr=NULL,
    kvkAtAdr=NULL,
    bagData = NULL,
    kvk_filtered=NULL,
    openWaveData = NULL
  )
  
  
  # ----------------------- Bagselectie  -------------------------------- 
  observeEvent(bagId, {
    
    rv$bagData <-   st_as_sf( bagId )
    
    # lookin up OpenWave meldingen
    
    if(!is.null(rv$bagData$vblpostcode)){
      rv$openWaveData <- .pseudo_db$getOpenWavePostcode(rv$bagData$vblpostcode) 
    } 
    # Lookup peolple living at adress!
    if(is.null(peopleData) & nrow(rv$bagData) == 1) {
      
      # alter tab title to name!
      title <- if(!is.na(rv$bagData$bag_adres)) {
        rv$bagData$bag_adres
      } else {
        "No title yet."
      }
      session$sendCustomMessage("changetitle", title)
      
      #js$getRecordFromAdress(rest_url = rest_url_adr, huisnummer = sample(c('UuIaHomF6', 'f7uAiMlO4', 'Mi9XJDSKT') ,1), postcode='xiSUKsG9A', id = 'inhabitantsAdr') # FAKE
      js$getRecordFromAdress(rest_url = rest_url_adr, huisletter = rv$bagData$huisletter,huisnummer = rv$bagData$huisnummer, postcode= rv$bagData$postcode, id = 'inhabitantsAdr') #REAL    
    }
    
  }) 
  
  observeEvent(kvkData, ignoreNULL = FALSE,{ 
    
    if(is.null(kvkData) & nrow(bagId) == 1){
      rv$kvkAtAdr <- filter(.kvk_ede, postcode == bagId$postcode & huisnummer == as.integer(bagId$huisnummer))
      
    } else{
      
      rv$kvkAtAdr <- kvkData
    } 
  })
  
  
  # inhabitants are already determined
  observeEvent(peopleData,{  
    rv$peopleAtAdr <- peopleData # %>% filter()
  })
  
  # inhabitants from REST service 
  observeEvent(input$depsuedo_adres_list, {  
    rv$openWaveData <- replaceAdres(rv$openWaveData, parseResDepseudo(input$depsuedo_adres_list))
  })
  
  
  
  # inhabitants from REST service
  observeEvent(input$inhabitantsAdr, {   
    
    # add the avtieve verzamelingen!
    dat <- parseResAdr(input$inhabitantsAdr) %>% mutate(actieveVerz = .verz_db$getAllVerzamelingen(pseudo_bsn))
    rv$peopleAtAdr <- dat %>% mutate(lenVerz = lengths(actieveVerz)) 
  }) 
  
  output$dt_people_at_adress <- DT::renderDT({    
    req(rv$peopleAtAdr)
    datatableFam(data=rv$peopleAtAdr  %>% arrange(geboortedatum), selectie = c('lenVerz', 'bsn', 'naam', 'geboortedatum_formatted', 'actie')) 
  })  
  
  
  # ----------------------- KVK  --------------------------------  
  output$dt_kvk <- DT::renderDT({   
    req(rv$kvkAtAdr)  
    rv$kvkAtAdr <- rv$kvkAtAdr %>% select(kvknummer,handelsnaam,omschrijving_sbi) 
    
    datatable(data= rv$kvkAtAdr %>% 
                setNames(.kolom_labeler$get(colnames(rv$kvkAtAdr), "short_label")), options=list(dom=c('t')), rownames= FALSE)  
    
  }) 
  
  
  # ----------------------- Open wave  --------------------------------  
  output$dt_openwave_pc <- DT::renderDT({   
    req( rv$openWaveData  )   
    
    # if (input$toggle_kvk_only & !is.null(rv$kvkAtAdr)) {
    #    rv$openWaveData <- rv$openWaveData %>% filter(handelsregister %in% rv$kvkAtAdr$kvknummer)
    #  }
    
    
    datatable(data=rv$openWaveData %>% setNames(.bron_labeler$get(colnames(rv$openWaveData), "long_label")), options=list(dom=c('t')), rownames= FALSE)  
    
  })  
  
  # ----------------------- Ui updaten  --------------------------------  
  output$txt_adr_info <- renderText({
    req(rv$bagData)
    if (nrow(rv$bagData) > 0) {
      paste('Informatie ', rv$bagData$bag_adres, sep = '') 
    }   
  })
  
  output$txt_adr_pc <- renderText({
    req(rv$bagData)
    if (nrow(rv$bagData) > 0) {
      paste('Meldingen Open Wave postcode ', rv$bagData$postcode, sep = '') 
    }   
  })
  
  output$ui_pand_info <- renderUI({
    
    # Bag rij(en) voor de selectie.
    req(rv$bagData) 
    if (nrow(rv$bagData) > 0) { 
      div(tags$table(  
        actionButton(inputId=rv$bagData$adresseerbaarobject,  onclick="Shiny.setInputValue(\'added_person\', this.id)", label="Naar verzameling"),
        bold_table_row("Adres", paste0(rv$bagData$bag_adres, ", ", rv$bagData$woonplaatsnaam)),
        bold_table_row("BAG object ID", rv$bagData$adresseerbaarobject),
        bold_table_row("Postcode", rv$bagData$postcode),
        bold_table_row("Status", rv$bagData$pandstatus),
        bold_table_row("Bouwjaar", rv$bagData$pandbouwjaar),
        bold_table_row("Oppervlakte", rv$bagData$oppervlakteverblijfsobject),
        bold_table_row("Gebruiksdoel", rv$bagData$verblijfsobjectgebruiksdoel)
      )
      )
    }
    
  })
  
  output$ui_pand_externe_bronnen <- renderUI({
    req(rv$bagData)
    shintobag::externe_bronnen_lokatie(rv$bagData)
  })
  
  output$leaflet_bagobject <- renderLeaflet({
    req(rv$bagData)
    bag_rij <- rv$bagData  
    # R/functions_spatial.R
    panden_data <- get_bag_panden(bag_rij, .db_brk, .db_bag)
    
    leaflet_bag_spatial(panden_data)
    
  })
  
  
  
  selection <- reactive({ 
    
    return (list(bag = rv$bagData, inwoners=rv$peopleAtAdr, bedrijven=rv$kvkAtAdr))
  })
  
  return(selection)  
}
