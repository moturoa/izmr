
# Module zoals een casusModule oid zou zijn.
casusModuleUI <- function(id){
  
  
  ns <- NS(id)
  fluidRow(
    
    tabBox(id = ns("tabset_casus"),width = 12, height = "850px",
           
           tabPanel(value='panel_family',   title=HTML('<i class="fa fa-users"></i> Familie'),
                    casusOverzichtUI(ns('overzichtsmodule')) 
           ),
           
           tabPanel(value='panel_adres',   title=HTML('<i class="fa fa-home"></i> Adres'),
                    #casusAdresUI(ns('adresmodule')) 
                    
           ),
           tabPanel(value='panel_netwerk',  title=HTML('<i class="fa fa-clock-o"></i> Netwerk'),
                    #casusNetwerkUI(ns('netwerkmodule')) 
           ),
           tabPanel(value='panel_bronnen',  uiOutput(ns("title_panel")),  
                    #casusBronnenUI(ns('bronmodule'))
                    
           ),
           tabPanel(value='panel_tijdlijn', title=HTML('<i class="fa fa-clock-o"></i> Tijdlijn'),   
                    #casusTijdlijnUI(ns('tijdlijnmodule'))
                    
           )
    )                    
  )
}


casusModule <- function(input, output, session, clicked_id = reactive(NULL)){
  
  
  fam <- .pdb$get_family_depseudo(clicked_id)
  #fam <- izmr::get_family_depseudo(clicked_id, .pdb)
  
  
  callModule(casusOverzichtModule, id="overzichtsmodule", family=fam) 
  
  
  
  
  
  
  
  #callModule(casusBronnenModule, id="overzichtsmodule", bronnen=fam) 
  
    
  
  
  
  
  
  
  
  
  
  
  
  this_person <- reactive({
    fam() %>%
      filter(relation == "persoon_poi")
  })
  
  this_adres <- reactive({
    req(fam())
    
    fam() %>%
      filter(relation == "persoon_poi") %>%
      select(vblpostcode, vblhuisnummer, vblhuisletter, vblhuisnummertoevoeging)
  })
  
  adres_personen <- izmr::get_adres_depseudo(this_adres, .pdb)
  
  
  person_suite <- reactive({
    .pdb$get_suite(clicked_id())
  })
  
  
  output$ui_person_name <- renderUI({
    tags$h4(this_person()$naam)
  })
  
  output$tab_person <- renderTable({
    
    this_person() %>%
      mutate(adres = paste(straatnaam, huisnummer, huisletter)) %>%
      select(naam, geboortedatum, adres)
    
  })
  
  output$tab_parents <- renderTable({
    
    fam() %>%
      filter(relation %in% c("vader","moeder")) %>%
      mutate(
        naam = izmr::clickable_link(pseudo_bsn, naam),
        adres = paste(straatnaam, huisnummer, huisletter)) %>%
      select(relation, naam, geboortedatum, adres, overleden)
    
  }, sanitize.text.function = function(x) x)
  
  output$tab_kids <- renderTable({
    
    fam() %>%
      filter(relation %in% c("zoon","dochter")) %>%
      mutate(naam = izmr::clickable_link(pseudo_bsn, naam),
             adres = paste(straatnaam, huisnummer, huisletter)) %>%
      select(relation, naam, geboortedatum, adres)
    
  }, sanitize.text.function = function(x) x)
  
  output$tab_huwelijk <- renderTable({
    
    fam() %>%
      filter(grepl("partner", relation)) %>%
      mutate(adres = paste(straatnaam, huisnummer, huisletter)) %>%
      select(relation, naam, geboortedatum, adres)
    
  })
  
  
  output$tab_adres <- renderTable({
    
    req(adres_personen())
    
    adres_personen() %>%
      filter(vwsdatuminschrijving == "") %>%
      select(
        naam, geboortedatum, geslacht, overleden
      )
    
  })
  
  
  output$tab_suite <- renderTable({
    
    req(person_suite())
    
    person_suite() %>%
      select(
        bron, begindatum, einddatum, omschrijving
      )
    
  })
  
}

