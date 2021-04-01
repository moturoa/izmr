
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
           tabPanel(value='panel_bronnen',  uiOutput(ns("bron_title_panel")),  
                    casusBronnenUI(ns('bronmodule'))
                    
           ),
           tabPanel(value='panel_tijdlijn', title=HTML('<i class="fa fa-clock-o"></i> Tijdlijn'),   
                    casusTijdlijnUI(ns('tijdlijnmodule'))
                    
           )
    )                    
  )
}


casusModule <- function(input, output, session, clicked_id = reactive(NULL)){
   
  
  # -------------- Retrieve Data for pseudoID --------------
  fam <- .pdb$get_family_depseudo(clicked_id)
  bron <-.pdb$get_all_bronnen(clicked_id)
  
  this_adres <- reactive({
    req(fam())
    
    .pdb$get_adres_depseudo(
          fam() %>%
            filter(relation == "persoon_poi") %>%
            select(vblpostcode, vblhuisnummer, vblhuisletter, vblhuisnummertoevoeging)
        )
  })    
    
    
    
    #observeEvent(bron(),{
    #  print( bron() )
    #})
    #  verh <- .pdb$get_verhuizingen_depseudo(clicked_id)
    #  observeEvent(verh(),{
    #    print( verh() )
    #  })
  # -------------- Overzicht --------------
  callModule(casusOverzichtModule, id="overzichtsmodule", family=fam) 
  
  
  
  # -------------- Adres --------------
  #callModule(casusAdresModule, id="adresmodule", adres=fam) 
  
  
  
  # -------------- Bronnen --------------
  output$bron_title_panel = renderUI({
    HTML(glue('<i class="fa fa-database"></i> Bronnen ({nrow(bron())})'))
  })
  
  callModule(casusBronnenModule, id="bronmodule", bronnen=bron) 
  
  
  # -------------- Tijdlijn --------------
  callModule(casusTijdlijnModule, id="tijdlijnmodule", bronnen=bron) 

  
}

