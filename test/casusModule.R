
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
                    casusBronnenUI(ns('bronmodule'))
                    
           ),
           tabPanel(value='panel_tijdlijn', title=HTML('<i class="fa fa-clock-o"></i> Tijdlijn'),   
                    #casusTijdlijnUI(ns('tijdlijnmodule'))
                    
           )
    )                    
  )
}


casusModule <- function(input, output, session, clicked_id = reactive(NULL)){
  
  
  fam <- .pdb$get_family_depseudo(clicked_id)
  bron <-.pdb$get_all_bronnen(clicked_id)
  observeEvent(bron(),{
    print( bind_rows(bron()) )
  })
  
  callModule(casusOverzichtModule, id="overzichtsmodule", family=fam) 
  
  #callModule(casusBronnenModule, id="bronmodule", bronnen=bron) 
  #callModule(casusTijdlijnModule, id="tijdlijnmodule", bronnen=rbind(bron)) 

  
}

