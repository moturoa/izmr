
#---- header -----
header <- shinydashboardPlus::dashboardHeader(
  
  title = tagList(
    span(class = "logo-lg logo-smaller", "Integrale Zoekmachine"), 
    icon("search")
  ), 
  tags$li(class = "dropdown",
          
          # About Menu
          tags$a(href="#", class="dropdown-toggle", 
                 `data-toggle` = "dropdown",
                 icon("info-circle")
          ),
         
  ),
  
  # Logout Menu (werkt niet lokaal, alleen op Shiny Server Pro)
  dropdownMenuOutput("profile"),
  dropdownMenuOutput("selverz"),
  
  controlbarIcon = shiny::icon("list")
)

#----- Sidebar -----

sidebar <- dashboardSidebar(
  
  sidebarMenuOutput("ui_sidebar")
  
)

#----- Body -----

body <- dashboardBody(
  
  
  izmr::izmr_dependencies(),
  
  
  tabsetPanel(id = "main",
              tabPanel("Search", value = "search",
                       
                       # werkt alleen met id = 'izm' (vanwege namespacing die we lastig in JS kunnen zetten)
                       izmr::izmSearchUI("izm")
                       
              ),
              tabPanel("Casus", value = "casus",
                       
                       tags$h4("Voorbeeld module"),
                       casusModuleUI("casus_izm")
                       
              )
              
  )
)

#----- Right sidebar ---- 
rightsidebar <- dashboardControlbar( 
  skin = "light",
  id = "rightsidebar",
  width = 700,
  
  controlbarMenu(
    controlbarItem(
      #id = 1,
      #icon = "list",
      #active = TRUE,   
      "hello"
    )    
  )
  
) 

#----- dashboardPage -----
shinydashboardPlus::dashboardPage(
  header = header, 
  sidebar = sidebar, 
  body = body,
  controlbar = rightsidebar
) 







