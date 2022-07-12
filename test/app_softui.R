
library(softui)
devtools::load_all()

options(
  izm_rest_url = "http://127.0.0.1",  # of on-premise "https://izm2-rest.ad.ede.nl" 
  izm_search_timeout = 1000,          # throttle op de search API
  pm_decrypt_secret = yaml::read_yaml("c:/repos/ede_izm_frontend/conf/secret.yml")$secret  # voor pseudomaker decrypt.
)

ui <- softui::simple_page(
  
  izmr::izmr_dependencies(),
  
  softui::box(title = "Search", icon = bsicon("search"),
              
              izmr::izmSearchUI("izm", design = "softui")
  )
  
  
)

server <- function(input, output, session){
  
}

shinyApp(ui, server)
