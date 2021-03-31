
casusTijdlijnUI <- function(id){ 
  
  
  ns <- NS(id)  
  fluidRow(
    shinydashboard::box( title ="filters", width = 4,
                         dateRangeInput(ns('daterange_tijdlijn'), label='Datum', language = "nl"),
                         checkboxInput(ns('chk_nulldate'), label='Toon zonder datum', value = TRUE),
                         checkboxGroupInput(ns("chk_bron_filters"), label = 'Bronnen')   
                         
                         
    ), 
    
    shinydashboard::box( title ="Tijdlijn", width = 8,
                         withSpinner(
                           htmlOutput(ns("persoon_tijdlijn"))
                         )            
    )
  )
} 

casusTijdlijnModule <- function(input, output, session, bronnen){
  rv <- reactiveValues(
    data_bron=NULL
  )
  
  
  
  # updating checker inputs
  observeEvent(bronnen(), ignoreNULL=FALSE,{
    rel <- unique(as.vector(bronnen()$bron))
    updateCheckboxGroupInput(session, 'chk_bron_filters',  choices = rel[!is.na(rel)  ], selected=rel[!is.na(rel) ])
  }) 
  
  
  observeEvent(bronnen(),{ 
    rv$data_bron <- bronnen() %>% select(begindatum, begindatum_formatted, omschrijving,  bron)
    
    
    if(!is.null(rv$data_bron$begindatum)) { 
      mindat <- min( rv$data_bron$begindatum , na.rm = TRUE)
    } else {
      mindat <- NULL
    } 
    updateDateRangeInput(session, 'daterange_tijdlijn', start=mindat)  
  })
  
  
  #--------------- tijdlijn -----------------------
  output$persoon_tijdlijn <- renderUI({
    if(is.null(rv$data_bron) | nrow(rv$data_bron) <= 0){
      tags$p("Geen trajecten gevonden.")
    } else {  
      timelineBlock(
        timelineEnd(color = "gray"),
        make_timeline_persoon(rv$data_bron  %>% filter(bron %in% input$chk_bron_filters & (begindatum >= ymd(input$daterange_tijdlijn[1]) | input$chk_nulldate))),
        timelineStart(color = "gray")
      ) 
    }
  }) 
  
  return(NULL)   
}

# ---------- Tijdlijn =--------------------------
make_timeline_persoon <- function(brondata){ 
  
  if(nrow(brondata) <= 0) return (NULL)
  persoon_data <- brondata %>%
    left_join(., timeline_key, by = "bron") %>%
    arrange(desc(begindatum)) %>%
    split(., 1:nrow(.))
  
  timeline_items <- lapply(persoon_data, function(x){
  
    # deze functie is gedefinieerd in R/timeline_functions.R
    # de list wordt als table weergegeven.
    table_item_timeline(list(Omschrijving = x$omschrijving),  
                        title = x$bron, 
                        date = x$begindatum_formatted,
                        icon = x$icon,
                        icon_color = x$color)
  })
  
  # Hier worden de jaar labels toegevoegd.
  timeline_items <- insert_year_labels(persoon_data, timeline_items, date_column = "Start")
  
  return(timeline_items)
}


timeline_key <- tibble::tribble(
  ~bron, ~icon, ~color,
  "Mens Centraal", "hospital", "red",
  "Carel", "people-carry" , "aqua",
  "Open Wave", "shield-alt", "light-blue",
  "Suite", "band-aid", "green",
  "Allegro", "balance-scale", "maroon",
  "Uitgeschreven", "sign-out-alt", "red",
  "Overleden", "sign-out-alt", "olive",
  "Geboren", "baby", "red",
  "Verhuizing", "home", "orange",
  "Huwelijk", "ring", "aqua",
  "Scheiding", "heart-broken", "red",
  "Kind", "baby", "green",
  "Curatele", "lock", "orange" 
)


# DIrect gejat van MO radar!
table_item_timeline <- function(lis, 
                                date, 
                                title = "Info",
                                icon="info", 
                                icon_color="shinto"){
  
  label_cell_css <- "font-weight: bold; padding-right: 10px;"
  
  table_row <- function(label, content){
    tags$tr(
      tags$td(label, style = label_cell_css),
      tags$td(content)
    )
  } 
  timelineItem(
    title = title,
    #icon = icon,
    #color = icon_color,
    time = date,
    tags$table(
      mapply(table_row, label = names(lis), content = unlist(lis), 
             SIMPLIFY = FALSE)
    )
  ) 
}

insert_year_labels <- function(items, timeline_items, date_column, missing_label = "Geen Datum"){
  
  n_inserted <- 0
  missing_inserted <- FALSE
  
  for(i in seq_along(items)){
    
    yr <- year(ymd(items[[i]][[date_column]]))
    
    # Insert year label, if there is data, and the date is not missing.
    if(length(yr) > 0 && !is.na(yr)){
      
      if(i == 1){
        timeline_items <- list.insert(timeline_items, 1, timelineLabel(yr, color="black"))
        n_inserted <- n_inserted + 1
      } else {
        
        if(yr < prev_yr){
          timeline_items <- list.insert(timeline_items, i + n_inserted, timelineLabel(yr, color="black"))
          n_inserted <- n_inserted + 1
        }
        
      }
      prev_yr <- yr
    }
    
    # Insert 'Missing' label, if there is data, but date is NA.
    # Do this only once - missing dates are always last.
    if(length(yr) > 0 && is.na(yr) && !missing_inserted){
      
      timeline_items <- list.insert(timeline_items, i + n_inserted, 
                                    timelineLabel(missing_label, color="red"))
      missing_inserted <- TRUE
      
    }
    
  } 
  return(timeline_items)
  
}




