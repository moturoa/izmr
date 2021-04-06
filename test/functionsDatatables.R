familyTable <- function(family, session){

    # create cklickable and add missing pseudo bsn
  dat <- family() %>% filter(relation != 'persoon_poi')  %>% arrange(geboortedatum) %>%
                      mutate(adres = glue("{straatnaam} {huisnummer} {huisletter} {postcode}"),
                             bsn = clickable_link(pseudo_bsn, bsn),
                             pseudo_bsn = ifelse(pseudo_bsn=="", uuid::UUIDgenerate(), pseudo_bsn))  
  
  dat$pin <- buttonInput(
    FUN = actionButton,
    len = nrow(dat),
    id =  dat$pseudo_bsn,
    label = "+", 
    onclick = glue('Shiny.setInputValue(\"{session$ns("expandPseudoBsn")}\",  (this.id))') 
  )  
  
  dat %>% 
    select(pin, bsn, naam, geboortedatum, adres,  relation) %>%
    datatable(extensions = "Responsive", 
            selection = "none", 
            options = list(dom = 't', lengthChange = FALSE,
                         pageLength=15), 
            escape = FALSE,
            rownames= FALSE)
}


buttonInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN( id[i], ...))
  }
  inputs
} 

bold_table_row <- function(label, content){
  tags$tr(
    tags$td(label, style="font-weight: bold; padding-right: 10px;"),
    tags$td(content)
  )
}



datatableBron <- function(..., data, selectie = c('begindatum_formatted', 'omschrijving'),secret_cols=NULL,extra_selectie = NULL) {
  if(!is.null(extra_selectie)){
    selectie <-  c(selectie, extra_selectie)
  }
  # add secret cols at the end!
  if(!is.null(secret_cols)){
    selectie <-  c(selectie, secret_cols)
  }  
  # Adding indicatie triangles
  data <- data %>%  select(any_of(selectie)) 
   
  
  datatable(...,data=data)
}


