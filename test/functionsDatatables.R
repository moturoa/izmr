familyTable <- function(family, session){

dat <- family() %>% filter(relation != 'persoon_poi')  %>% arrange(geboortedatum) %>%
                    mutate(adres = glue("{straatnaam} {huisnummer} {huisletter} {postcode}"),
                           bsn = clickable_link(pseudo_bsn, bsn))  

dat$pin = buttonInput(
  FUN = actionButton,
  len = nrow(dat),
  id =  dat$pseudo_bsn,
  label = "+", 
  onclick = glue('Shiny.onInputChange(\"{session$ns("expandPseudoBsn")}\",  (this.id))') 
)  


datatable(data=dat %>% select(pin, bsn, naam, geboortedatum, adres,  relation) ,
          extensions = "Responsive", selection = "multi", 
          options=list(dom=c('t'), lengthChange = FALSE,
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
