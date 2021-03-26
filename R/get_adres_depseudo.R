#' Get all persons at an address (de-anonymized)
#' @param adres is a *reactive* **list** with 'postcode', 'huisnummer', 'huisletter', 'huisnummertoevoeging',
#' without NA (must be replaced with '')!
#' @export
get_adres_depseudo <- function(adres, database_object){
  
  
  adres_data <- reactive({
    
    req(adres())
    database_object$get_person_brp(what = "adres", adres = adres())
    
  })
  
  adres_id <- reactive({
    adres_data() %>% pull(pseudo_bsn)
  })
  
  rest_out <- callModule(restCallModule, "adres", pseudo_ids = adres_id, what = "lookup")
  
  
  reactive({
    
    req(adres_data())
    
    # dwz: depseudonimiseer is klaar.
    req(nrow(rest_out()) > 0)
    
    left_join(adres_data(), rest_out(), 
              by = "pseudo_bsn", 
              suffix = c(".y", ""))
    
  })
  
}

