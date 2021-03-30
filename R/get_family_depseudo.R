#' Lookup family, and de-anonymize if possible.
#' @description Get family for a person, and immediately de-anonymize if possible. 
#' Returns a reactive with the family data (including anonymous data), joined with the 
#' de-anonymized data.
#' @param id_in A reactive vector with a pseudo-BSN.
#' @param database_object An instance of the pseudoData R6 object (includes database connection).
#' @export
get_family_depseudo_old <- function(id_in, database_object){
  
  fam <- reactive({
    req(id_in())
    database_object$get_family(id_in(), what = "bsn")
  })
  
  fam_id <- reactive({
    fam() %>% pull(pseudo_bsn)
  })
  
  f_out <- callModule(restCallModule, "fam", pseudo_ids = fam_id, what = "lookup")
  
  
  reactive({
    
    req(fam())
    req(nrow(f_out()) > 0)
    
    left_join(fam(), f_out(), 
              by = "pseudo_bsn", 
              suffix = c(".y", ""))
    
  })
  
}

get_bronnen <- function(id_in, database_object){
  
  bron <- reactive({
    req(id_in())
    database_object$get_all_bronnen(id_in() )
  })
  
  fam_id <- reactive({
    fam() %>% pull(pseudo_bsn)
  })
  
  f_out <- callModule(restCallModule, "fam", pseudo_ids = fam_id, what = "lookup")
  
  
  reactive({
    
    req(fam())
    req(nrow(f_out()) > 0)
    
    left_join(fam(), f_out(), 
              by = "pseudo_bsn", 
              suffix = c(".y", ""))
    
  })
  
}
