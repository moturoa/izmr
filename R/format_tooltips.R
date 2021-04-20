#' Naam met tooltip (overleden datum), adres met tooltip (laatste adres in Ede)
#' @param naam Naam
#' @param overleden Date
#' @rdname format_tooltip
#' @export
format_naam_tooltip <- function(naam, overleden){
  tooltip <- ifelse(!is.na(overleden), 
                    glue::glue("<i class='fa fa-cross' data-toggle='tooltip' data-placement='right' 
                               title='Overleden op { format(overleden, '%d-%m-%Y')}'></i>"), 
                    "")
  paste(naam, tooltip)
}

#' @rdname format_tooltip
#' @export
format_adres_tooltip <- function(vwsdatuminschrijving,
                         vwsgemeentevaninschrijvingomschrijving,
                         straatnaam,
                         huisnummer,
                         huisletter,
                         postcode){
  
  ifelse(is.na(vwsdatuminschrijving), 
         paste(straatnaam, huisnummer, huisletter, postcode), 
         glue::glue("Verhuisd naar {vwsgemeentevaninschrijvingomschrijving} op",
                    " {format(vwsdatuminschrijving, '%d-%m-%Y')}",
                    " <i class='fa fa-home' data-toggle='tooltip' data-placement='right' ",
                    "title='Laatst bekende adres binnen Ede: ",
                    "{paste(straatnaam, huisnummer, huisletter, postcode)}'></i>"))
  
}  
