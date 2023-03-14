#' Make a clickable link
#' @export
clickable_link <- function(value, label, inputId = "izm-izmclickedid", na_label = "Onbekend"){
  
  i_na <- which(is.na(label))
  
  out <- glue("<a href='javascript:;' onclick = \"setClickedId('{value}', '{inputId}')\">",
       "<span style = \"cursor: pointer;\">{label}",
       "</span></a>")
  
  out[i_na] <- na_label
  out
}
