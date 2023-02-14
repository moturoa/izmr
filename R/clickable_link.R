#' Make a clickable link
#' @export
clickable_link <- function(value, label, inputId = "izm-izmclickedid"){
  
  glue("<a href='javascript:;' onclick = \"setClickedId('{value}', '{inputId}')\">",
       "<span style = \"cursor: pointer;\">{label}",
       "</span></a>")
}
