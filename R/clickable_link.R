#' Make a clickable link
#' @export
clickable_link <- function(value, label){
  
  glue("<a onclick = \"setClickedId('{value}')\">",
       "<span style = \"cursor: pointer;\">{label}",
       "</span></a>")
}
