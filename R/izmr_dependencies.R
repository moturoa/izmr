#' Dependencies for izmr
#' @description In the UI, place the command \code{izmr::izmr_dependencies()}.
#' @export
izmr_dependencies <- function() {
  
  list(
    htmltools::htmlDependency(name = "izmsearch", version = "0.1",
                              package = "izmr",
                              src = "assets",
                              script = "izmsearch/izmsearch.js"
    ),

    htmltools::htmlDependency(name = "izmrestcalls", version = "0.1",
                              package = "izmr",
                              src = "assets",
                              script = "restcalls/getRecordFromId.js"
    )
  )
}


