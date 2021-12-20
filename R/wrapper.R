#' Launch the app
#'
#' @return launched app
#' @rdname launchSSP
#' @export

launchSSP <- function(){
  shiny::runApp(system.file(package = "shinySetupPostgreSQL", "app"))
}
