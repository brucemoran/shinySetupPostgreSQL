#' Launch the app
#' @export

launchSSP <- function(){
  shiny::runApp(paste0(.libPaths()[1], "/shinySetupPostgreSQL/app"))
}
