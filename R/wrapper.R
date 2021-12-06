#' Launch the app
#'
#' @return launched app
#' @rdname launchSSP
#' @export

launchSSP <- function(){
  shiny::runApp(paste0(.libPaths()[1], "/shinySetupPostgreSQL/app"))
}
