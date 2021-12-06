#' Launch the app

launchSSP <- function(){
  shiny::runApp(paste0(base::system.file(package = "shinySetupPostgreSQL"), "/app"))
}
