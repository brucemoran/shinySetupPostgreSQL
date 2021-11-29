# reactive_observe.R

#' DT:renderDataTable filtering
#' The resulting output should be caught by output$maintable
#' @return a DataTable object
#' @rdname render_simple_maintable
#' @importFrom magrittr '%>%'
#' @export

render_simple_maintable <- function(INPUT, DATAER){

    DATAER %>% dplyr::select(1,2,3,4,5,6,7,8,13, everything()) %>%
               dplyr::distinct()
}
