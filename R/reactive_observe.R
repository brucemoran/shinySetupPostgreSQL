# reactive_observe.R

#' DT:renderDataTable filtering
#' The resulting output should be caught by output$maintable
#' @return a DataTable object
#' @rdname render_simple_maintable
#' @importFrom magrittr '%>%'
#' @export

render_simple_maintable <- function(DATAER){

    DATAER %>% dplyr::select(1,2,3,4,5,6,7,8,13, everything()) %>%
               dplyr::distinct()
}

#' DT:renderDataTable
#' @param VALS_DATA data frame with columns to display uniquely
#' @return a data.frame object
#' @rdname render_unique_maintable
#' @importFrom magrittr '%>%'
#' @export

render_unique_maintable <- function(VALS_DATA){

  uniq_list <- apply(VALS_DATA, 2, function(f){
    return(unique(unlist(f)))
  })

  uniq_leng <- unlist(apply(VALS_DATA, 2, function(f){
    return(length(unique(unlist(f))))
  }))

  todf <- lapply(uniq_list, function(f){
    tms <- max(uniq_leng) - length(f)
    c(f, rep("", times = tms))
  })

  tb <- tibble::as_tibble(do.call(cbind, todf))
}
