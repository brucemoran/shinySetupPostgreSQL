# modals.R

#' Opens modal (text-box for input) to ask for user credentials
#' @return a modal object
#' @rdname mod_validate_user_nt
#' @export

mod_validate_user_nt <- function(INPUT) {

  showModal(
    modalDialog(title = "Enter user and password",
      footer = NULL,
      easyClose = FALSE,
      tagList(
          textInput(paste0("username", INPUT$user_cred), "Username", placeholder = ""),
          passwordInput(paste0("password", INPUT$user_cred), "Password", placeholder = ""),
          textInput(paste0("con_table", INPUT$user_cred), "Table Name", value = "svuhmolrep"),
          actionButton("userpass", "Login"),
          actionButton("valadvan", "Advanced..."),
          actionButton("disconnex", "Disconnect"),
        conditionalPanel(
          condition = "output.advanced == true",
          textInput(paste0("con_drv", INPUT$user_cred), "Driver", value = "RPostgres::Postgres()"),
          textInput(paste0("con_host", INPUT$user_cred), "Host", value = "localhost"),
          textInput(paste0("con_port", INPUT$user_cred), "Port", value = 5432),
          textInput(paste0("con_timezone", INPUT$user_cred), "Timezone", value = "GMT"),
          textInput(paste0("con_dbname", INPUT$user_cred), "Database Name", value = "postgres"),
          textInput(paste0("con_outdir", INPUT$user_cred), "Default Output Dir", value = "~/Downloads")
        )
      )
    )
  )
}

#' Conditionally show advanced connection options for validate_user_nt
#' from: https://stackoverflow.com/questions/65168516
#' @param INPUT input reactive values
#' @rdname mod_validate_user_cond
#' @export

mod_validate_user_cond <- function(INPUT){
  rv <- shiny::reactiveValues(advanced = FALSE)

  shiny::observeEvent(INPUT$valadvan, {
    rv$advanced <- !rv$advanced
  })

  advanced <- shiny::reactive({rv$advanced})

  return(advanced)
}

#' Test if connection to Postgresql can be made
#' @return a modal object
#' @rdname mod_test_db_con
#' @import RPostgres
#' @export

mod_test_db_con <- function(INPUT) {

  cancon <- DBI::dbCanConnect(drv = eval(parse(text = INPUT$con_drv)),
                              host = INPUT$con_host,
                              port = INPUT$con_port,
                              timezone = INPUT$con_timezone,
                              dbname = INPUT$con_dbname,
                              user = INPUT$username,
                              password = INPUT$password)

  return(cancon)
}

#' Test connection to Postgresql, handle success and error
#' @return a modal object
#' @rdname mod_test_db_results
#' @import RPostgres
#' @export

mod_test_db_results <- function(INPUT, CON){

  if(!CON$cancon){
    shinyalert::shinyalert(paste0("Could not connect using specified credentials"),
                           type = "error",
                           showConfirmButton = FALSE)
    return(FALSE)

  } else {

    CON$current <- DBI::dbConnect(drv = eval(parse(text = INPUT$con_drv)),
                                  host = INPUT$con_host,
                                  port = INPUT$con_port,
                                  timezone = INPUT$con_timezone,
                                  dbname = INPUT$con_dbname,
                                  user = INPUT$username,
                                  password = INPUT$password)

    CON$extantable <- DBI::dbExistsTable(CON$current,
                                         INPUT$con_table)

    if(CON$extantable){

      shinyalert::shinyalert("Login successful",
                             type = "success",
                             showConfirmButton = TRUE)
      return(TRUE)
    } else {

      shinyalert::shinyalert(paste0("Table: ", INPUT$con_table, " does not exist but has been created and is empty"),
                             type = "warning",
                             showConfirmButton = TRUE)

      DBI::dbCreateTable(conn = CON$current,
                         name = INPUT$con_table,
                         fields = new_table_cols())

      CON$extantable <- DBI::dbExistsTable(conn = CON$current,
                                           name = INPUT$con_table)

      return(TRUE)
     }
  }
}

#' Tell user a table they are trying to create exists and will be appended with new data
#' @return a modal object
#' @rdname mod_load_data_proceed
#' @export

mod_load_data_proceed <- function(INPUT) {

  showModal(
   modalDialog(title = paste0("Table ",
                              INPUT$con_table,
                              " exists already.\n",
                              "If you go proceed, data will be combined"),
               easyClose = FALSE,
               actionButton(inputId = "go_loaddata", label = "Ok"),
               modalButton("Cancel"),
               footer = NULL)
  )
}

#' Disconnection from Postgresql
#' @return a modal object
#' @rdname mod_disc_db_con
#' @import RPostgres
#' @export

mod_disc_db_con <- function(INPUT) {

  all_cons <- DBI::dbListConnections(eval(parse(text = INPUT$con_drv)))

  for(con in all_cons){
    DBI::dbDisconnect(con)
  }
  shinyalert::shinyalert("Disconnected",
                         type = "success",
                         showConfirmButton = FALSE)
  shiny::stopApp(returnValue = invisible())
}

#' Opens modal (text-box for input) to ask if save should go ahead
#' @param INPUT object
#' @return a modal object
#' @rdname mod_sure_to_save
#' @export

mod_sure_to_save <- function(INPUT) {
  showModal(
    modalDialog(
      title = "Enter Table Name to Which to Save (N.B. saving to current table overwrites data and saves edits)",
      textInput(inputId = "save_tab_name", "Table Name", value = INPUT$con_table),
      easyClose = FALSE,
      actionButton(inputId = "go_save", label = "Ok"),
      modalButton("Cancel"),
      footer = NULL
    )
  )
}

#' Opens modal telling save location
#' @return a modal object
#' @rdname mod_saving_to
#' @export

mod_saving_to <- function(INPUT) {
  showModal(
    modalDialog(
      title = paste0("Saving data to: ", INPUT$con_table, ".", Sys.Date(), ".rds"),
      easyClose = FALSE,
      actionButton(inputId = "go_rds", label = "Ok"),
      modalButton("Cancel"),
      footer = NULL
    )
  )
}

#' Opens modal (text-box for input) to ask for name of new column
#' @return a modal object
#' @rdname mod_add_column
#' @export

mod_add_column <- function(INPUT) {
  showModal(modalDialog(title = "Enter new column name",
            textInput(inputId = "new_col", "Column Name", placeholder = ""),
            actionButton("ins_col", "Add"),
            modalButton("Cancel"),
            footer = NULL)
  )
}

#' Opens modal (dropdowns for input) to ask for orders of columns
#' @param VALS_DATA named list of colnames of data
#' @return a modal object
#' @rdname mod_order_column
#' @export

mod_order_column <- function(VALS_DATA) {
  colns <- colnames(VALS_DATA)
  choices_list <- as.list(colns)
  names(choices_list) <- colns

  showModal(modalDialog(title = "Select new column order",
            lapply(1:length(colns), function(f) {
              shiny::selectInput(inputId = paste0("column_", f),
                                 label = paste0("Column ", f),
                                 choices = choices_list,
                                 selected = choices_list[[f]])
            }),
            actionButton("arr_col", "Reorder"),
            modalButton("Cancel"),
            footer = NULL)
  )
}

#' Opens modal (dropdowns for input) to ask for column to rename
#' @param VALS_DATA named list of colnames of data
#' @return a modal object
#' @rdname mod_rename_column
#' @export

mod_rename_column <- function(VALS_DATA) {
  colns <- colnames(VALS_DATA)
  choices_list <- as.list(colns)
  names(choices_list) <- colns

  showModal(modalDialog(title = "Select Column to Rename",
            shiny::selectInput(inputId = "col_to_rename",
                               label = "Current Column Name",
                               choices = choices_list),
            textInput(inputId = "new_colname", "New Column Name", placeholder = ""),
            actionButton("rename_col", "Rename"),
            modalButton("Cancel"),
            footer = NULL)
  )
}

#' Opens modal to ask for columns to delete (only those without data available)
#' @param VALS_DATA vals_data object
#' @return a modal object
#' @rdname mod_delete_column
#' @export

mod_delete_column <- function(VALS_DATA) {
  colns <- colnames(VALS_DATA)
  choices_list <- as.list(colns)
  names(choices_list) <- colns

  showModal(modalDialog(title = "Select column to delete",
            shiny::selectInput(inputId = "col_to_del",
                               label = NULL,
                               choices = choices_list,
                               selected = NULL),
            actionButton("col_del", "Delete"),
            modalButton("Cancel"),
            footer = NULL)
  )
}

#' Opens modal (dropdowns for input) to ask for columns to display uniqly in table
#' @param VALS_DATA named list of colnames of data
#' @return a modal object
#' @rdname mod_uniq_columns
#' @export

mod_uniq_columns <- function(VALS_DATA) {
  colns <- colnames(VALS_DATA)
  choices_list <- as.list(colns)
  names(choices_list) <- colns

  showModal(modalDialog(title = "Select Columns to Display Unique Contents",
            lapply(1:length(colns), function(f) {
              shiny::selectInput(inputId = paste0("ucolumn_", f),
                                 label = choices_list[[f]],
                                 choices = c("Yes", "No"),
                                 selected = "No")
            }),
            actionButton("uniq_col", "Uniquify"),
            modalButton("Cancel"),
            footer = NULL)
  )
}

#' Opens modal to ask for map from data column to table column
#' @param VALS_TIB input tibble of new data to tabulate
#' @param SHEETNAME for XLSX input allow sheet name to be used as a column
#' @return a modal object
#' @rdname mod_map_columns
#' @export

mod_map_columns <- function(INPUT, VALS_TIB, NAME = NULL) {

  ##data, and name of sheet which can hold info we want
  dat_colns <- c("empty", colnames(VALS_TIB), "Test")
  dat_choice_list <- as.list(dat_colns)
  names(dat_choice_list) <- dat_colns

  ##new_table_cols
  ntc_colns <- colnames(shinySetupPostgreSQL::new_table_cols())[-1]

  ##arrange columns of modal
  leng <- 1:length(ntc_colns)
  spleng <- split(leng, ceiling(seq_along(leng)/(length(leng)/3)))

  ##modal
  showModal(
    modalDialog(
      title = paste0("Map Input Data from sheet: ", NAME),
        column(12,
          column(4, lapply(spleng$`1`, function(f){
            shiny::selectInput(inputId = paste0("ntc_", ntc_colns[f]),
                               label = ntc_colns[f],
                               choices = unique(unlist(dat_choice_list)),
                               selected = shinySetupPostgreSQL::grep_choice_selectin(choices = unique(unlist(dat_choice_list)),
                                                      label = ntc_colns[f],
                                                      fail = "empty"))
                    })
                ),
          column(4, lapply(spleng$`2`, function(f){
            shiny::selectInput(inputId = paste0("ntc_", ntc_colns[f]),
                               label = ntc_colns[f],
                               choices = unique(unlist(dat_choice_list)),
                               selected = shinySetupPostgreSQL::grep_choice_selectin(choices = unique(unlist(dat_choice_list)),
                                                      label = ntc_colns[f],
                                                      fail = "empty"))
                    })
                ),
          column(4, lapply(spleng$`3`, function(f){
            shiny::selectInput(inputId = paste0("ntc_", ntc_colns[f]),
                               label = ntc_colns[f],
                               choices = unique(unlist(dat_choice_list)),
                               selected = shinySetupPostgreSQL::grep_choice_selectin(choices = unique(unlist(dat_choice_list)),
                                                      label = ntc_colns[f],
                                                      fail = "empty"))
                     })
                )
        ),
      actionButton("go_map_table", "Map"),
      modalButton("Cancel"),
      easyClose = FALSE,
      footer = NULL)
  )
}
