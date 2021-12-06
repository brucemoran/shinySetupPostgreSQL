# modals.R

#' Opens modal (text-box for input) to ask for user credentials
#' @return a modal object
#' @rdname validate_user
#' @export

validate_user_nt <- function(INPUT) {
  showModal(modalDialog(title = "Enter user and password",
            textInput(paste0("username", INPUT$user_cred), "Username", placeholder = ""),
            passwordInput(paste0("password", INPUT$user_cred), "Password", placeholder = ""),
            textInput(paste0("con_drv", INPUT$user_cred), "Driver", value = "DBI::dbDriver('PostgreSQL')"),
            textInput(paste0("con_host", INPUT$user_cred), "Host", value = "localhost"),
            textInput(paste0("con_port", INPUT$user_cred), "Port", value = 5432),
            textInput(paste0("con_dbname", INPUT$user_cred), "Database Name", value = "postgres"),
            textInput(paste0("con_newtable", INPUT$user_cred), "Table Name", value = "svuh_mol_rep_15_21"),
            actionButton("userpass", "Login"),
            actionButton("disconnex", "Disconnect"),
            footer = NULL)
  )
}

#' Test connection to Postgresql, handle success and error
#' @return a modal object
#' @rdname test_db_con
#' @import RPostgreSQL
#' @export

test_db_con <- function(INPUT) {
  print(INPUT)
  cancon <- DBI::dbCanConnect(drv = eval(parse(text = INPUT$con_drv)),
                              host=INPUT$con_host,
                              port=INPUT$con_port,
                              dbname=INPUT$con_dbname,
                              user=INPUT$username,
                              password=INPUT$password)
  print(cancon[1])
  if (cancon[1] == TRUE) {

    shinyalert::shinyalert("Login successful",
                           type = "success",
                           showConfirmButton = TRUE)

  } else {
    RPostgreSQL::dbDisconnect(eval(parse(text = INPUT$con_drv)))
    shinyalert::shinyalert("Login failed", "please try entering credentials again\nor contact Bruce Moran for access",
                           type = "error",
                           showConfirmButton = FALSE)
    shiny::stopApp(returnValue = invisible())
  }
}

#' Tell user a table they are trying to create exists and will be appended with new data
#' @return a modal object
#' @rdname newtable_exists
#' @export

newtable_exists <- function(INPUT) {

  showModal(
   modalDialog(title = paste0("Table ",
                              INPUT$con_newtable,
                              " exists already.\n",
                              "If you go proceed, data will be combined"),
               easyClose = FALSE,
               actionButton(inputId = "go_askdata", label = "Ok"),
               modalButton("Cancel"),
               footer = NULL)
  )
}

#' Disconnection from Postgresql
#' @return a modal object
#' @rdname disc_db_con
#' @import RPostgreSQL
#' @export

disc_db_con <- function(INPUT) {
  print(INPUT)
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
#' @return a modal object
#' @rdname connection_change
#' @export

connection_change <- function(INPUT) {
  showModal(modalDialog(title = "Database Connection Credentials",
    textInput(paste0("username", INPUT$user_cred), "Username", placeholder = ""),
    passwordInput(paste0("password", INPUT$user_cred), "Password", placeholder = ""),
    textInput(paste0("con_drv", INPUT$user_cred), "Driver", value = "DBI::dbDriver('PostgreSQL')"),
    textInput(paste0("con_host", INPUT$user_cred), "Host", value = "localhost"),
    textInput(paste0("con_port", INPUT$user_cred), "Port", value = 5432),
    textInput(paste0("con_dbname", INPUT$user_cred), "Database Name", placeholder = ""),
    textInput(paste0("con_newtable", INPUT$user_cred), "New Table Name", placeholder = ""),
    actionButton("userpass", "Login"),
    actionButton("disconnex", "Disconnect"),
    footer = NULL)
  )
}

#' Modal to tell user to select data input
#' @return a modal object
#' @rdname tell_about_data
#' @export

tell_about_data <- function() {
  showModal(
    modalDialog(title = "Select data to load (.xlsx or .rds formats only)?",
                easyClose = FALSE,
                actionButton(inputId = "go_data", label = "Ok"),
                modalButton("Cancel"),
                footer = NULL)
  )
}


#' Modal to tell user data loaded
#' @return a modal object
#' @rdname tell_about_load
#' @export

tell_about_load <- function() {
  showModal(
    modalDialog(title = "Data loaded",
                easyClose = FALSE,
                actionButton(inputId = "go_datared", label = "Ok"),
                footer = NULL)
  )
}

#' Opens modal (text-box for input) to ask if save should go ahead
#' @param INPUT object
#' @param CON connection to db
#' @param VALS_DATA data object
#' @return a modal object
#' @rdname sure_to_save
#' @export

sure_to_save <- function(INPUT, CON, VALS_DATA) {
  showModal(
    modalDialog(
      title = "Save current table? Saving overwrites previous data",
      easyClose = FALSE,
      actionButton(inputId = "go_save", label = "Ok"),
      modalButton("Cancel"),
      footer = NULL
    )
  )

  shiny::observeEvent(INPUT$go_save, {
    dplyr::copy_to(dest = CON$current,
                   df = VALS_DATA,
                   name = INPUT$con_newtable,
                   temporary = FALSE,
                   overwrite = TRUE)
    shiny::removeModal()
  })
}

#' Opens modal telling save location
#' @return a modal object
#' @rdname saving_to
#' @export

saving_to <- function(INPUT) {
  showModal(
    modalDialog(
      title = paste0("Saving data to: ", INPUT$con_newtable, ".", Sys.Date(), ".rds"),
      easyClose = FALSE,
      actionButton(inputId = "go_rds", label = "Ok"),
      modalButton("Cancel"),
      footer = NULL
    )
  )
}


#' Opens modal (text-box for input) to ask for name of new column
#' @return a modal object
#' @rdname add_column
#' @export

add_column <- function(INPUT) {
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
#' @rdname order_column
#' @export

order_column <- function(VALS_DATA) {
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

#' Opens modal to ask for columns to delete (only those without data available)
#' @param VALS_DATA vals_data object
#' @return a modal object
#' @rdname delete_column
#' @export

delete_column <- function(VALS_DATA) {
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
#' @rdname uniq_columns
#' @export

uniq_columns <- function(VALS_DATA) {
  colns <- colnames(VALS_DATA)
  choices_list <- as.list(colns)
  names(choices_list) <- colns

  showModal(modalDialog(title = "Select Columns to Display Unique Contents",
            lapply(1:length(colns), function(f) {
              shiny::selectInput(inputId = paste0("ucolumn_", f),
                                 label = choices_list[[f]],
                                 choices = c("Yes", "No"),
                                 selected = "Yes")
            }),
            actionButton("uniq_col", "Uniquify"),
            modalButton("Cancel"),
            footer = NULL)
  )
}
