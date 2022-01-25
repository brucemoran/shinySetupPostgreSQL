# server.R

function(input, output, session) {

  ## * log in to database ------------------------------------------------------

  ## check credentials exist for Postgresql database, else exit

  shiny::observe( shinySetupPostgreSQL::validate_user_nt(input) )

  output$advanced <- shinySetupPostgreSQL::validate_user_cond(input)

  shiny::outputOptions(output, "advanced", suspendWhenHidden = FALSE)

  ## test connection to ensure user and connection OK, and connect

  con <- shiny::reactiveValues()

  shinySetupPostgreSQL::obsev_userpass(input, con)

  ## allow disconnect as too many open connections (16+)

  shinySetupPostgreSQL::obsev_disconnex(input)

  ## allow changing of credentials for database

  shinySetupPostgreSQL::obsev_db_conxn(input, output)

  ## * load table that already exists ------------------------------------------

  vals_data <- shiny::reactiveValues()

  shinySetupPostgreSQL::obsev_extantabled(input, con, vals_data)

  ## * take file inputs --------------------------------------------------------

  shinySetupPostgreSQL::obsev_FILENAMES(input, con, vals_data)

  ## * overwrite table with input ----------------------------------------------

  shinySetupPostgreSQL::obsev_go_loaddata(input, con, vals_data)

  ## * save current table ------------------------------------------------------

  shinySetupPostgreSQL::obsev_save_tab(input)

  shinySetupPostgreSQL::obsev_go_save(input, con, vals_data)

  ## * add columns to table ----------------------------------------------------

  shinySetupPostgreSQL::obsev_add_col(input)

  shinySetupPostgreSQL::obsev_ins_col(input, vals_data)

  ## * remove empty columns from table -----------------------------------------

  shinySetupPostgreSQL::obsev_del_col(input, vals_data)

  shinySetupPostgreSQL::obsev_col_del(input, vals_data)

  ## * reorder columns in table ------------------------------------------------

  shinySetupPostgreSQL::obsev_ord_col(input, vals_data)

  shinySetupPostgreSQL::obsev_arr_col(input, vals_data)

  ## * rename columns in table -------------------------------------------------

  shinySetupPostgreSQL::obsev_ren_col(input, vals_data)

  shinySetupPostgreSQL::obsev_rename_col(input, vals_data)

  ## * output tables -----------------------------------------------------------

  shinySetupPostgreSQL::obsev_show_tab(input, vals_data, output)

  shinySetupPostgreSQL::obsev_save_rds(input)

  shinySetupPostgreSQL::obsev_go_rds(input, con, vals_data)

  ## * unique values of each column selected ---------------------------------

  shinySetupPostgreSQL::obsev_uni_col(input, vals_data)

  shinySetupPostgreSQL::obsev_uniq_col(input, vals_data, output)

  ## * disconnect from db on session end -------------------------------------

  session$onSessionEnded(function() {
    shiny::observe(
      shinySetupPostgreSQL::disc_db_con(input)
    )
  })
}
