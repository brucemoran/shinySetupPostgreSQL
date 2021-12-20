# server.R

function(input, output, session) {

  ## * log in to database ------------------------------------------------------

  ## check credentials exist for Postgresql database, else exit

  shiny::observe( shinySetupPostgreSQL::validate_user_nt(input) )

  output$advanced <- shinySetupPostgreSQL::validate_user_cond(input)

  shiny::outputOptions(output, "advanced", suspendWhenHidden = FALSE)

  ## test connection to ensure user and connection OK, and connect

  con <- shiny::reactiveValues()

  obsev_userpass(input, con)

  ## allow disconnect as too many open connections (16+)

  obsev_disconnex(input)

  ## allow changing of credentials for database

  obsev_db_conxn(input, output)

  ## * load table that already exists ------------------------------------------

  vals_data <- shiny::reactiveValues()

  obsev_extantabled(input, con, vals_data)

  ## * take file inputs --------------------------------------------------------

  obsev_FILENAMES(input, con, vals_data)

  obsev_go_datared(input, con, vals_data)

  ## * overwrite table with input ----------------------------------------------

  obsev_go_askdata(input, con, vals_data)

  ## * save current table ------------------------------------------------------

  obsev_save_tab(input)

  obsev_go_save(input, con, vals_data)

  ## * add columns to table ----------------------------------------------------

  obsev_add_col(input)

  obsev_ins_col(input, vals_data)

  ## * remove empty columns from table -----------------------------------------

  obsev_del_col(input, vals_data)

  obsev_col_del(input, vals_data)

  ## * reorder columns in table ------------------------------------------------

  obsev_ord_col(input, vals_data)

  obsev_arr_col(input, vals_data)

  ## * rename columns in table -------------------------------------------------

  obsev_ren_col(input, vals_data)

  obsev_rename_col(input, vals_data)

  ## * output tables -----------------------------------------------------------

  obsev_show_tab(input, vals_data, output)

  obsev_save_rds(input)

  obsev_go_rds(input, vals_data)

  ## * unique values of each column selected ---------------------------------

  obsev_uni_col(input, vals_data)

  obsev_uniq_col(input, vals_data, output)

  ## * disconnect from db on session end -------------------------------------

  session$onSessionEnded(function() {
    shiny::observe(
      shinySetupPostgreSQL::disc_db_con(input)
    )
  })
}
