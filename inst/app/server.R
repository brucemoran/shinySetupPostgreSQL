# server.R

function(input, output, session) {

  ## * log in to database ------------------------------------------------------

  ## check credentials exist for Postgresql database, else exit

  shiny::observe( shinySetupPostgreSQL::validate_user_nt(input) )

  output$advanced <- shinySetupPostgreSQL::validate_user_cond(input)

  shiny::outputOptions(output, "advanced", suspendWhenHidden = FALSE)

  ## test connection to ensure user and connection OK, and connect

  con <- shiny::reactiveValues()

  shiny::observeEvent(input$userpass, {

    con$cancon <- shinySetupPostgreSQL::test_db_con(input)

    shiny::removeModal()

    con$extantable <- shinySetupPostgreSQL::test_db_results(input, con)

    if(con$extantable){
      con$extantabled <- 1
      con$data_dir <- DBI::dbGetQuery(conn = con$current,
                                      statement = "SHOW data_directory;")
    }
  })

  ## allow disconnect as too many open connections (16+)
  ## results in inability to connect!

  shiny::observeEvent(input$disconnex, {

      shinySetupPostgreSQL::disc_db_con(input)

  })

  ## allow changing of credentials for database

  shiny::observeEvent(input$db_conxn, {

    shinySetupPostgreSQL::validate_user_nt(input)

    output$advanced <- shinySetupPostgreSQL::validate_user_cond(input)

    shiny::outputOptions(output, "advanced", suspendWhenHidden = FALSE)
  })

  ## * load table that already exists ------------------------------------------

  vals_data <- shiny::reactiveValues()

  shiny::observeEvent(con$extantabled, ignoreInit=TRUE, {

    ## table read and can be displayed/saved if new data is not to be appended
    vpd <- dplyr::tbl(con$current, input$con_table)
    vals_data$Data <- dplyr::collect(vpd)

    if(dim(vals_data$Data)[1]>0){
      shinyalert::shinyalert(paste0(input$con_table, " has loaded"),
                            type = "success",
                            showConfirmButton = TRUE)
    } else {
      shinyalert::shinyalert(paste0(input$con_table, " did not load"),
                            type = "error",
                            showConfirmButton = TRUE)
    }
  })

  ## * take file inputs --------------------------------------------------------

  shiny::observeEvent(input$FILENAMES, ignoreInit=TRUE, {

    ## * append or create table ------------------------------------------------

    if(is.null(con$extantable)){

      vals_data$Data <- shinySetupPostgreSQL::parse_input(input)

      shinySetupPostgreSQL::tell_about_load()

    } else {

      shinySetupPostgreSQL::newtable_exists(input)

    }
  })

  shiny::observeEvent(input$go_datared, {

    shiny::removeModal()

    DBI::dbCreateTable(conn = con$current,
                       name =  input$con_table,
                       fields = vals_data$Data)

    dplyr::copy_to(dest = con$current,
                   df = df_copy_to,
                   name = input$con_table,
                   temporary = FALSE,
                   overwrite = TRUE)
  })

  ## * overwrite table with input ----------------------------------------------

  shiny::observeEvent(input$go_askdata, ignoreInit=TRUE, {

      shiny::removeModal()

      vals_new <- shinySetupPostgreSQL::parse_input(input)

      vals_data$Data <<- rbind(vals_data$Data, vals_new)

      dplyr::copy_to(dest = con$current,
                     df = vals_data$Df,
                     name = input$con_table,
                     temporary = FALSE,
                     overwrite = TRUE)
  })

  ## * save current table ------------------------------------------------------

  shiny::observeEvent(input$save_tab, ignoreInit=TRUE, {

    shinySetupPostgreSQL::sure_to_save(input)

  })

  shiny::observeEvent(input$go_save, {

    df_copy_to <- as.data.frame(vals_data$Data)
    dplyr::copy_to(dest = con$current,
                   df = df_copy_to,
                   name = input$save_tab_name,
                   temporary = FALSE,
                   overwrite = TRUE)
    shiny::removeModal()
  })

  ## * add columns to table ----------------------------------------------------

  shiny::observeEvent(input$add_col, ignoreInit = TRUE, {

    shinySetupPostgreSQL::add_column(input)

  })

  shiny::observeEvent(input$ins_col, ignoreInit = TRUE, ignoreNULL = TRUE, {

    if(input$new_col %in% colnames(vals_data$Data)){

      shiny::removeModal()
      shinyalert::shinyalert("Column Already Exists",
                             type = "error",
                             showConfirmButton = TRUE)
    } else {

      shiny::removeModal()
      vals_data$Data <- tibble::add_column(.data = vals_data$Data,
                                           !!input$new_col := NA)

    }
  })

  ## * remove empty columns from table -----------------------------------------

  shiny::observeEvent(input$del_col, ignoreInit=TRUE, {

    shinySetupPostgreSQL::delete_column(vals_data$Data)

  })

  shiny::observeEvent(input$col_del, ignoreNULL = TRUE, {

    check_empty <- unique(unlist(vals_data$Data[[input$col_to_del]]))

    if(!is.na(check_empty)){

      shiny::removeModal()
      shinyalert::shinyalert("Column to be Deleted is not empty",
                             type = "error",
                             showConfirmButton = TRUE)
    } else {

      shiny::removeModal()
      vals_data$Data <- dplyr::select(.data = vals_data$Data, -!!input$col_to_del)
      shinyalert::shinyalert("Column Deleted",
                             type = "success",
                             showConfirmButton = TRUE)
    }
  })

  ## * reorder columns in table ------------------------------------------------

  shiny::observeEvent(input$ord_col, ignoreInit=TRUE, {

    shinySetupPostgreSQL::order_column(vals_data$Data)

  })

  shiny::observeEvent(input$arr_col, ignoreInit=TRUE, {

    reorder_cols <- unlist(lapply(seq_along(colnames(vals_data$Data)), function(f){
      colnf <- paste0("column_", f)
      return(input[[colnf]])
    }))

    shiny::removeModal()
    vals_data$Data <- dplyr::select(.data = vals_data$Data, !!reorder_cols)

  })

  ## * rename columns in table -------------------------------------------------

  shiny::observeEvent(input$ren_col, ignoreInit=TRUE, {

    shinySetupPostgreSQL::rename_column(vals_data$Data)

  })

  shiny::observeEvent(input$rename_col, ignoreInit=TRUE, {

    vals_data$Data <- dplyr::rename(.data = vals_data$Data,
                                    !!input$new_colname := !!input$col_to_rename)
    shiny::removeModal()

  })

  ## * output tables -----------------------------------------------------------

  shiny::observeEvent(input$show_tab, ignoreInit=TRUE, {

    output$maintable1 <- DT::renderDataTable({
        shinySetupPostgreSQL::render_simple_maintable(vals_data$Data)
    })

    output$maintable2 <- DT::renderDataTable({
        shinySetupPostgreSQL::render_simple_maintable(vals_data$Data)
    })
  })

  shiny::observeEvent(input$save_rds, ignoreInit=TRUE, {

    shinySetupPostgreSQL::saving_to(input)

  })

  shiny::observeEvent(input$go_rds, {

    shiny::removeModal()
    save_file <- paste0(con$data_dir, "/", input$con_table, ".", Sys.Date(), ".rds")

    saveRDS(object = vals_data$Data,
            file = save_file)

    shinyalert::shinyalert("Data Saved",
                           type = "success",
                           showConfirmButton = TRUE)
  })

  ## * unique values of each column selected ---------------------------------

  shiny::observeEvent(input$uni_col, ignoreInit=TRUE, {

    shinySetupPostgreSQL::uniq_columns(vals_data$Data)

  })

  shiny::observeEvent(input$uniq_col, ignoreInit=TRUE, {

    ##vector of yes or no
    uniq_col <- unlist(lapply(seq_along(colnames(vals_data$Data)), function(f){
      if(input[[paste0("ucolumn_", f)]] == "Yes"){
        return(f)
      }
    }))

    shiny::removeModal()

    vals_data$Uniq <- shinySetupPostgreSQL::render_unique_maintable(vals_data$Data[,uniq_col])
    output$uniqtable <- DT::renderDataTable({
      vals_data$Uniq
    })

    output$download_uniq <- shiny::downloadHandler(
      filename <- function() {
        paste0(con$data_dir, "/", input$con_table, ".uniq.", Sys.Date(), ".csv")
      },
      content <- function(con) {
        write.csv(vals_data$Uniq, con)
      }
    )
  })

  ## * disconnect from db on session end -------------------------------------
  session$onSessionEnded(function() {
    shiny::observe(
      shinySetupPostgreSQL::disc_db_con(input)
    )
  })
}
