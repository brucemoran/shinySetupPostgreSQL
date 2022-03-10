# observe_events.R

#' Test connection credentials and connect, specify if table is extant
#' @param INPUT session input
#' @param CON connection reactiveVal
#' @return NULL
#' @rdname obsev_userpass
#' @export

obsev_userpass <- function(INPUT, CON){
  print("obsev_userpass")
  shiny::observeEvent(INPUT$userpass, {

    CON$cancon <- shinySetupPostgreSQL::mod_test_db_con(INPUT)
    print(CON$cancon)
    shiny::removeModal()

    CON$extantable <- shinySetupPostgreSQL::mod_test_db_results(INPUT, CON)

    if(CON$extantable){
      CON$extantabled <- 1
      CON$data_dir <- DBI::dbGetQuery(conn = CON$current,
                                      statement = "SHOW data_directory;")
    }
  })
}

#' Allow disconnection (useful if too many previous connections and for logoff)
#' @param INPUT session input
#' @return NULL
#' @rdname obsev_disconnex
#' @export

obsev_disconnex <- function(INPUT){
  print("obsev_disconnex")
  shiny::observeEvent(INPUT$disconnex, {

      shinySetupPostgreSQL::mod_disc_db_con(INPUT)

  })
}

#' Allow change of connection credentials
#' @param INPUT session input
#' @param OUTPUT session OUTPUT
#' @return NULL
#' @rdname obsev_db_conxn
#' @export

obsev_db_conxn <- function(INPUT, OUTPUT){
  print("obsev_db_conxn")
  shiny::observeEvent(INPUT$db_conxn, {

    shinySetupPostgreSQL::mod_validate_user_nt(INPUT)

    OUTPUT$advanced <- shinySetupPostgreSQL::mod_validate_user_cond(INPUT)

    shiny::outputOptions(OUTPUT, "advanced", suspendWhenHidden = FALSE)
  })
}

#' Allow change of connection credentials
#' @param INPUT session input
#' @param CON connection reactiveVal
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_extantabled
#' @export

obsev_extantabled <- function(INPUT, CON, VALS_DATA){
  print("obsev_extantabled")
  shiny::observeEvent(CON$extantabled, ignoreInit = TRUE, {

    ## table read and can be displayed/saved if new data is not to be appended
    vpd <- dplyr::tbl(CON$current, INPUT$con_table)

    ##collect into the standard Data reactive
    VALS_DATA$Data <- dplyr::collect(vpd)

    ##if a new table, make the numeric columns numeric for forward compat.
    num_table_cols <- colnames(VALS_DATA$Data)[colnames(VALS_DATA$Data) %in% numeric_table_cols()]
    dat_table_cols <- colnames(VALS_DATA$Data)[colnames(VALS_DATA$Data) %in% date_table_cols()]
    VALS_DATA$Data <- dplyr::mutate(.data = VALS_DATA$Data,
                                    dplyr::across(!!num_table_cols, as.numeric),
                                    dplyr::across(!!dat_table_cols, as.Date))

    ## show table loaded
    shinyalert::shinyalert(paste0(INPUT$con_table, " has loaded"),
                           type = "success",
                           showConfirmButton = TRUE)
    CON$TAT <- TRUE
  })
}

#' Take filename INPUT and parse
#' @param INPUT session input
#' @param CON connection reactiveVal
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_FILENAMES
#' @export

obsev_FILENAMES <- function(INPUT, CON, VALS_DATA){
  print("obsev_FILENAMES")
  shiny::observeEvent(INPUT$FILENAMES, ignoreInit=TRUE, {

    ## loaded data is combined with current table
    ## NB that tables can be empty by specifying unused table name

    shinySetupPostgreSQL::mod_load_data_proceed(INPUT)

  })
}

#' Load data into current table
#' @param INPUT session input
#' @param CON connection reactiveVal
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_go_loaddata
#' @export

obsev_go_loaddata <- function(INPUT, CON, VALS_DATA){
  print("obsev_go_loaddata")

  shiny::observeEvent(INPUT$go_loaddata, ignoreInit=TRUE, {

      shiny::removeModal()

      ##parse FILENAMES
      shinySetupPostgreSQL::parse_input(INPUT, VALS_DATA)

  })
}

#' Save table
#' @param INPUT session input
#' @return NULL
#' @rdname obsev_save_tab
#' @export

obsev_save_tab <- function(INPUT){
  print("obsev_save_tab")
  shiny::observeEvent(INPUT$save_tab, ignoreInit=TRUE, {

    shinySetupPostgreSQL::mod_sure_to_save(INPUT)

  })
}

#' Save table to db
#' @param INPUT session input
#' @param CON connection reactiveVal
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_go_save
#' @export

obsev_go_save <- function(INPUT, CON, VALS_DATA){
  print("obsev_go_save")
  shiny::observeEvent(INPUT$go_save, {

    df_copy_to <- as.data.frame(VALS_DATA$Data)

    dplyr::copy_to(dest = CON$current,
                   df = df_copy_to,
                   name = INPUT$save_tab_name,
                   temporary = FALSE,
                   overwrite = TRUE)
    shiny::removeModal()
  })
}

#' Add column into table
#' @param INPUT session input
#' @return NULL
#' @rdname obsev_add_col
#' @export

obsev_add_col <- function(INPUT){
  print("obsev_add_col")
  shiny::observeEvent(INPUT$add_col, ignoreInit = TRUE, {

    shinySetupPostgreSQL::mod_add_column(INPUT)

  })
}

#' Method to add column into table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_ins_col
#' @export

obsev_ins_col <- function(INPUT, VALS_DATA){
  print("obsev_ins_col")
  shiny::observeEvent(INPUT$ins_col, ignoreInit = TRUE, ignoreNULL = TRUE, {

    if(INPUT$new_col %in% colnames(VALS_DATA$Data)){

      shiny::removeModal()
      shinyalert::shinyalert("Column Already Exists",
                             type = "error",
                             showConfirmButton = TRUE)
    } else {

      shiny::removeModal()
      VALS_DATA$Data <- tibble::add_column(.data = VALS_DATA$Data,
                                           !!INPUT$new_col := NA)

    }
  })
}

#' Delete column into table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_del_col
#' @export

obsev_del_col <- function(INPUT, VALS_DATA){
  print("obsev_del_col")
  shiny::observeEvent(INPUT$del_col, ignoreInit=TRUE, {

    shinySetupPostgreSQL::mod_delete_column(VALS_DATA$Data)

  })
}

#' Method to delete column into table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_col_del
#' @export

obsev_col_del <- function(INPUT, VALS_DATA){
  print("obsev_col_del")
  shiny::observeEvent(INPUT$col_del, ignoreNULL = TRUE, {

    check_empty <- unique(unlist(VALS_DATA$Data[INPUT$col_to_del]))

    if(!is.na(check_empty)){

      shiny::removeModal()
      shinyalert::shinyalert("Column to be Deleted is not empty",
                             type = "error",
                             showConfirmButton = TRUE)
    } else {

      shiny::removeModal()
      VALS_DATA$Data <- dplyr::select(.data = VALS_DATA$Data, -!!INPUT$col_to_del)
      shinyalert::shinyalert("Column Deleted",
                             type = "success",
                             showConfirmButton = TRUE)
    }
  })
}

#' Method to initiate reorder columns in table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_ord_col
#' @export

obsev_ord_col <- function(INPUT, VALS_DATA){
  print("obsev_ord_col")
  shiny::observeEvent(INPUT$ord_col, ignoreInit = TRUE, {

    shinySetupPostgreSQL::mod_order_column(VALS_DATA$Data)

  })
}

#' Method to reorder columns in table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_arr_col
#' @export

obsev_arr_col <- function(INPUT, VALS_DATA){
  print("obsev_arr_col")
  shiny::observeEvent(INPUT$arr_col, ignoreInit=TRUE, {

    reorder_cols <- unlist(lapply(seq_along(colnames(VALS_DATA$Data)), function(f){
      colnf <- paste0("column_", f)
      return(INPUT[[colnf]])
    }))

    shiny::removeModal()
    VALS_DATA$Data <- dplyr::select(.data = VALS_DATA$Data, !!reorder_cols)

  })
}

#' Method to rename columns in table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_ren_col
#' @export

obsev_ren_col <- function(INPUT, VALS_DATA){
  print("obsev_ren_col")
  shiny::observeEvent(INPUT$ren_col, ignoreInit=TRUE, {

    shinySetupPostgreSQL::mod_rename_column(VALS_DATA$Data)

  })
}

#' Reordering columns in table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_rename_col
#' @export

obsev_rename_col <- function(INPUT, VALS_DATA){
  print("obsev_rename_col")
  shiny::observeEvent(INPUT$rename_col, ignoreInit=TRUE, {

    VALS_DATA$Data <- dplyr::rename(.data = VALS_DATA$Data,
                                    !!INPUT$new_colname := !!INPUT$col_to_rename)
    shiny::removeModal()

  })
}

#' Show table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @param OUTPUT session OUTPUT
#' @return NULL
#' @rdname obsev_show_tab
#' @export

obsev_show_tab <- function(VALS_DATA, OUTPUT){
  print("obsev_show_tab")
  shiny::observeEvent(VALS_DATA$Data, ignoreInit=TRUE, {

    OUTPUT$maintable1 <- DT::renderDataTable({
        shinySetupPostgreSQL::ro_render_simple_maintable(VALS_DATA)
    })

    OUTPUT$maintable2 <- DT::renderDataTable({
        shinySetupPostgreSQL::ro_render_simple_maintable(VALS_DATA)
    })
  })
}

#' Save table to RDS
#' @param INPUT session input
#' @return NULL
#' @rdname obsev_save_rds
#' @export

obsev_save_rds <- function(INPUT){
  print("obsev_save_rds")
  shiny::observeEvent(INPUT$save_rds, ignoreInit=TRUE, {

    shinySetupPostgreSQL::mod_saving_to(INPUT)

  })
}

#' Go saving RDS
#' @param INPUT session input
#' @param CON database connection
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_go_rds
#' @export

obsev_go_rds <- function(INPUT, CON, VALS_DATA){
  print("obsev_go_rds")
  shiny::observeEvent(INPUT$go_rds, {

    shiny::removeModal()
    save_file <- paste0(CON$data_dir, "/", INPUT$con_table, ".", Sys.Date(), ".rds")

    saveRDS(object = VALS_DATA$Data,
            file = save_file)

    shinyalert::shinyalert("Data Saved",
                           type = "success",
                           showConfirmButton = TRUE)
  })
}

#' Uniquify columns in table
#' @param INPUT session input
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_uni_col
#' @export

obsev_uni_col <- function(INPUT, VALS_DATA){
  print("obsev_uni_col")
  shiny::observeEvent(INPUT$uni_col, ignoreInit=TRUE, {

    shinySetupPostgreSQL::mod_uniq_columns(VALS_DATA$Data)

  })
}

#' Column data uniquified and output
#' @param INPUT session input
#' @param CON database connection
#' @param VALS_DATA data reactiveVal
#' @param OUTPUT session OUTPUT
#' @return NULL
#' @rdname obsev_uniq_col
#' @export

obsev_uniq_col <- function(INPUT, VALS_DATA, OUTPUT){
  print("obsev_uniq_col")
  shiny::observeEvent(INPUT$uniq_col, ignoreInit=TRUE, {

    ##vector of yes or no
    uniq_col <- unlist(lapply(seq_along(colnames(VALS_DATA$Data)), function(f){
      if(INPUT[[paste0("ucolumn_", f)]] == "Yes"){
        return(f)
      }
    }))

    shiny::removeModal()

    VALS_DATA$Uniq <- shinySetupPostgreSQL::ro_render_unique_maintable(VALS_DATA$Data[,uniq_col])
    OUTPUT$uniqtable <- DT::renderDataTable({
      VALS_DATA$Uniq
    })

    OUTPUT$download_uniq <- shiny::downloadHandler(
      filename <- function() {
        paste0(CON$data_dir, "/", INPUT$con_table, ".uniq.", Sys.Date(), ".csv")
      },
      content <- function(con) {
        write.csv(VALS_DATA$Uniq, con)
      }
    )
  })
}

#' Parse Dates and other Data from VALS_DATA$New and binds to VALS_DATA$Data
#' @param VALS_DATA reactive with $Data element of data (can be empty) and $New (nonempty)
#' @return none, changes VALS_DATA$Data to df with VALS_DATA$New added
#' @rdname obsev_valsdata_new
#' @importFrom magrittr '%>%'
#' @export

obsev_valsdata_new <- function(VALS_DATA){

  ##dates
  shiny::observeEvent(VALS_DATA$New, ignoreNULL = TRUE, ignoreInit = TRUE, {
    print("Parsing VALS_DATA$New")

    vdn <- VALS_DATA$New

    ##find dates and format
    print(as.data.frame(vdn))
    vdn[,grep("Date_|DOB", colnames(vdn))] <- date_as_class(tb = vdn, pattern = "Date_|DOB")

    ##use one of these cols to get Year from when unspecified
    if(length(grep("Date_Requested", colnames(vdn)))>0){
      year_from <- "Date_Requested"
    } else {
      year_from <- "Date_Ext_Rec"
    }
    vdn[,"Year"] <- unlist(lapply(vdn[, year_from], function(f){
        as.character(format(f, format = "%Y"))
      }))

    ##make numeric cols into numeric
    num_table_cols <- colnames(vdn)[colnames(vdn) %in% numeric_table_cols()]
    vdn <- dplyr::mutate(.data = vdn, dplyr::across(!!num_table_cols, as.numeric))

    ##block can be contained in SVUH Lab No.
    vdn <- tidyr::separate(data = vdn,
                           col = "Specimen",
                           into = c("Specimen", "Block"),
                           sep = "[[:space:]]",
                           extra = "merge",
                           fill = "right")

    ##combine
    if(is.null(VALS_DATA$Data)){
      VALS_DATA$Data <- vdn
    } else {
      VALS_DATA$Data <- dplyr::bind_rows(VALS_DATA$Data, vdn)
    }

    VALS_DATA$Data <- as.data.frame(VALS_DATA$Data)
  })
}
