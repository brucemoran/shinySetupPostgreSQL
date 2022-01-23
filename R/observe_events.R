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

    CON$cancon <- shinySetupPostgreSQL::test_db_con(INPUT)

    shiny::removeModal()

    CON$extantable <- shinySetupPostgreSQL::test_db_results(INPUT, CON)

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

      shinySetupPostgreSQL::disc_db_con(INPUT)

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

    shinySetupPostgreSQL::validate_user_nt(INPUT)

    OUTPUT$advanced <- shinySetupPostgreSQL::validate_user_cond(INPUT)

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
  shiny::observeEvent(CON$extantabled, ignoreInit=TRUE, {

    ## table read and can be displayed/saved if new data is not to be appended
    vpd <- dplyr::tbl(CON$current, INPUT$con_table)
    VALS_DATA$Data <- dplyr::collect(vpd)

    if(dim(VALS_DATA$Data)[1]>0){
      shinyalert::shinyalert(paste0(INPUT$con_table, " has loaded"),
                            type = "success",
                            showConfirmButton = TRUE)
    } else {
      shinyalert::shinyalert(paste0(INPUT$con_table, " did not load"),
                            type = "error",
                            showConfirmButton = TRUE)
    }
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

    ## * create or append table ------------------------------------------------

    if(is.null(CON$extantable)){

      VALS_DATA$Data <- shinySetupPostgreSQL::parse_input(INPUT)

      shinySetupPostgreSQL::tell_about_load()

    } else {

      shinySetupPostgreSQL::newtable_exists(INPUT)

    }
  })
}

#' Data read
#' @param INPUT session input
#' @param CON connection reactiveVal
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_go_datared
#' @export

obsev_go_datared <- function(INPUT, CON, VALS_DATA){
  print("obsev_go_datared")
  shiny::observeEvent(INPUT$go_datared, {

    shiny::removeModal()

    DBI::dbCreateTable(conn = CON$current,
                       name =  INPUT$con_table,
                       fields = VALS_DATA$Data)

    dplyr::copy_to(dest = CON$current,
                   df = df_copy_to,
                   name = INPUT$con_table,
                   temporary = FALSE,
                   overwrite = TRUE)
  })
}

#' Ask to save data into current table
#' @param INPUT session input
#' @param CON connection reactiveVal
#' @param VALS_DATA data reactiveVal
#' @return NULL
#' @rdname obsev_go_askdata
#' @export

obsev_go_askdata <- function(INPUT, CON, VALS_DATA){
  print("obsev_go_askdata")

  shiny::observeEvent(INPUT$go_askdata, ignoreInit=TRUE, {

      shiny::removeModal()

      ##parse FILENAMES
      vals_new <- shinySetupPostgreSQL::parse_input(INPUT)

      date_cols <- vals_new[,grep("Date_", colnames(vals_new))]
      date_class <- names(table(unlist(lapply(date_cols, class))))

      if(date_class != "Date"){
        dc_list <- lapply(date_cols, function(f){
          delim <- "-"
          if(length(strsplit(unlist(date_cols[1]), "/")[[1]])){
            delim <- "/"
          }
          as.Date(f, format = paste0("%d",delim,"%m",delim,"%Y"))
        })
        vals_new[,grep("Date_", colnames(vals_new))] <- dc_list
      }

      ##should be one of these cols to get Year from
      if("Date_Rec" %in% colnames(vals_new)){
        is_rec <- "Date_Rec"
      } else {
        is_rec <- "Date_Ext_Rec"
      }

      vals_new[,"Year"] <- unlist(lapply(vals_new[,is_rec], function(f){
          format(f, format="%Y")
        }))

      ##combine
      VALS_DATA$Data <- dplyr::bind_rows(VALS_DATA$Data, vals_new)

      df_copy_to <- as.data.frame(VALS_DATA$Data)

      dplyr::copy_to(dest = CON$current,
                     df = df_copy_to,
                     name = INPUT$con_table,
                     temporary = FALSE,
                     overwrite = TRUE)
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

    shinySetupPostgreSQL::sure_to_save(INPUT)

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

    shinySetupPostgreSQL::add_column(INPUT)

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

    shinySetupPostgreSQL::delete_column(VALS_DATA$Data)

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

    shinySetupPostgreSQL::order_column(VALS_DATA$Data)

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

    shinySetupPostgreSQL::rename_column(VALS_DATA$Data)

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

obsev_show_tab <- function(INPUT, VALS_DATA, OUTPUT){
  print("obsev_show_tab")
  shiny::observeEvent(INPUT$show_tab, ignoreInit=TRUE, {

    OUTPUT$maintable1 <- DT::renderDataTable({
        shinySetupPostgreSQL::render_simple_maintable(VALS_DATA$Data)
    })

    OUTPUT$maintable2 <- DT::renderDataTable({
        shinySetupPostgreSQL::render_simple_maintable(VALS_DATA$Data)
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

    shinySetupPostgreSQL::saving_to(INPUT)

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

    shinySetupPostgreSQL::uniq_columns(VALS_DATA$Data)

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

    VALS_DATA$Uniq <- shinySetupPostgreSQL::render_unique_maintable(VALS_DATA$Data[,uniq_col])
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
