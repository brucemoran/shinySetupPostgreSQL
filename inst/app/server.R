# server.R

##based on https://github.com/jienagu/DT-Editor
library(magrittr)
library(RPostgreSQL)
source("../../R/parsing.R")
source("../../R/modals.R")
source("../../R/reactive_observe.R")

function(input, output, session) {

  ## * log in to database ------------------------------------------------------

  ## check credentials exist for Postgresql database, else exit

  shiny::observe(validate_user_nt(input))

  ## test connection to ensure user and connection OK

  shiny::observeEvent(input$userpass, {
      test_db_con(input)
      shiny::removeModal()
  })

  ## allow disconnect as too many open connections (16+)
  ## results in inability to connect!

  shiny::observeEvent(input$disconnex, {
      disc_db_con(input)
  })

  ## allow changing of credentials for database

  shiny::observeEvent(input$db_conxn, {
      connection_change(input)
  })

  ## * take file inputs --------------------------------------------------------

  vals_data <- shiny::reactiveValues()
  con <- shiny::reactiveValues()

  shiny::observeEvent(input$FILENAMES, {
    con$current <- DBI::dbConnect(drv = eval(parse(text = input$con_drv)),
                          host = input$con_host,
                          port = input$con_port,
                          dbname = input$con_dbname,
                          user = input$username,
                          password = input$password)

    con$extantable <- DBI::dbExistsTable(con$current,
                                         input$con_newtable)

    ## * append or create table ------------------------------------------------

    if(con$extantable){

      newtable_exists(input)

      shiny::observeEvent(input$go_askdata, {

        shiny::removeModal()

        vpd <- dplyr::tbl(con$current, input$con_newtable)
        col_vpd <- dplyr::collect(vpd)
        vals_data$Data <- renameParse(col_vpd)

        vals_new <- parse_input(input)

        vals_data$Data <<- rbind(vals_data$Data, vals_new)

        dplyr::copy_to(dest = con$current,
                       df = vals_data$Data,
                       name = input$con_newtable,
                       temporary = FALSE,
                       overwrite = TRUE)
      })

    } else {

      vals_data$Data <- parse_input(input)
      tell_about_load()

      shiny::observeEvent(input$go_datared, {

        shiny::removeModal()

        DBI::dbCreateTable(conn = con$current,
                           name =  input$con_newtable,
                           fields = vals_data$Data)
      })
    }
  })

  ## * output tables -----------------------------------------------------------
  shiny::observeEvent(input$show_table, {
    output$maintable <- DT::renderDataTable({
        render_simple_maintable(input, vals_data$Data)
    })
  })
}
