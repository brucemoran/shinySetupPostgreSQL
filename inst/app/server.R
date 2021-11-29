# server.R

##based on https://github.com/jienagu/DT-Editor
library(magrittr)
source("../../R/parsing.R")
source("../../R/modals.R")
source("../../R/reactive_observe.R")

function(input, output, session) {

  ## check credentials exist for Postgresql database, else exit

  shiny::observe(validate_user_nt(input))

  shiny::observeEvent(input$userpass, {
      test_db_con(input)
      shiny::removeModal()

  })

  ##allow disconnect as too many connections results in inability to connect!

  shiny::observeEvent(input$disconnex, {
      disc_db_con(input)
  })

  ### allow changing of credentials for database

  shiny::observeEvent(input$db_conxn, {
      connection_change(input)
  })

  ## * log in to database ------------------------------------------------------

  vals_data <- shiny::reactiveValues()
  con <- shiny::reactiveValues()

  shiny::observeEvent(input$FILENAMES, {
    con$current <- DBI::dbConnect(drv = eval(parse(text = input$con_drv)),
                          host = input$con_host,
                          port = input$con_port,
                          dbname = input$con_dbname,
                          user = input$username,
                          password = input$password)

    print(con$current)
    con$extantable <- DBI::dbExistsTable(con$current,
                                         input$con_newtable)
    print(con$extantable)

    ## * allow data input --------------------------------------------------------

    if(con$extantable){

      print("Table exists")
      newtable_exists(input)

      ##asked if OK to store

      shiny::observeEvent(input$go_askdata, {

        shiny::removeModal()
        vpd <- dplyr::tbl(con$current, input$con_newtable)
        vals_data$Data <- dplyr::collect(vpd) %>%
                          renameParse()

        print("inputData")
        vals_new <- inputData(input)

        print("rbind Datas")
        vals_data$Data <<- rbind(vals_data$Data, vals_new)

        print("copy_to")
        dplyr::copy_to(dest = con$current,
                       df = vals_data$Data,
                       name = input$con_newtable,
                       temporary = FALSE,
                       overwrite = TRUE)
      })

    } else {

      print("Table doesn't exist")

      vals_data$Data <- inputData(input)
      tell_about_load()

      print(head(vals_data$Data))

      ##create table
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
