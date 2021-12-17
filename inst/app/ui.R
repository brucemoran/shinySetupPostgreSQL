# ui.R

shiny::fluidPage(
  shinyalert::useShinyalert(),
  shinyjs::useShinyjs(),
  shiny::headerPanel("Shiny Setup PostgreSQL Tables"),
  shiny::tabsetPanel(
    shiny::tabPanel("Setup",
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 9,
          shiny::fluidRow(
             splitLayout(cellWidths = c("40%", "15%", "15%", "15%", "15%", "15%"),
                shiny::fileInput(
                  inputId = "FILENAMES",
                  label = NULL,
                  multiple = TRUE,
                  accept = c(".xlsx", ".rds"),
                  buttonLabel = "Select  input...",
                  placeholder = NULL),
                shiny::actionButton("save_rds", icon = icon("save"), label = "Save RDS"),
                shiny::actionButton("show_tab", icon = icon("list"), label = "Show Table"),
                shiny::actionButton("save_tab", icon = icon("save"), label = "Save Table"),
                shiny::actionButton("db_conxn", icon = icon("lightbulb"), label = "New Conxn")
              ),
          )
        ),
        shiny::mainPanel(fluid = TRUE, DT::dataTableOutput('maintable1'))
      ),
    ),
    shiny::tabPanel("Edit",
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 9,
          shiny::fluidRow(
            shiny::actionButton("add_col", icon = icon("plus"), label = "Add Column"),
            shiny::actionButton("ord_col", icon = icon("move"), label = "Order Columns"),
            shiny::actionButton("ren_col", icon = icon("move"), label = "Rename Columns"),
            shiny::actionButton("del_col", icon = icon("move"), label = "Delete Column")
          )
        ),
        shiny::mainPanel(fluid = TRUE, DT::dataTableOutput('maintable2'))
      ),
    ),
    shiny::tabPanel("Values",
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 9,
          shiny::fluidRow(
            shiny::actionButton("uni_col", icon = icon("minus"), label = "Uniquify Column"),
            shiny::downloadButton(outputId = "download_uniq",
                                  label = "Download Unique Table")
          )
        ),
        shiny::mainPanel("Uniq Table", fluid = TRUE,
          DT::dataTableOutput('uniqtable')
        )
      ),
    )
  ),
  shiny::mainPanel()
)
