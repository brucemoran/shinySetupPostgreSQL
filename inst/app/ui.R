# ui.R

shiny::fluidPage(
  shinyalert::useShinyalert(),
  shinyjs::useShinyjs(),
  shiny::headerPanel("SVUH Dept. of Pathology Molecular Report Database"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(width = 9,
      shiny::fluidRow(
         splitLayout(cellWidths = c("40%", "15%", "15%", "15%", "15%"),
            shiny::fileInput(
              inputId = "FILENAMES",
              label = NULL,
              multiple = TRUE,
              accept = c(".xlsx", ".rds"),
              buttonLabel = "Select  input...",
              placeholder = NULL),
            shiny::actionButton("save_rds", icon = icon("save"), label = "Save RDS"),
            shiny::actionButton("load_table", icon = icon("refresh"), label = "Load Table"),
            shiny::actionButton("show_table", icon = icon("list"), label = "Show Table"),
            shiny::actionButton("db_conxn", icon = icon("lightbulb"), label = "New Conxn")
          ),
      )
    ),
    shiny::mainPanel()
    ),
  shiny::mainPanel("Main Table", fluid = TRUE,
    DT::dataTableOutput('maintable')
  )
)
