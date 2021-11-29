# ui.R

shiny::fluidPage(
  shinyalert::useShinyalert(),
  shinyjs::useShinyjs(),
  shiny::headerPanel("SVUH Dept. of Pathology Molecular Report Database"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(width=80,
      shiny::actionButton("db_conxn", icon = icon("lightbulb"), label = "Connect as New User", style='margin:8px'),
      shiny::actionButton("show_table", icon = icon("list"), label = "Show Main Table", style='margin:8px')
    ),
    shiny::mainPanel(
      shiny::fileInput(
      inputId = "FILENAMES",
      label = "Select XLSX or RDS files as input to new table",
      multiple = TRUE,
      accept = c(".xlsx", ".rds"),
      width = NULL,
      buttonLabel = "Browse...",
      placeholder = "No file(s) selected")
    )
  ),
  shiny::mainPanel("Main Table", fluid = TRUE,
    DT::dataTableOutput('maintable')
  )
)
