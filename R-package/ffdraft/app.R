#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)

source("./Untitled.R")

appdata <-
  readr::read_rds("../data/processed/appdata_DU.rds") %>%
  as.data.frame()

dash_header <-
  shinydashboard::dashboardHeader(title = "Basic dashboard")

dash_sidebar <-
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Dataset",
        tabName = "dataset",
        icon = icon("dashboard")
      )
    )
  )

tab_dataset <- shinydashboard::tabItem(
  tabName = "dataset",

  # Multi-variable select input
  fluidRow(shinydashboard::box(
    title = h3("Select columns to show: "),
    selectInput(
      "selectedColumns",
      label = NULL,
      choices = colnames(appdata),
      multiple = TRUE,
      selectize = TRUE,
      selected = c(
        "player",
        "pos",
        "rank",
        "points",
        "points_vor"
      )
    ),
    width = 12
  )),

  fluidRow(
    column(
      radioButtons(
        "avg_type",
        label = h3("Average Type"),
        choices = list(
          "Average" = "average",
          "Robust" = "robust",
          "Weighted" = "weighted"
        ),
        selected = "weighted"
      ),
      width = 3
    )
  ),

  # Display dataset using DataTable HTML widget
  fluidRow(shinydashboard::box(DT::DTOutput("playerTable"),
    width = 12
  ))
)

dash_body <-
  shinydashboard::dashboardBody(shinydashboard::tabItems(tab_dataset))

# dashboardPage -----------------------------------------------------------
ui <-
  shinydashboard::dashboardPage(
    header = dash_header,
    sidebar = dash_sidebar,
    body = dash_body
  )

server <- function(input, output, session) {
  
  filteredTable <- reactive({
    appdata %>%
      dplyr::arrange(rank) %>%
      dplyr::filter(
        avg_type == input$avg_type
      ) %>%
      dplyr::select(one_of(input$selectedColumns))
  })

  output$playerTable <- DT::renderDT({
    DT::datatable(
      filteredTable(),
      filter = "top",
      selection = "none",
      editable = T
    ) %>%
      DT::formatRound(intersect(
        input$selectedColumns,
        c(
          "points",
          "drop_off",
          "sd_pts",
          "points_vor",
          "floor",
          "ceiling",
          "ecr",
          "adp",
          "risk",
          "adp_diff"
        )
      ),
      digits = 2
      )
  })

  proxy <- DT::dataTableProxy("playerTable")

  observeEvent(input$playerTable_cell_edit, {
    info <- input$playerTable_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    appdata[i, j] <<- DT::coerceValue(v, appdata[i, j])
    DT::replaceData(proxy, appdata, resetPaging = FALSE) # important
  })

}

shinyApp(ui, server)
