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
library(ffanalytics)
library(Rglpk)

source("./Untitled.R")

appdata <- load_appdata() %>%
  add_vor() %>%
  add_costs()
# readr::read_rds("../data/processed/appdata.rds")

leagueParticipants <-
  c(
    "SHOW",
    "BMB",
    "FU",
    "DIPS",
    "LAD",
    "ABIB",
    "GIBA",
    "ET",
    "ADH",
    "WALK",
    "BBRC",
    "KENN"
  )

dash_header <-
  shinydashboard::dashboardHeader(title = "Basic dashboard")

dash_sidebar <-
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Dataset",
        tabName = "dataset",
        icon = icon("dashboard")
      )
    ),
    selectizeInput(
      "draftPlayer",
      "Player",
      choices = unique(appdata$`Player (Team)`),
      options = list(
        placeholder = "Select a player",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    selectizeInput(
      "selectOwner",
      "Owner",
      choices = leagueParticipants,
      options = list(
        placeholder = "Select a owner",
        onInitialize = I('function() { this.setValue(""); }')
      )
    ),
    numericInput(
      "draftPaid",
      "Amount Paid",
      value = 1,
      min = 1,
      max = 99
    ),
    actionButton("draftSubmit", "Submit")
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
        "Rank",
        "owner",
        "paid",
        "Player (Team)",
        "Pos",
        "Points",
        "VOR",
        "VOR_static",
        "Cost",
        "Cost_Floor",
        "Cost_Ceiling",
        "Cost_static"
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
    ),
    column(
      radioButtons(
        "floorCeiling",
        label = h3("Estimate type"),
        choices = list(
          "Points" = "neither",
          "Floor" = "floor",
          "Ceiling" = "ceiling",
          "Static" = "static"
        ),
        selected = "static"
      ),
      width = 3
    ),
    column(
      numericInput(
        "maxRisk",
        "Maximum Risk",
        value = 10,
        min = 1,
        max = 99
      )
      ,
      width = 3
    ),
    column(
      actionButton("saveDrafted", "Save Drafted"),
      actionButton("loadDrafted", "Load Drafted"),
      width = 3
    )
  ),

  fluidRow(shinydashboard::box(DT::DTOutput("optimalTeam"),
    width = 12
  )),

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
  
  observe({
    
    filteredTable <- appdata %>%
      dplyr::arrange(Rank) %>%
      dplyr::filter(
        avg_type == input$avg_type,
        Risk <= input$maxRisk
      ) %>%
      dplyr::select(one_of(input$selectedColumns))
    
    optimalTeam <- appdata %>%
      dplyr::filter(
        avg_type == input$avg_type,
        Risk <= input$maxRisk
      ) %>%
      optimizeLineup(
        .owner = input$ownerLineup,
        .floorCeiling = input$floorCeiling
      ) %>%
      dplyr::select(
        `Player (Team)`,
        Pos,
        Points,
        Floor,
        Ceiling,
        VOR,
        Cost,
        Cost_Ceiling,
        Cost_Floor,
        Cost_static,
        paid,
        AAV,
        Risk
      )
    
    output$optimalTeam <- DT::renderDT({
      DT::datatable(optimalTeam, options = list(
        lengthMenu = list(c(5, 15, -1), c("5", "15", "All")),
        pageLength = -1
      )) %>%
        DT::formatRound(c(
          "Points",
          "VOR",
          "Floor",
          "Ceiling",
          "Risk"
        ),
        digits = 2
        ) %>%
        DT::formatCurrency(c("Cost", "Cost_Ceiling", "Cost_Floor", "Cost_static", "paid", "AAV"),
                           digits = 0
        )
    })
    
    output$playerTable <- DT::renderDT({
      DT::datatable(
        filteredTable,
        filter = "top",
        options = list(
          scrollX = TRUE,
          scrollY = TRUE,
          paging = TRUE
        ),
        selection = "none",
        editable = T
      ) %>%
        DT::formatRound(intersect(
          input$selectedColumns,
          c(
            "Points",
            "Drop off",
            "sdPts",
            "VOR",
            "Floor",
            "Ceiling",
            "Points",
            "Floor",
            "Ceiling",
            "ECR",
            "ADP",
            "Risk",
            "ADP Diff"
          )
        ),
        digits = 2
        ) %>%
        DT::formatCurrency(intersect(
          input$selectedColumns,
          c("Cost", "Cost_Ceiling", "Cost_Floor", "Cost_static", "paid", "AAV")
        ),
        digits = 0
        )
    })
  })
  
  proxy <- DT::dataTableProxy("playerTable")

  observeEvent(input$playerTable_cell_edit, {
    info <- input$playerTable_cell_edit
    str(info)
    i <- info$row
    j <- info$col
    v <- info$value
    appdata[i, j] <<- DT::coerceValue(v, unlist(appdata[i, j]))
    replaceData(proxy, appdata, resetPaging = FALSE) # important
  })

  observeEvent(input$draftSubmit, {
    
    appdata <<-
      draftPlayer(
        appdata,
        .playerName = input$draftPlayer,
        .owner = input$selectOwner,
        .paid = input$draftPaid
      ) %>%
      add_vor() %>%
      add_costs()
    
    undraftedPlayers <- appdata %>%
      dplyr::filter(is.na(owner)) %>%
      dplyr::distinct(as.character(`Player (Team)`)) %>%
      unlist()
    
    names(undraftedPlayers) <- undraftedPlayers

    updateSelectizeInput(
      session,
      "draftPlayer",
      choices = undraftedPlayers,
      selected = input$selectedGroupingCols,
      options = list(
        placeholder = "Select a owner",
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })

  observeEvent(input$saveDrafted, {
    appdata %>%
      readr::write_rds("../data/interim/appdata_saved.rds")
  })

  observeEvent(input$loadDrafted, {
    appdata <<-
      readr::read_rds("../data/interim/appdata_saved.rds")
  })
}

shinyApp(ui, server)
