library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)

dashboardPage(
  dashboardHeader(
    title = "Cancer Mapping"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map (Entire U.S.)", tabName = "mapall", icon = icon("map-o")),
    selectInput("selected",
                   "Select Item to Display",
                   choice),
    sidebarMenu(
      menuItem("Map (by State)", tabName = "mapstates", icon = icon("map")),
      selectizeInput("statels",
                     "Select State to Display",
                     states))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "mapall",
              h2("Map of Entire U.S."),
              fluidRow(leafletOutput("map")),
              h3("Summaries and Plots"),
              fluidRow(box(tableOutput('alltab')),
                         tabBox(title = "Plots", side = "right",
                           tabPanel("Density Plot", plotlyOutput('dens')),
                           tabPanel("Box Plot", plotlyOutput('boxp'))
                           ))
              ),
      tabItem(tabName = "mapstates",
              splitLayout(
                leafletOutput("mapstate"),
                fluidPage(
                  box(tableOutput('statetab'), width = 15,
                      tabBox(title = "Plots", side = "right",
                             tabPanel("Density Plot", plotlyOutput('sdens')),
                             tabPanel("Box Plot", plotlyOutput('sboxp'))))),
                cellWidths = c(450,600)
              ))
      )
  )
)
