library(shiny)
library(shinydashboard)
library(chorddiag)

## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Single Consideration", tabName = "single"),
    menuItem("Sunbrust Visualization", tabName = "sunbrust"),
    menuItem("Chord Diagram", tabName = "chord")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "single",
            h2("Single Object Consideration"),
            uiOutput("objects_x"),
            fluidRow(
              box(width = 10, height = 420,
            plotOutput("plot_objects")),
            
              box(width = 2, height = 420,
            plotOutput("fashion_output", height=320, width =320))
            )
            
    ),
    
    tabItem(tabName = "sunbrust",
            h2("Sunbrust Visualization"),
            fluidRow(
              column(10, align="center", 
                     sunburstOutput("sunburst", width = "1200", height = "1200")

        )
            )
    ),
    tabItem(tabName = "chord",
            h2("Chord Visualization"),
            fluidRow(
              column(10, align="center", 
                     chorddiagOutput("char", width = "1200", height = "1200")
              )
              )
            )
    
  )
)

# Put them together into a dashboardPage
dashboardPage(
  skin="red",
  dashboardHeader(title = "Visual Analytics"),
  sidebar,
  body
)
