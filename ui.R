library(shiny)
library(shinydashboard)
library(chorddiag)
library(heatmaply)


## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Single Consideration", tabName = "single"),
    menuItem("Sunbrust Visualization", tabName = "sunbrust"),
    menuItem("Chord Diagram", tabName = "chord"),
    menuItem("HeatMap", tabName = "heat")
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
              #column(10, align="center",
              #       sunburstOutput("sunburst", width = "1200", height = "1200")
              
              box(
                uiOutput("sunburst_auswahl_links"),
                sunburstOutput("sunburst", width = "1200", height = "1200")),
              
              box(
                uiOutput("sunburst_auswahl_rechts"),
                sunburstOutput("sunburst2", width = "1200", height = "1200"))
        
            )
    ),
    tabItem(tabName = "chord",
            h2("Chord Visualization"),
            fluidRow(
              column(10, align="center", 
                     chorddiagOutput("char", width = "1200", height = "1200")
              )
              )
      ),
    tabItem(tabName = "heat",
            h2("Heat Map"),
            fluidRow(
              column(10, align="center", 
                     plotlyOutput("heatmap", width = "100%", height="600px")
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
