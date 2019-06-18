library(shiny)
library(shinydashboard)
library(chorddiag)
library(heatmaply)
library(sunburstR)


## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Single Consideration", tabName = "single"),
    menuItem("Sunbrust Visualization", tabName = "sunbrust"),
    menuItem("Chord Diagram", tabName = "chord"),
    menuItem("HeatMap", tabName = "heat"),
    menuItem("Bar Charts", tabName = "bar"))
  
)


plotvariable <- "plot_objects"
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "single",
            h2("Single Object Consideration"),
            uiOutput("objects_x"),
            fluidRow(
              box(width = 10, height = 420,
                  plotOutput(plotvariable)),
              
              box(width = 2, height = 420,
                  plotOutput("fashion_output", height=320, width =320))
              
            )

    ),
    
    tabItem(tabName = "sunbrust",
            h2("Sunbrust Visualization"),
            fluidRow(
              
              box(
                uiOutput("sunburst_auswahl_links"),
                sunburstOutput("sunburst", width = "500", height = "500")),
              
              box(
                uiOutput("sunburst_auswahl_rechts"),
                sunburstOutput("sunburst2", width = "500", height = "500"))
              
              
            ),
            fluidRow(
              
              verbatimTextOutput("sunburstclicked_li")
            ),
            fluidRow(
              verbatimTextOutput("sunburstclicked_re")
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
              ),
              column(10,
                     verbatimTextOutput("click")
              )  
            ),
            fluidRow(
              box(width = 10, height = 5000,
                  uiOutput("plots")),
              
              box(width = 2, height = 5000,
                  uiOutput("plotimages"))
            )
    ),
    tabItem(tabName = "bar",
            h2("Bar Chart"),
            fluidRow(
              column(10, align="center",
                     plotlyOutput("barchart_allg", width = "100%", height="600px"),
                     br(),
                     verbatimTextOutput("barcklick1"),
                     br(),
                     plotlyOutput("barchart_allg2", width = "100%", height="600px")
                     
              )))
    
  )
)

# Put them together into a dashboardPage
dashboardPage(
  skin="red",
  dashboardHeader(title = "Visual Analytics"),
  sidebar,
  body
)
