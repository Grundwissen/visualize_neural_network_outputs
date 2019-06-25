library(shiny)
library(shinydashboard)
library(chorddiag)
library(heatmaply)
library(sunburstR)


## ui.R ##
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Sunbrust Visualization", tabName = "sunbrust"),
    menuItem("Chord Diagram", tabName = "chord"),
    menuItem("Confusion Map", tabName = "heat"),
    menuItem("Bar Charts", tabName = "bar"))
  
)


plotvariable <- "plot_objects"
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "sunbrust",
            h2("Sunbrust Visualization"),
            fluidRow(
              
              box(
                uiOutput("sunburst_auswahl_links"),
                sunburstOutput("sunburst", width = "100%", height = "800")),
              
              box(
                uiOutput("sunburst_auswahl_rechts"),
                sunburstOutput("sunburst2", width = "100%", height = "800"))
              
              
            ),
            fluidRow(
              box(width = 8, height = 5000,
                  uiOutput("plots_sunbrust_links")),
              
              box(width = 4, height = 5000,
                  uiOutput("plotimages_sunburst_links"))
            )
    ),
    tabItem(tabName = "chord",
            h2("Chord Visualization"),
            fluidRow(
              column(10, align="center", 
                     uiOutput("chord_auswahl"),
                     chorddiagOutput("char", width = "1000", height = "1000")
              )
            )
    ),
    tabItem(tabName = "heat",
            h2("Confusion Map"),
            fluidRow(
              
              column(10, align="center", 
                     uiOutput("heatmap_auswahl"),
                     plotlyOutput("heatmap", width = "100%", height="600px")
              ),
              column(10,
                     verbatimTextOutput("click")
              )  
            ),
            fluidRow(
              box(width = 8, height = 5000,
                  uiOutput("plots")),
              
              box(width = 4, height = 5000,
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