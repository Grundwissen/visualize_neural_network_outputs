library(shiny)

# Define UI for miles per gallon application
fluidPage(
  
  titlePanel("Visual Analytics Dashboard"),
  sidebarLayout(
  
  sidebarPanel(
      
    uiOutput("objects_x")

      ),
  
  mainPanel(
    plotOutput("plot_objects")
)
)
)
