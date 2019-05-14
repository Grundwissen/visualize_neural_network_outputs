library(shiny)

data <- jsonlite::read_json('x.json')
data <- data.frame(do.call(rbind,data))
data$label <- unlist(data$label)
data$predicted <- unlist(data$predicted)
data$correct <- data$label == data$predicted
data$ID <- seq.int(nrow(data))


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
  
  output$objects_x <- renderUI({
    selectInput("TrainingObject", "Choose Training Objects:", as.list(data$ID)) 
  })
  
  y <- reactive({unlist(data[input$TrainingObject,]$array, use.names=FALSE)})
  
  output$plot_objects <- renderPlot({plot(y())})
  
}
)
