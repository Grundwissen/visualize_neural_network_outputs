library(shiny)
library(ggplot2)
library (plyr)
library(sunburstR)
library(lattice)
library(jsonlite)
library(plyr)
library(tidyr)
library(chorddiag)

data <- read_json('test3.json')
data <- data.frame(do.call(rbind,data))
data$label <- unlist(data$label)
data$label2 <- unlist(data$label2)
data$predicted <- unlist(data$predicted)
data$predicted2 <- unlist(data$predicted2)
data$correct <- data$label == data$predicted
data$ID <- seq.int(nrow(data))

dataFALSE <- data[data$label != data$predicted,]

data_pie <- count(dataFALSE[,5:6])
data_pie <- data_pie %>% unite(col = Type, label2:predicted2, sep = "-")

 data_Chord <- dataFALSE[,c("label2","predicted2")]
 data_Chord <- count(data_Chord[,1:2])
 
 nameVals <- sort(unique(unlist(data_Chord[1:2])))
 myMat <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))
 myMat[as.matrix(data_Chord[c("label2", "predicted2")])] <- data_Chord[["freq"]]
 myMat <- t(myMat)
                         
shinyServer(function(input, output) {
  
  ## sidebar auswahlmenü ##
  output$objects_x <- renderUI({
    selectInput("TrainingObject", "Choose Training Objects:", as.list(data$ID)) 
  })
  
  ## mainpanel plot ##
  y <- reactive({unlist(data[input$TrainingObject,]$array)})
  output$plot_objects <- renderPlot({barplot(y(), names.arg = c("Class 0", "Class 1",  "Class 2",  "Class 3",  "Class 4",  "Class 5",  "Class 6",  "Class 7",  "Class 8",  "Class 9"))})

  
  array_image <- reactive({
    yourMatrix <- matrix(unlist(data[input$TrainingObject,]$image), nrow = 28, ncol = 28)
    yourMatrix <- apply(yourMatrix, 1, rev)
    fashion_image <- image(1:28, 1:28, t(yourMatrix), col = gray(seq(0, 1, length = 256)))
  })
  
  output$fashion_output <- renderPlot({array_image()})
  
  output$sunburst <- renderSunburst({sunburst(data_pie, count =  TRUE, legend = list(w = 150,h = 50, s = 15 , t = 1 ), breadcrumb = list(w = 150,h = 75, s = 15 , t = 10))})
 
  output$char <- renderChorddiag({chorddiag(myMat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)}) 
}
)
