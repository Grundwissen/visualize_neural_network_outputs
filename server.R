library(shiny)
library(ggplot2)
library(plyr)
library(sunburstR)
library(lattice)
library(jsonlite)
library(plyr)
library(tidyr)
library(chorddiag)
library(heatmaply)
library(plotly)
library(data.table)

## model 1
## data preparation ##
data <- read_json('test3.json')
data <- data.frame(do.call(rbind,data))
data$label <- unlist(data$label)
data$label2 <- unlist(data$label2)
data$predicted <- unlist(data$predicted)
data$predicted2 <- unlist(data$predicted2)
data$correct <- data$label == data$predicted
data$ID <- seq.int(nrow(data))

## piediagram ##
dataFALSE <- data[data$label != data$predicted,]
data_pie <- count(dataFALSE[,5:6])
data_pie <- data_pie %>% unite(col = Type, label2:predicted2, sep = "-")

## Chorddiagram ##
data_Chord <- dataFALSE[,c("label2","predicted2")]
data_Chord <- count(data_Chord[,1:2])
nameVals <- sort(unique(unlist(data_Chord[1:2])))
myMat <- matrix(0, length(nameVals), length(nameVals), dimnames = list(nameVals, nameVals))
myMat[as.matrix(data_Chord[c("label2", "predicted2")])] <- data_Chord[["freq"]]
myMat <- t(myMat)

## HeatMap ##
data_heat <- data[,c("label2","predicted2")]
data_heat <- count(data_heat[,1:2])

nameValsHeat <- sort(unique(unlist(data_heat[1:2])))
myMatHeat <- matrix(0, length(nameValsHeat), length(nameValsHeat), dimnames = list(nameValsHeat, nameValsHeat))
myMatHeat[as.matrix(data_heat[c("label2", "predicted2")])] <- data_heat[["freq"]]
myMatHeat <- t(myMatHeat)

## model 2
## data preparation ##
data2 <- read_json('model2.json')
data2 <- data.frame(do.call(rbind,data2))
data2$label <- unlist(data2$label)
data2$label2 <- unlist(data2$label2)
data2$predicted <- unlist(data2$predicted)
data2$predicted2 <- unlist(data2$predicted2)
data2$correct <- data2$label == data2$predicted
data2$ID <- seq.int(nrow(data2))

## piediagram ##
dataFALSE2 <- data2[data2$label != data2$predicted,]
data_pie2 <- count(dataFALSE2[,5:6])
data_pie2 <- data_pie2 %>% unite(col = Type, label2:predicted2, sep = "-")

## Chorddiagram ##
data_Chord2 <- dataFALSE2[,c("label2","predicted2")]
data_Chord2 <- count(data_Chord2[,1:2])
nameVals2 <- sort(unique(unlist(data_Chord2[1:2])))
myMat2 <- matrix(0, length(nameVals2), length(nameVals2), dimnames = list(nameVals2, nameVals2))
myMat2[as.matrix(data_Chord2[c("label2", "predicted2")])] <- data_Chord2[["freq"]]

## HeatMap ##
data_heat2 <- data2[,c("label2","predicted2")]
data_heat2 <- count(data_heat2[,1:2])

nameValsHeat2 <- sort(unique(unlist(data_heat2[1:2])))
myMatHeat2 <- matrix(0, length(nameValsHeat2), length(nameValsHeat2), dimnames = list(nameValsHeat2, nameValsHeat2))
myMatHeat2[as.matrix(data_heat2[c("label2", "predicted2")])] <- data_heat2[["freq"]]
myMatHeat2 <- t(myMatHeat2)


datapielist <- list(data_pie, data_pie2)


library(caret)
library(e1071)
mat <- confusionMatrix(factor(data$predicted2),factor(data$label2))
mat_by_label <- data.frame(mat$byClass)
ov <- data.frame(mat$overall)
df <- subset(mat_by_label, select = c('Balanced.Accuracy'))
# mod1 und mod2 als kopie der vorhandenen daten einfügen 
# muss bei mehreren modellen mit cbind gemacht werden
ov$mod1 <- ov$mat.overall
df$mod1 <- df$Balanced.Accuracy

mat2 <- confusionMatrix(factor(data2$predicted2),factor(data2$label2))
mat_by_label2 <- data.frame(mat2$byClass)
ov2 <- data.frame(mat2$overall)
df2 <- subset(mat_by_label2, select = c('Balanced.Accuracy'))

ov$mod2 <- ov2$mat2.overall
df$mod2 <- df2$Balanced.Accuracy


#### dev area ####

shinyServer(function(input, output) {
  
  output$objects_x <- renderUI({
    selectInput("TrainingObject", "Choose Training Objects:", as.list(data$ID)) 
  })
  
  y <- reactive({unlist(data[input$TrainingObject,]$array)})
  output$plot_objects <- renderPlot({
    
  barplot(y(), names.arg = c("Class 0", "Class 1",  "Class 2",  "Class 3",  "Class 4",  "Class 5",  "Class 6",  "Class 7",  "Class 8",  "Class 9"))
    
  })
  
  array_image <- reactive({
    yourMatrix <- matrix(unlist(data[input$TrainingObject,]$image), nrow = 28, ncol = 28)
    yourMatrix <- apply(yourMatrix, 1, rev)
    fashion_image <- image(1:28, 1:28, t(yourMatrix), col = gray(seq(0, 1, length = 256)))
  })
  
  ####dynamic bar plots ####
  
  output$fashion_output <- renderPlot({
    array_image()})
  
  output$plots <- renderUI({
    plot_output_list <- lapply(1:nrow(reacs()), function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname)
    })   
    do.call(tagList, plot_output_list)
  })
  
  observe({
    lapply(1:nrow(reacs()), function(i){
      output[[paste("plot", i, sep="") ]] <- renderPlot({
        barplot(unlist(reacs()[i,]$array), names.arg = c("Class 0", "Class 1",  "Class 2",  "Class 3",  "Class 4",  "Class 5",  "Class 6",  "Class 7",  "Class 8",  "Class 9"))
      })
    })
  })
  
  ##### dynamic image #####
  output$plotimages <- renderUI({
    plot_output_list <- lapply(1:nrow(reacs()), function(i) {
      plotname <- paste("plotimage", i, sep="")
      plotOutput(plotname)
    })   
    do.call(tagList, plot_output_list)
  })
  
  
  observe({
    lapply(1:nrow(reacs()), function(i){
      output[[paste("plotimage", i, sep="") ]] <- renderPlot({
        yourMatrix <- matrix(unlist(reacs()[i,]$image), nrow = 28, ncol = 28)
        yourMatrix <- apply(yourMatrix, 1, rev)
        image(1:28, 1:28, t(yourMatrix), col = gray(seq(0, 1, length = 256)))})
    })
    }
)
  #### sunburst output links #### 
  
  output$sunburst_auswahl_links <- renderUI({
    selectInput("auswahl_sunburst_links", "Model Auswahl:", c(1,2,3)) 
  })
  
  sunbrylinks <- reactive({sunburst(datapielist[as.integer(input$auswahl_sunburst_links)][[1]], count =  TRUE, legend = list(w = 150,h = 50, s = 15 , t = 1 ), breadcrumb = list(w = 150,h = 75, s = 15 , t = 10))})
  #addedd add_shiny
  output$sunburst <- renderSunburst(add_shiny({sunbrylinks()}))
  
  # test sunburst click left
  selection_li <- reactive({
    input$sunburst_click
  })
  
  
  output$sunburstclicked_li <- renderPrint({
    selection_li()
  })
  
  
  #### sunburst output rechts ####
  
  output$sunburst_auswahl_rechts <- renderUI({
    selectInput("auswahl_sunburst_rechts", "Model Auswahl:", c(1,2,3)) 
  })
  
  sunbryrechts <- reactive({sunburst(datapielist[as.integer(input$auswahl_sunburst_rechts)][[1]], count =  TRUE, legend = list(w = 150,h = 50, s = 15 , t = 1 ), breadcrumb = list(w = 150,h = 75, s = 15 , t = 10))})
  # added add_shiny 
  output$sunburst2 <- renderSunburst(add_shiny({sunbryrechts()}))
  #test sunburst click right 
  selection_re <- reactive({
    input$sunburst2_click
  })
  
  
  output$sunburstclicked_re <- renderPrint({
    selection_re()
  })
  
  #### weitere outputs
  
  output$char <- renderChorddiag({chorddiag(myMat, type = "bipartite", showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)}) 
  
  output$heatmap <- renderPlotly({heatmaply(myMatHeat, cellnote = myMatHeat,  dendrogram = 'none', plot_method = 'plotly', column_text_angle = 0,label_names = c("predicted","class","count")) %>% layout(xaxis = list(position = 1, side = "top"))})
  
  #output$click <- renderPrint({
  #  d <- event_data("plotly_click")
  #  if (is.null(d)) "Click on Class" else d$x
  #})
  
  # renderPrint für get_label funktion
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    d2 <- get_labels(d$x,d$y)
    if (is.null(d)) "Click on Class" else d2
  })
  
  # functions for Heatmap/Confusion matrix
  get_labels  <- function(x,y){
    row_labels <- rev(as.vector(rownames(myMatHeat)))
    col_labels <- colnames(myMatHeat)
    return(c(col_labels[x],row_labels[y]))
  }
  #rauskommentiert da fehler produziert 
  #labels = get_labels(d[x],d[y])
  #frame_heat = subset(data, label2 == labels[1] & predicted2 == labels[2])
  
  ### bardiagram allg.
  
  output$barchart_allg <- renderPlotly({plot_ly(ov, x = row.names(ov), y = ~mod1, type = 'bar', name = 'mod1')%>% add_trace(y = ~mod2, name = 'mod2')%>% layout(yaxis = list(title = 'Value') , barmode ='group')})
  #testing click on bar charts
  output$barcklick1 <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on Class" else d
  })
  
  output$barchart_allg2 <- renderPlotly({plot_ly(df, x = row.names(df), y = ~mod1, type = 'bar', name = 'mod1')%>% add_trace(y = ~mod2, name = 'mod2')%>% layout(yaxis = list(title = 'Balanced accuracy') , barmode ='group')})
  

    reacs <- reactive({
    d <- event_data("plotly_click")
    labels <- get_labels(d$x,d$y)
    frame_heat <- subset(data, label2 == labels[1] & predicted2 == labels[2])
    })

  
    
  }
)
