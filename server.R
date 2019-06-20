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
library(caret)
library(e1071)

# Colors
groupColors <-c("#7293CB","#84BA5B","#D35E60","#808585","#9067A7","#AB6857","#CCC210","#00008B","#FFB6C1","#E1974C") #Chord
colors = c("#E1974C","#7293CB","#84BA5B","#D35E60","#808585","#9067A7","#AB6857","#CCC210","#00008B","#FFB6C1") #sunburst
barcolors = c("#E1974C","#FFB6C1","#9067A7","#808585","#D35E60","#AB6857","#CCC210","#00008B","#84BA5B","#7293CB") #bars


## model 1
## data preparation ##
data <- read_json('test.json')
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
data2 <- read_json('test2.json')
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


## model 3
## data preparation ##
data3 <- read_json('test3.json')
data3 <- data.frame(do.call(rbind,data3))
data3$label <- unlist(data3$label)
data3$label2 <- unlist(data3$label2)
data3$predicted <- unlist(data3$predicted)
data3$predicted2 <- unlist(data3$predicted2)
data3$correct <- data3$label == data3$predicted
data3$ID <- seq.int(nrow(data3))

## piediagram ##
dataFALSE3 <- data3[data3$label != data3$predicted,]
data_pie3 <- count(dataFALSE3[,5:6])
data_pie3 <- data_pie3 %>% unite(col = Type, label2:predicted2, sep = "-")

## Chorddiagram ##
data_Chord3 <- dataFALSE3[,c("label2","predicted2")]
data_Chord3 <- count(data_Chord3[,1:2])
nameVals3 <- sort(unique(unlist(data_Chord3[1:2])))
myMat3 <- matrix(0, length(nameVals3), length(nameVals3), dimnames = list(nameVals3, nameVals3))
myMat3[as.matrix(data_Chord3[c("label2", "predicted2")])] <- data_Chord3[["freq"]]

## HeatMap ##
data_heat3 <- data3[,c("label2","predicted2")]
data_heat3 <- count(data_heat3[,1:2])

nameValsHeat3 <- sort(unique(unlist(data_heat3[1:2])))
myMatHeat3 <- matrix(0, length(nameValsHeat3), length(nameValsHeat3), dimnames = list(nameValsHeat3, nameValsHeat3))
myMatHeat3[as.matrix(data_heat3[c("label2", "predicted2")])] <- data_heat3[["freq"]]
myMatHeat3 <- t(myMatHeat3)




datapielist <- list(data_pie, data_pie2, data_pie3) # auswahl für sunbursts

heatmap_list <- list(myMatHeat, myMatHeat2, myMatHeat3) # auswahl für heatmap

data_for_heatmap_list <-list(data, data2, data3) # datalist for heatmap auswahl

data_for_chord_list <-list(myMat, myMat2, myMat3)



#Matric Model 1
mat <- confusionMatrix(factor(data$predicted2),factor(data$label2))
mat_by_label <- data.frame(mat$byClass)
ov <- data.frame(mat$overall)
df <- subset(mat_by_label, select = c('Balanced.Accuracy'))
ov$mod1 <- ov$mat.overall
df$mod1 <- df$Balanced.Accuracy


#Matric Model 2
mat2 <- confusionMatrix(factor(data2$predicted2),factor(data2$label2))
mat_by_label2 <- data.frame(mat2$byClass)
ov2 <- data.frame(mat2$overall)
df2 <- subset(mat_by_label2, select = c('Balanced.Accuracy'))
ov$mod2 <- ov2$mat2.overall
df$mod2 <- df2$Balanced.Accuracy


#Matric Model 3
mat3 <- confusionMatrix(factor(data3$predicted2),factor(data3$label2))
mat_by_label3 <- data.frame(mat3$byClass)
ov3 <- data.frame(mat3$overall)
df3 <- subset(mat_by_label2, select = c('Balanced.Accuracy'))
ov$mod3 <- ov3$mat3.overall
df$mod3 <- df3$Balanced.Accuracy


#### Shiny dev area ####

shinyServer(function(input, output) {
  

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
        barplot(unlist(reacs()[i,]$array), col=barcolors, names.arg = c("Tshirt/Top","Trouser","Pullover","Dress","Coat","Sandal","Shirt","Sneaker","Bag","Ankle boot"))
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
        image(1:28, 1:28, t(yourMatrix), col = gray(seq(1, 0, length = 256)))})
    })
    }
)
  #### sunburst output links #### 
  
  output$sunburst_auswahl_links <- renderUI({
    selectInput("auswahl_sunburst_links", "Model Auswahl:", c(1,2,3)) 
  })
  
  sunbrylinks <- reactive({sunburst(datapielist[as.integer(input$auswahl_sunburst_links)][[1]], colors = colors, count =  TRUE, legend = list(w = 150,h = 50, s = 15 , t = 1 ), breadcrumb = list(w = 150,h = 75, s = 15 , t = 10))})
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
  
  sunbryrechts <- reactive({sunburst(datapielist[as.integer(input$auswahl_sunburst_rechts)][[1]], colors = colors ,count =  TRUE, legend = list(w = 150,h = 50, s = 15 , t = 1 ), breadcrumb = list(w = 150,h = 75, s = 15 , t = 10))})
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
  output$chord_auswahl <- renderUI({
    selectInput("chord_auswahl", "Model Auswahl:", c(1,2,3)) 
  })
  
  output$char <- renderChorddiag({chorddiag(data_for_chord_list[as.integer(input$chord_auswahl)][[1]], groupColors = groupColors, showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)}) 
  
  
  output$heatmap_auswahl <- renderUI({
    selectInput("heatmap_auswahl", "Model Auswahl:", c(1,2,3)) 
  })
  
  
  output$heatmap <- renderPlotly({heatmaply(heatmap_list[as.integer(input$heatmap_auswahl)][[1]], cellnote = heatmap_list[as.integer(input$heatmap_auswahl)][[1]],  dendrogram = 'none', plot_method = 'plotly', column_text_angle = 0,label_names = c("predicted","class","count")) %>% layout(xaxis = list(position = 1, side = "top"))})
  
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
  
  output$barchart_allg <- renderPlotly({plot_ly(ov, x = row.names(ov), y = ~mod1, type = 'bar',marker = list(color = 'rgb(158,202,225)'), name = 'mod1')%>% add_trace(y = ~mod2, name = 'mod2',marker = list(color = 'rgb(258,102,125)'))%>% add_trace(y = ~mod3, name = 'mod3',marker = list(color = 'rgb(100,102,125)'))%>% layout(yaxis = list(title = 'Value') , barmode ='group')})
  output$barchart_allg2 <- renderPlotly({plot_ly(df, x = row.names(df), y = ~mod1, type = 'bar',marker = list(color = 'rgb(158,202,225)'), name = 'mod1')%>% add_trace(y = ~mod2, name = 'mod2',marker = list(color = 'rgb(258,102,125)'))%>% add_trace(y = ~mod3, name = 'mod3',marker = list(color = 'rgb(100,102,125)'))%>% layout(yaxis = list(title = 'Balanced accuracy') , barmode ='group')})

  reacs <- reactive({
  d <- event_data("plotly_click")
  labels <- get_labels(d$x,d$y)
  
  print(input$heatmap_auswahl)

  if (is.null(input$heatmap_auswahl)){
    x <- 1
  }
  else{
    x <- input$heatmap_auswahl
  }
  
  frame_heat <- subset(data_for_heatmap_list[[as.integer(x)]], label2 == labels[1] & predicted2 == labels[2])

    })

  reacs_sunburst_links <- reactive({
    d <- selection_li()
    
    print(input$auswahl_sunburst_links)
    
    if (is.null(input$auswahl_sunburst_links)){
      x <- 1
    }
    else{
      x <- input$auswahl_sunburst_links
    }
    
    frame_sunburst_links <- subset(data_for_heatmap_list[[as.integer(x)]], label2 == d[1] & predicted2 == d[2])
    
  })
  
  reacs_sunburst_rechts <- reactive({
    d <- selection_re()
    
    print(input$auswahl_sunburst_rechts)
    
    if (is.null(input$auswahl_sunburst_rechts)){
      x <- 1
    }
    else{
      x <- input$auswahl_sunburst_rechts
    }


    frame_sunburst_rechts <- subset(data_for_heatmap_list[[as.integer(x)]], label2 == d[1] & predicted2 == d[2])
    
  })
  
  output$plots_sunbrust_links <- renderUI({
    plot_output_list <- lapply(1:nrow(reacs_sunburst_links()), function(i) {
      plotname <- paste("plot_sunburst_links", i, sep="")
      plotOutput(plotname)
    })   
    do.call(tagList, plot_output_list)
  })
  
  observe({
    lapply(1:nrow(reacs_sunburst_links()), function(i){
      output[[paste("plot_sunburst_links", i, sep="") ]] <- renderPlot({
        barplot(unlist(reacs_sunburst_links()[i,]$array), col=barcolors, names.arg = c("Tshirt/Top","Trouser","Pullover","Dress","Coat","Sandal","Shirt","Sneaker","Bag","Ankle boot"))
      })
    })
  })
  
  ##### dynamic image #####
  output$plotimages_sunburst_links <- renderUI({
    plot_output_list <- lapply(1:nrow(reacs_sunburst_links()), function(i) {
      plotname <- paste("plotimage_sunburst_links", i, sep="")
      plotOutput(plotname)
    })   
    do.call(tagList, plot_output_list)
  })
  
  
  observe({
    lapply(1:nrow(reacs_sunburst_links()), function(i){
      output[[paste("plotimage_sunburst_links", i, sep="") ]] <- renderPlot({
        yourMatrix <- matrix(unlist(reacs_sunburst_links()[i,]$image), nrow = 28, ncol = 28)
        yourMatrix <- apply(yourMatrix, 1, rev)
        image(1:28, 1:28, t(yourMatrix), col = gray(seq(1, 0, length = 256)))})
    })
  }
  )
  
  ####rechts####
  
  output$plots_sunbrust_links <- renderUI({
    plot_output_list <- lapply(1:nrow(reacs_sunburst_rechts()), function(i) {
      plotname <- paste("plot_sunburst_links", i, sep="")
      plotOutput(plotname)
    })   
    do.call(tagList, plot_output_list)
  })
  
  observe({
    lapply(1:nrow(reacs_sunburst_rechts()), function(i){
      output[[paste("plot_sunburst_links", i, sep="") ]] <- renderPlot({
        barplot(unlist(reacs_sunburst_rechts()[i,]$array), col=barcolors, names.arg = c("Tshirt/Top","Trouser","Pullover","Dress","Coat","Sandal","Shirt","Sneaker","Bag","Ankle boot"))
      })
    })
  })
  
  ##### dynamic image #####
  output$plotimages_sunburst_links <- renderUI({
    plot_output_list <- lapply(1:nrow(reacs_sunburst_rechts()), function(i) {
      plotname <- paste("plotimage_sunburst_links", i, sep="")
      plotOutput(plotname)
    })   
    do.call(tagList, plot_output_list)
  })
  
  
  observe({
    lapply(1:nrow(reacs_sunburst_rechts()), function(i){
      output[[paste("plotimage_sunburst_links", i, sep="") ]] <- renderPlot({
        yourMatrix <- matrix(unlist(reacs_sunburst_rechts()[i,]$image), nrow = 28, ncol = 28)
        yourMatrix <- apply(yourMatrix, 1, rev)
        image(1:28, 1:28, t(yourMatrix), col = gray(seq(1, 0, length = 256)))})
    })
  }
  )
  
  
  }
)
