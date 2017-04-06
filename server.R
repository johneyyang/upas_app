#_______________________________________________________________________________
# libraries
  library(shiny)
  library(tidyverse)
  library(leaflet)
  library(scales)
  library(plotly)
#_______________________________________________________________________________

#_______________________________________________________________________________
  source("upas_functions.R")
#_______________________________________________________________________________

#_______________________________________________________________________________
  pdf(file = NULL)
#_______________________________________________________________________________

# server function
function(input, output){
 
#_______________________________________________________________________________
# load data
 datasetInput <- reactive({
 # file info
  inFile <- input$file1
 # null case
  if(is.null(inFile))
   return(NULL)
 # load
  for(i in 1:nrow(inFile)){
   ifelse(i==1,
     upas <- load_upas_file(inFile$datapath[i], inFile$name[i]),
     upas <- rbind(upas, load_upas_file(inFile$datapath[i], inFile$name[i]))
   )
  }
 # process
  upas <- upas_process(upas)
 # return
  return(upas)
 })
#_______________________________________________________________________________

#_______________________________________________________________________________
# output table with id's
  output$view <- renderTable({
    upas <- datasetInput()
    unique(upas$id)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# met data to plot
  met_data <- reactive({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
    data_met(datasetInput())
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# op data to plot
  op_data <- reactive({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
    data_op(datasetInput())
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# pm data to plot
  pm_data <- reactive({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   data_pm(datasetInput())
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# summary data to plot
  summary_data <- reactive({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   datasetInput()
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# plot met
  output$plot_met <- renderPlot({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   p <- plot_met(met_data())
   p
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# plot op flow
  output$plot_op_flow <- renderPlotly({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   p <- plot_op_flow(op_data())
   ggplotly(p)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# plot op vol
  output$plot_op_vol <- renderPlotly({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   p <- plot_op_vol(op_data())
   ggplotly(p)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
  # plot op batv
  output$plot_op_batv <- renderPlotly({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   p <- plot_op_batv(op_data())
   ggplotly(p)
  })
#_______________________________________________________________________________
  
#_______________________________________________________________________________
# plot op batf
  output$plot_op_batf <- renderPlotly({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   p <- plot_op_batf(op_data())
   ggplotly(p)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# plot pm
  output$plot_pm <- renderPlotly({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   p <- plot_pm(pm_data())
   ggplotly(p)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# plot dp hist
  output$plot_hist_dp <- renderPlotly({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   p <- plot_hist_dp(summary_data())
   ggplotly(p)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
# map
  output$upasmap <- renderLeaflet({
   validate(
    need(input$file1 != "", "upload a dataset")
   )
   upas_map(datasetInput(), input$map_color)
  })
#_______________________________________________________________________________

#_______________________________________________________________________________
  output$downloadData <- downloadHandler(
   filename = function() { 
    paste("upas", '.csv', sep = '')
   },
   content = function(file) {
    write.csv(datasetInput(), file)
   }
  )
#_______________________________________________________________________________
#_______________________________________________________________________________
# close server function
}
#_______________________________________________________________________________