#_______________________________________________________________________________
# libraries
  library(shiny)
  library(tidyverse)
  library(leaflet)
  library(scales)
  library(plotly)
#_______________________________________________________________________________

#_______________________________________________________________________________
# ui function
fluidPage(
  #_____________________________________________________________________________
  # top row
  fluidRow(
    column(2, br(), "UPAS test app", p()
    ),
    column(3,
         offset = 0,
         fileInput("file1", "", multiple = TRUE, accept = ".txt", width = "100%")
    ),
    column(4, br(),
         downloadButton("downloadData", "export processed data")
    )
  ),
  #_____________________________________________________________________________
  # end top row

  #_____________________________________________________________________________
  # tab panels
  fluidRow(column(10, offset = 1,
  tabsetPanel(type = "tabs", 
  tabPanel("map",
    fluidRow(column(2, offset = 1,
    selectInput("map_color",
                label = "", 
                choices = c("id", "dp", "t", "rh"),
                selected = 1))),
    fluidRow(
      leafletOutput("upasmap", height = 800),
      p()
      )),
  tabPanel("met",
    fluidRow(
    column(1, p())
    #column(2, offset = 1,
    #selectInput("select_id_met", label = "", 
    #  choices = "id_list", 
    #  selected = 1))
    ),
    fluidRow(plotOutput("plot_met", height = "700px"))
    ),
  tabPanel("op",
    fluidRow(
    column(1, p())
    #column(2, offset = 1,
    #selectInput("select_id_op", label = "", 
    #choices = "id_list", 
    #selected = 1))
    ),
    fluidRow(plotlyOutput("plot_op_flow", height = "250px")),
    fluidRow(plotlyOutput("plot_op_vol", height = "250px")),
    fluidRow(plotlyOutput("plot_op_batv", height = "250px"))
    #fluidRow(plotlyOutput("plot_op_batf", height = "200px"))
    ),
  tabPanel("dp",
    fluidRow(
    column(1, p())
    #column(2, offset = 1,
    #selectInput("select_id_pm", label = "", 
    #choices = "id_list", 
    #selected = 1))
    ),
    fluidRow(plotlyOutput("plot_pm", height = "700px"))
    ),
    tabPanel("summary",
      fluidRow(
        column(1, p())
      ),
      fluidRow(plotlyOutput("plot_hist_dp", height = "400px", width = "50%"))
    ),
    tabPanel("about",
             fluidRow(
              column(width = 12,
                     align="left",
                     tags$footer(includeText("text/about.txt"),
                                 style = "padding: 10px;"))),
             fluidRow(
              column(width = 12,
                     align="left",
                     tags$footer(includeText("text/about_2.txt"),
                                 style = "padding: 10px;")))
  )
  )
  )
  )
#_______________________________________________________________________________
# close ui
)
#_______________________________________________________________________________