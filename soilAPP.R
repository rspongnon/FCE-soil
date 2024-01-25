library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(shiny)
library(RCurl)

ui <- fluidPage(
  titlePanel("Florida Coastal Everglades soil data"),
  sidebarLayout(
    selectInput(inputId = "site",
                label = "Select site:",
                choices = list("SRS1d",
                               "SRS2",
                               "SRS3",
                               "SRS4",
                               "SRS5",
                               "SRS6",
                               "TS/Ph1",
                               "TS/Ph2",
                               "TS/Ph3",
                               "TS/Ph4",
                               "TS/Ph5",
                               "TS/Ph6",
                               "TS/Ph7",
                               "TS/Ph8",
                               "TS/Ph9",
                               "TS/Ph10",
                               "TS/Ph11")),
    selectInput(inputId = "ele",
                label = "Select element:",
                choices = list("Percent Carbon" ="per_C",
                               "Percent Hydrogen" ="per_H",
                               "Percent Nitrogen" ="per_N",
                               "Percent Phosphorus" ="per_P",
                               "N:P ratio" ="np_ratio",
                               "Percent Organic Matter" = "per_O"))
  ),
  mainPanel(
    plotOutput("Plot"),
    tableOutput("Table")
  )
  
)


server <- (function(input, output) {
  
  output$Plot <- renderPlot({
    if (input$ele =="per_C"){cplots(input$site)}
    else if (input$ele =="per_N"){nplots(input$site)}
    else if (input$ele =="per_P"){pplots(input$site)}
    else if (input$ele =="per_H"){hplots(input$site)}
    else if (input$ele =="np_ratio"){ratplots(input$site)}
    else if (input$ele =="per_O"){oplots(input$site)}
  })
  output$Table <- renderTable({
    (dataset<- FCE_sums[,c(input$site,"Year", input$ele)])
  })
})

shinyApp(ui = ui, server = server)

