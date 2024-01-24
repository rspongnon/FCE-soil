install.packages("shiny")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(shiny)

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
  cplots <- function(site) {
    dataset <- FCE_sums %>% 
      filter(SITENAME == site)
    ggplot(data = FCE_sums[FCE_sums$SITENAME == site,],
           mapping = aes(Year,per_C,color = per_C))+
      geom_pointrange(aes(ymin =(per_C - error_C/2), ymax = (per_C +error_C/2)))+
      ylim(0,55)}
  
  cplots("SRS3")
  
  
  nplots <- function(site) {
    dataset <- FCE_sums %>% 
      filter(SITENAME == site)
    ggplot(data = FCE_sums[FCE_sums$SITENAME == site,],
           mapping = aes(Year,per_N,color = per_N))+
      geom_pointrange(aes(ymin =(per_N - error_N/2), ymax = (per_N +error_N/2)))+
      ylim(0,5)}
  
  pplots <- function(site) {
    dataset <- FCE_sums %>% 
      filter(SITENAME == site)
    ggplot(data = dataset,
           mapping = aes(Year,per_P,color = per_P))+
      geom_pointrange(aes(ymin =(per_P - error_P/2), ymax = (per_P +error_P/2)))+
      ylim(0,.15)}
  
  hplots <- function(site) {
    dataset <- FCE_sums %>% 
      filter(SITENAME == site)
    ggplot(data = dataset,
           mapping = aes(Year,per_H,color = per_H))+
      geom_pointrange(aes(ymin =(per_H - error_H/2), ymax = (per_H +error_H/2)))+
      ylim(0,5)}
  
  ratplots <- function(site) {
    dataset <- FCE_sums %>% 
      filter(SITENAME == site)
    ggplot(data = dataset,
           mapping = aes(Year,np_ratio,color = np_ratio))+
      geom_point()+
      ylim(0,250)}
  
  oplots <- function(site) {
    dataset <- FCE_sums %>% 
      filter(SITENAME == site)
    ggplot(data = dataset,
           mapping = aes(Year,per_O,color = per_O))+
      geom_pointrange(aes(ymin =(per_O - error_O/2), ymax = (per_O +error_O/2)))+
      ylim(0,100)}
  
  
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

