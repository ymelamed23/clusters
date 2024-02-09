library(shiny)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(plotly)
library(janitor)
library(scales)

#load data
offenses = read.csv('offenses.csv')
names(offenses) <- gsub("\\.", " ", names(offenses))
offenses = offenses %>% select(-'X')
#make list of variables
variables = offenses %>% 
  select(-team, -posteam, -Cluster, -season, -champion, -champs, -Cluster_Description, -avg_no_pur_pass_epa, -avg_no_pure_rush_epa) %>% 
  head()

ui <- fluidPage(
  h1("Yonathan Melamed's NFL Offense Clusters"),
  h3("Using K-Means Clustering"),
  theme = shinytheme("flatly"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      checkboxGroupInput(
        'year',
        'Season',
        choices = unique(offenses$season)
      ),
      actionLink("selectall", "Select/Deselect All"),
      selectInput(inputId = "x_attribute", 
                  label   = "select attribute for x axis", 
                  choices = names(variables)),
      selectInput(inputId = "y_attribute", 
                  label   = "select attribute for y axis", 
                  choices = names(variables))),
    mainPanel = mainPanel(
      plotlyOutput("myplot", height = "600px", width = "700px", inline = TRUE)
    )))


server = function(input, output, session){
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session,"year","Season",choices=unique(offenses$season))
    }
    else
    {
      updateCheckboxGroupInput(session,"year","Season",choices=unique(offenses$season),selected=unique(offenses$season))
    }
  })
  
  
  app_plot_data <- reactive({offenses |> filter(season %in% input$year)})
  
  
  output$myplot <- renderPlotly({
    ggplot(data = app_plot_data(), aes(x=.data[[input$x_attribute]], y=.data[[input$y_attribute]], color = factor(Cluster_Description), text=team)) +
      geom_point() +
      theme_classic() +
      theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) + 
      labs(title = 'Offensive Statistics by Cluster', color='Cluster Description', subtitle = 'SB Champions are circled') +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
      geom_point(data=app_plot_data() %>% filter(champs==1),
                 pch=21,
                 size=4)
  })
}

shinyApp(ui = ui, server = server)
