### open libaries ###
library(haven)
library(tidyr)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(psych)
library(GPArotation)
library(lm.beta)
library(mctest)
library(plotly)
library(dplyr)
library(stringr)
library(rmarkdown)
#ibrary(ingmarkdowntemplates)
library(ggplot2)
#library(ggflags)
library(lubridate)
library(png)
library(ggimage)
library(readxl)
library(tidyxl)
library(data.table)
library(zoo)
library(shiny)
library(corrr)
library(tidyverse)
library(caret)
library(car)
library(gt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
#library(shiny)
#library(shinydashboard)
library(xfun)
library(bslib)
#library(stdlib.h)

source("functions/trend_plot_format_function.R")
source("functions/funnel_plot_format_function.R")
source("functions/time_plot_format_function.R")
source("functions/post_stamp_function.R")
source("functions/post_stamp_function_option2.R")
source("functions/tab_function.R")

# throw these two datasets in the tool folder

# For conversion
try_set <- read_csv("./HL_conv_comb.csv")

# For traffic
try_set2 <- read_csv("./HL_PP_comb.csv")

x <- as.data.frame(try_set)
z <- as.data.frame(try_set2) 


# Define UI for application that draws a histogram
ui <- fluidPage(
  includeCSS("./ing_styles.css"),
  
  # App title ----
  titlePanel(title=div(img(src="ING Identifier_RGB.jpg", height='100px',width='100px'), "Hustlers weekly home loan progress")),
  
  # Sidebar layout with input and output definitions ----
  
  sidebarLayout(
    # Sidebar panel for inputs ----
    
    sidebarPanel(
      tags$style(
        tags$link(rel = "stylesheet", type = "text/css", href = "ing_style.css",
                  ".span8 .well { background-color: #00FFFF; }"),
        tags$style("input {font-family: 'INGMe'; font-size:16px; font-weight:bold;}"),
        tags$style("label {font-family: 'INGMe'; font-size:16px; font-weight:bold;}"),
        tags$style("choices {font-family: 'INGMe'; font-size:14px; font-weight:bold;}"),
        tags$style("buttons {font-family: 'INGMe'; font-size:14px; font-weight:bold;}")
      ),
      # Input: Select the random distribution type ----
      selectInput(inputId= "Month", 
                  label= "Month",
                  choices = unique(x$Month)),
      
      selectInput(inputId = "week", 
                  label = "Week:",
                  choices = unique(x$week)),
      
      # Input: Select the random distribution type ----
      selectInput(inputId = "N2BvsE2B", 
                  label ="N2BvsE2B:",
                  choices = unique(x$N2BvsE2B)),
      
      selectInput(inputId = "Device", 
                  label ="Device:",
                  choices = unique(z$Device)),
      
      # selectInput(inputId = "product", 
      #              label ="Product:",
      #             choices = unique(z$product)),
      
      width =2
    ),
    
    # Main panel for displaying outputs ----
    
    mainPanel(
      tags$header(
        tags$style(rel = "stylesheet", type = "text/css", href = "ing_style.css")
      ),
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "ing_style.css")
        ),
        type = "tabs",
        tabPanel("Traffic sources", plotlyOutput("plot5"),
                 h6("If plot is empty, data not available", align = "left")),
        tabPanel("Traffic", plotlyOutput("plot4"),
                 h6("If plot is empty, data not available", align = "left")),
        tabPanel("Funnel", plotlyOutput("plot1"),
          h6("If plot is empty, data not available", align = "left")),
        tabPanel("Conversion 1", plotlyOutput("plot2"),
                 h6("Conversion 1 == #visits start flow/ #visits product page", align = "left")),
        tabPanel("Conversion 2", plotlyOutput("plot3"),
                 h6("Conversion 2 == #visits finish flow/ #visits start flow", align = "left")),
      )
      , width = 5
    )
  )
) 



# Define server logic required to draw a histogram
server <- function(input, output, session){
  
  # create graph 1
  output$plot1 <- renderPlotly({
    
    df_dat <- filter(x, week == input$week
    )
    
    plot_ly() %>%
      add_trace(
        type = "funnel",
        name= "N2B",
        y = c("Product page", "Start application", "Finish application"),
        x = c(df_dat$`Product page`[df_dat$N2BvsE2B=="N2B"],
              df_dat$`Start application`[df_dat$N2BvsE2B=="N2B"], 
              df_dat$`Finish application`[df_dat$N2BvsE2B=="N2B"]),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(82, 81, 153)", "rgb(82, 81, 153)", "rgb(82, 81, 153)"
        )),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white" ))) %>%
      
      add_trace(
        type = "funnel",
        name= "E2B",
        y = c("Product page", "Start application", "Finish application"),
        x = c(df_dat$`Product page`[df_dat$N2BvsE2B=="E2B"],
              df_dat$`Start application`[df_dat$N2BvsE2B=="E2B"], 
              df_dat$`Finish application`[df_dat$N2BvsE2B=="E2B"]),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)"
        )),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white" ))) %>%  
      
      layout(yaxis = list(categoryarray = "Product page", "Start application", "Finish application",
                          tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             xaxis = list(tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  })
  
  # create graph 2
  output$plot2 <- renderPlotly ({
    
    df_dat <- filter(x, Month == input$Month,
                     Device ==input$Device)
    
    # plotly object
    plot_ly(x = as.character(unique(df_dat$week))) %>%
      add_trace(y = ~df_dat$con_1[df_dat$N2BvsE2B=="N2B"], 
                type='bar', 
                name="N2Bcon_1",
                text = df_dat$con_1[df_dat$N2BvsE2B=="N2B"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(255,098,000)')) %>%
      add_trace(y = ~df_dat$con_1[df_dat$N2BvsE2B=="E2B"], 
                type='bar', 
                name="E2Bcon_1",
                text = df_dat$con_1[df_dat$N2BvsE2B=="E2B"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(96,166,218)')) %>%
      
      
    layout(title = "",
           xaxis = list(title = "",
                        textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                        showline = TRUE,
                        linecolor = 'rgb(204, 204, 204)',
                        linewidth = 2,
                        tickfont = list(family = "ING me",
                                        size = 10,
                                        color = 'rgb(105, 105, 105)')),
           yaxis = list(title = "",
                        range =c(0,100),
                        showticklabels = TRUE,
                        linecolor = 'rgb(204, 204, 204)',
                        linewidth = 2,
                        # autotick = TRUE,
                        # dtick = 10,
                        ticks = 'inside',
                        tickcolor = 'rgb(204, 204, 204)',
                        # tickwidth = 1,
                        # ticklen = 10,
                        tickfont = list(family = "ING me",
                                        size = 10,
                                        color = 'rgb(105, 105, 105)')),
           barmode = 'group',
           legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  })
  
  # create graph 3
  output$plot3 <- renderPlotly ({
    
    df_dat <- filter(x, Month == input$Month,
                     Device ==input$Device)
    
    # plotly object
    plot_ly(x = as.character(unique(df_dat$week))) %>%
      add_trace(y = ~df_dat$con_2[df_dat$N2BvsE2B=="N2B"], 
                type='bar', 
                name="N2Bcon_2",
                text = df_dat$con_2[df_dat$N2BvsE2B=="N2B"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_2: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(255,098,000)')) %>%
      add_trace(y = ~df_dat$con_2[df_dat$N2BvsE2B=="E2B"], 
                type='bar', 
                name="E2Bcon_2",
                text = df_dat$con_2[df_dat$N2BvsE2B=="E2B"],
                hovertemplate = paste('%{x}', '<br>df_dat$con_2: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(96,166,218)')) %>%
      #   add_trace(y = ~df_dat$con_1[df_dat$N2BvsE2B=="UNK"], 
      #            type='bar', 
      #            name="UNKcon_1",
      #            text = df_dat$con_1[df_dat$N2BvsE2B=="UNK"],
      #            hovertemplate = paste('%{x}', '<br>df_dat$con_1: %{text}<br>'),
      #            texttemplate = '%{y}', textposition = 'outside',
      #            hoverinfo = 'text',
      #            textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
      #            marker = list(color = 'rgb(52,150,81)')) %>%
      
      
    layout(title = "",
           xaxis = list(title = "",
                        textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                        showline = TRUE,
                        linecolor = 'rgb(204, 204, 204)',
                        linewidth = 2,
                        tickfont = list(family = "ING me",
                                        size = 10,
                                        color = 'rgb(105, 105, 105)')),
           yaxis = list(title = "",
                        range =c(0,100),
                        showticklabels = TRUE,
                        linecolor = 'rgb(204, 204, 204)',
                        linewidth = 2,
                        # autotick = TRUE,
                        # dtick = 10,
                        ticks = 'inside',
                        tickcolor = 'rgb(204, 204, 204)',
                        # tickwidth = 1,
                        # ticklen = 10,
                        tickfont = list(family = "ING me",
                                        size = 10,
                                        color = 'rgb(105, 105, 105)')),
           barmode = 'group',
           legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  })
  
  # create graph 4
  output$plot4 <- renderPlotly ({
    
    df_dat <- filter(z, 
                     Month == input$Month,
                     Device ==input$Device,
                     #product ==input$product,
                     N2BvsE2B == input$N2BvsE2B
    )
    
    # plotly object
    plot_ly(x = as.character(unique(df_dat$week))) %>%
      add_trace(y = ~df_dat$visits[df_dat$product=="Orange Advantage"], 
                type='scatter', 
                mode ='lines',
                name="visits_Orange Advantage",
                text = df_dat$visits[df_dat$product=="Orange Advantage"],
                hovertemplate = paste('%{x}', '<br>df_dat$visits: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(255,098,000)'),
                line = list(color = 'rgb(255,098,000)'))  %>%
      add_trace(y =~df_dat$visits[df_dat$product=="Mortgage simplifier"], 
                type='scatter', 
                mode ='lines',
                name="visits_Mortgage simplifier",
                text = df_dat$visits[df_dat$product=="Mortgage simplifier"],
                hovertemplate = paste('%{x}', '<br>df_dat$visits: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(96,166,218)'),
                line = list(color = 'rgb(96,166,218)')) %>%
      add_trace(y =~df_dat$visits[df_dat$product=="Fixed rate"], 
                type='scatter', 
                mode ='lines',
                name="visits_Fixed rate",
                text = df_dat$visits[df_dat$product=="Fixed rate"],
                hovertemplate = paste('%{x}', '<br>df_dat$visits: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(168, 168, 168)'),
                line = list(color = 'rgb(168, 168, 168)')) %>%
      
      layout(title = "",
             xaxis = list(title = "",
                          textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                          showline = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             yaxis = list(title = "",
                          #range =c(0,100),
                          showticklabels = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          # autotick = TRUE,
                          # dtick = 10,
                          ticks = 'inside',
                          tickcolor = 'rgb(204, 204, 204)',
                          # tickwidth = 1,
                          # ticklen = 10,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    
    
    
  })
  
  # create graph 5
  output$plot5 <- renderPlotly ({
    
    df_dat <- filter(z, 
                     Month == input$Month,
                     !is.na(traffic)
    )
    
    # plotly object
    plot_ly(x = as.character(unique(df_dat$week))) %>%
      add_trace(y = ~df_dat$`Product page`[df_dat$traffic=="SEO"], 
                type='scatter', 
                mode ='lines',
                name="SEO",
                text = df_dat$`Product page`[df_dat$traffic=="SEO"],
                hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(255,098,000)'),
                line = list(color = 'rgb(255,098,000)'))  %>%
      
      add_trace(y = ~df_dat$`Product page`[df_dat$traffic=="SEA"], 
                type='scatter', 
                mode ='lines',
                name="SEA",
                text = df_dat$`Product page`[df_dat$traffic=="SEA"],
                hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(96,166,218)'),
                line = list(color = 'rgb(96,166,218)'))  %>%
      
      add_trace(y = ~df_dat$`Product page`[df_dat$traffic=="Direct"], 
                type='scatter', 
                mode ='lines',
                name="Direct",
                text = df_dat$`Product page`[df_dat$traffic=="Direct"],
                hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(168, 168, 168)'),
                line = list(color = 'rgb(168, 168, 168)'))  %>%
      
      add_trace(y = ~df_dat$`Product page`[df_dat$traffic=="Referrals"], 
                type='scatter', 
                mode ='lines',
                name="Referrals",
                text = df_dat$`Product page`[df_dat$traffic=="Referrals"],
                hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(171, 0, 102)'),
                line = list(color = 'rgb(171, 0, 102)'))  %>%
      
      add_trace(y = ~df_dat$`Product page`[df_dat$traffic=="Affiliates"], 
                type='scatter', 
                mode ='lines',
                name="Affiliates",
                text = df_dat$`Product page`[df_dat$traffic=="Affiliates"],
                hovertemplate = paste('%{x}', '<br>df_dat$`Product page`: %{text}<br>'),
                texttemplate = '%{y}', textposition = 'outside',
                hoverinfo = 'text',
                textfont = list(family = "ING me", color = 'rgb(105, 105, 105)', size = 14),
                marker = list(color = 'rgb(52, 150, 81)'),
                line = list(color = 'rgb(52, 150, 81)'))  %>%
     
      
      layout(title = "",
             xaxis = list(title = "",
                          textfont = list(color = 'rgb(105, 105, 105)', size = 10, family = "ING me"),
                          showline = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             yaxis = list(title = "",
                          #range =c(0,100),
                          showticklabels = TRUE,
                          linecolor = 'rgb(204, 204, 204)',
                          linewidth = 2,
                          # autotick = TRUE,
                          # dtick = 10,
                          ticks = 'inside',
                          tickcolor = 'rgb(204, 204, 204)',
                          # tickwidth = 1,
                          # ticklen = 10,
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    
    
    
  })
  # })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)