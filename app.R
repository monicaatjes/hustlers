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


# For high level traffic
try_traf <- read_csv("./HL_traf_tool3.csv")
#try_month <- read_csv("./HL_conv_month_tool3.csv")
#try_week <- read_csv("./HL_conv_week_tool3.csv")
conv_tot <- read_csv("./HL_conv_tool3.csv")

conv_sales <- read_csv("./HL_conv_tool34.csv")
conv_markchan <- read_csv("./HL_conv_tool35.csv")

x <- as.data.frame(testing)
#z <- as.data.frame(try_month) 
#y <- as.data.frame(try_week) 
K <- as.data.frame(conv_tot)
S <- as.data.frame(conv_sales)
D <- as.data.frame(conv_markchan)

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
      selectInput(inputId= "MonthvsWeek", 
                  label= "MonthvsWeek",
                  choices = unique(x$MonthvsWeek[!is.na(x$Sales)])),
      
      selectInput(inputId= "Visitor or Client", 
                  label= "Visitor or Client",
                  choices = unique(x$`Visitor or Client`)),
      
      selectInput(inputId= "Date", 
                  label= "week",
                  choices = unique(x$Date[!is.na(x$Sales)])),
      
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
        tabPanel("Total traffic main pages", plotlyOutput("plot1"),
                 h6("If plot is empty, data not available", align = "left")),
        tabPanel("Traffic pp vs target", plotlyOutput("plot2"),
                 h6("Static plot", align = "left")),
        tabPanel("Funnel", plotlyOutput("plot3"),
                 h6("If plot is empty, data not available", align = "left")),
        tabPanel("Conversion", plotlyOutput("plot4"),
                 h6("If plot is empty, data not available", align = "left")),
        tabPanel("Source", plotlyOutput("plot5"),
                 h6("If plot is empty, data not available", align = "left")),
       
      )
      , width = 5
    )
  )
) 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    output$plot1 <- renderPlotly ({
      
      # create graph 1
      df_dat <- filter(x, 
                       MonthvsWeek ==input$MonthvsWeek,
                       `Visitor or Client` == input$`Visitor or Client`)
    # plotly object
    plot_ly(x = df_dat$Date) %>%
      add_trace(y = ~df_dat$`Product page open`, 
                type='scatter', 
                mode ='lines',
                name="Product_pages_open",
                text = df_dat$`Product page open`,
                marker = list(color = 'rgb(82,81,153)'),
                line = list(color = 'rgb(82,81,153)'))  %>%
      add_trace(y = ~df_dat$`Product page secured`, 
                type='scatter', 
                mode ='lines',
                name="Product_pages_secured",
                text = df_dat$`Product page secured`,
                marker = list(color = 'rgb(255,098,000)'),
                line = list(color = 'rgb(255,098,000)'))  %>%
      add_trace(y = ~df_dat$`Product page app`, 
                type='scatter', 
                mode ='lines',
                name="Product pages app",
                text = df_dat$`Product page app`,
                marker = list(color = 'rgb(171,0,102)'),
                line = list(color = 'rgb(171,0,102)'))  %>%
      
      add_trace(y = ~df_dat$`Campaign page`, 
                type='scatter', 
                mode ='lines',
                name="Campaign_pages",
                text = df_dat$`Campaign page`,
                marker = list(color = 'rgb 168, 168, 168'),
                line = list(color = 'rgb 168, 168, 168'))  %>%
      
      add_trace(y = ~df_dat$Calculators, 
                type='scatter', 
                mode ='lines',
                name="Calculators",
                text = df_dat$Calculators,
                marker = list(color = 'rgb(52, 150, 81)'),
                line = list(color = 'rgb(52,150,81)'), dash="dash")  %>%
      
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
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    
    
    
  })
  
  # create graph 2
  output$plot2 <- renderPlotly ({
    
    df_dat <- filter(x,
                     `Visitor or Client` == "total" &
                     MonthvsWeek == "Month",
                     !is.na(`Product page target`),
                     Date != "2023-02-01" ) %>%
      group_by(month = lubridate::floor_date(Date, 'month'))
    
    # plotly object
    plot_ly(x = unique(df_dat$Date)) %>%
      add_trace(y = ~df_dat$`Product page target`, 
                type='bar', 
                name="Product_page",
              #  text = df_dat$`Product page`,
                marker = list(color = 'rgb(82,81,153)')) %>%
  
      add_trace(y = ~unique(df_dat$target_PP_23), 
                type='scatter', 
                mode ='lines',
                name="target_PP",
              #  text = df_dat$target_FF,
                marker = list(color = 'rgb(255,098,000)'),
                line = list(color = 'rgb(255,098,000)'))  %>%
      
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
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             barmode = 'group',
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
      
    
    
  })
  
  # create graph 3
  output$plot3 <- renderPlotly({
    
    df_dat <- filter(S, !is.na(Sales),
                     `Visitor or Client` != "None" & `Visitor or Client` != "::unspecified::",
                     MonthvsWeek == input$MonthvsWeek,
                     Date == input$Date,
                     )
    
    plot_ly() %>%
      add_trace(
        type = "funnel",
        name= "visitor",
        y = c("Product page", "Start application", "Finish application", "Sales"),
        x = c(
          df_dat$`Product page`[df_dat$`Visitor or Client`=="visitor"],
          df_dat$`Start application`[df_dat$`Visitor or Client`=="visitor"], 
          df_dat$`Finish application`[df_dat$`Visitor or Client`=="visitor"],
          df_dat$Sales[df_dat$`Visitor or Client`=="visitor"]),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(82, 81, 153)", "rgb(82, 81, 153)", "rgb(82, 81, 153)",
                                "rgb(82, 81, 153)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white" ))) %>%
      
      add_trace(
        type = "funnel",
        name= "client",
        y = c("Product page", "Start application", "Finish application", "Sales"),
        x = c(df_dat$`Product page`[df_dat$`Visitor or Client`=="client"],
              df_dat$`Start application`[df_dat$`Visitor or Client`=="client"], 
              df_dat$`Finish application`[df_dat$`Visitor or Client`=="client"],
              df_dat$Sales[df_dat$`Visitor or Client`=="client"]),
        textposition = "inside",
        textinfo = "value",
        opacity = 1.0,
        marker = list(color = c("rgb(255,098,000)", "rgb(255,098,000)", "rgb(255,098,000)",
                                "rgb(255,098,000)")),
        textfont = list(family = "ING me", size = 14, color = "white"),
        connector = list(fillcolor = c("white", "white", "white", "white" ))) %>%  
      
      layout(yaxis = list(categoryarray = c("Product page", "Start application", "Finish application", "Sales"),
                          tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             xaxis = list(tickfont =list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
 
     })
  
  # create graph 4
  output$plot4 <- renderPlotly ({
    
    df_dat <- filter(x,
                     `Visitor or Client` == "total",
                     MonthvsWeek == input$MonthvsWeek
    )
    
    # plotly object
    plot_ly(x = df_dat$Date) %>%
      add_trace(y = ~df_dat$con_1, 
                type='bar', 
                name="Start rate",
                # text = df_dat$`Start application`,
                marker = list(color = 'rgb(82,81,153)')) %>%
      
    #  add_trace(y = ~df_dat$con_2, 
    #            type='bar', 
    #            name="con_2",
    #            # text = df_dat$`Start application`,
    #            marker = list(color = 'rgb(82, 81, 153)')) %>%
      
      add_trace(y = ~df_dat$target_con1, 
                type='scatter', 
                mode ='lines',
                name="target_start_rate",
                #  text = df_dat$target_FF,
                marker = list(color = 'rgb(255,098,000)'),
                line = list(color = 'rgb(255,098,000)'))  %>%
      
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
                          tickfont = list(family = "ING me",
                                          size = 10,
                                          color = 'rgb(105, 105, 105)')),
             barmode = 'group',
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
    
  }) 
    
     # create graph 5
    output$plot5 <- renderPlotly ({
      
      df_dat <- filter(D,
                       MonthvsWeek == input$MonthvsWeek,
                       Date == input$Date,
                       `Visitor or Client` == "total",
                       !is.na(`Last Touch Marketing Channel`)
                       
                       
      )
      
      colors <- c('rgb(51, 51, 51)', 'rgb(82, 81, 153)', 'rgb(85, 155, 209)', 
                  'rgb(171, 0, 102)', 'rgb(208, 217, 60)', 'rgb(52, 150, 81)',
                  'rgb(255, 98, 0)', 'rgb(52, 150, 81)', 'rgb(215, 0,0)',
                  'rgb(105, 105, 105)', 'rgb(240, 240, 240)', 'rgb(0, 0,0)')
      # plotly object
      plot_ly(df_dat, labels =~`Last Touch Marketing Channel`, values=~`Product page`) %>%
              add_pie(hole =0.6,
                      textposition = 'inside',
                      textinfo = 'label+percent',
                      insidetextfont = list(color = '#FFFFFF'),
                      textfont = list(family = "ING me", size = 14, color = "white"),
                      marker =list(colors= colors)) %>%
              layout(
             xaxis = list(),
             yaxis = list(),
             legend =list(orientation = 'h', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)'))
             )
      
             
            
             
             
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

