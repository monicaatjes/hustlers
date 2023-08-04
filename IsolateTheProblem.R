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

x <- as.data.frame(try_traf)
#z <- as.data.frame(try_month) 
#y <- as.data.frame(try_week) 
K <- as.data.frame(conv_tot)
S <- as.data.frame(conv_sales)
D <- as.data.frame(conv_markchan)


HL_ALL_ALL <- dplyr::full_join(x, K)
HL_ALL_ALL <- dplyr::full_join(HL_ALL_ALL, S)
HL_ALL_ALL <- dplyr::full_join(HL_ALL_ALL, D)

x <- HL_ALL_ALL


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
                  choices = unique(S$MonthvsWeek)),
      
      selectInput(inputId= "Visitor or Client", 
                  label= "Visitor or Client",
                  choices = unique(S$`Visitor or Client`)),
      
      selectInput(inputId= "Date", 
                  label= "week",
                  choices = unique(S$Date)),
      
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
    
    # create graph 1
    output$plot3 <- renderPlotly({
      
      
      MonthvsWeek <- reactive({
        filter(x, MonthvsWeek == input$MonthvsWeek,
               `Visitor or Client` == input$`Visitor or Client`
        )
      })
      observeEvent(MonthvsWeek(), {
        choices <- unique(MonthvsWeek()$Date)
        updateSelectInput(inputId = "Date", choices = choices) 
      })
      
      
     
      
      plot_ly(MonthvsWeek()) %>%
        add_trace(
          type = "funnel",
          name= "visitor",
          y = c("Product page", "Start application", "Finish application", "Sales"),
          x = c(
            MonthvsWeek()$`Product page`[MonthvsWeek()$`Visitor or Client`=="visitor"],
            MonthvsWeek()$`Start application`[MonthvsWeek()$`Visitor or Client`=="visitor"], 
            MonthvsWeek()$`Finish application`[MonthvsWeek()$`Visitor or Client`=="visitor"],
            MonthvsWeek()$Sales[MonthvsWeek()$`Visitor or Client`=="visitor"]),
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
          x = c(MonthvsWeek()$`Product page`[MonthvsWeek()$`Visitor or Client`=="client"],
                MonthvsWeek()$`Start application`[MonthvsWeek()$`Visitor or Client`=="client"], 
                MonthvsWeek()$`Finish application`[MonthvsWeek()$`Visitor or Client`=="client"],
                MonthvsWeek()$Sales[MonthvsWeek()$`Visitor or Client`=="client"]),
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
      
      df_dat <- filter(S,
                       `Visitor or Client` == input$`Visitor or Client`,
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
    
    
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)