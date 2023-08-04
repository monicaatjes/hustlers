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
                  choices = unique(x$MonthvsWeek)),
      
      selectInput(inputId= "Visitor or Client", 
                  label= "Visitor or Client",
                  choices = unique(x$`Visitor or Client`)),
      
      selectInput(inputId= "Date", 
                  label= "week",
                  choices = unique(x$Date)),
      
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
    MonthvsWeek <- reactive({
      filter(x, MonthvsWeek == input$MonthvsWeek,
             `Visitor or Client` == input$`Visitor or Client`
      )
    })
    observeEvent(MonthvsWeek(), {
      choices <- unique(MonthvsWeek()$Date)
      updateSelectInput(inputId = "Date", choices = choices) 
    })
    
  
    
    
  
    # plotly object
    plot_ly(MonthvsWeek(), x =MonthvsWeek()$Date) %>%
      add_trace(y = ~MonthvsWeek()$`Product page open`, 
                type='scatter', 
                mode ='lines',
                name="Product_pages_open",
                text = MonthvsWeek()$`Product page open`,
                marker = list(color = 'rgb(82,81,153)'),
                line = list(color = 'rgb(82,81,153)'))  %>%
      add_trace(y = ~MonthvsWeek()$`Product page secured`, 
                type='scatter', 
                mode ='lines',
                name="Product_pages_secured",
                text = MonthvsWeek()$`Product page secured`,
                marker = list(color = 'rgb(255,098,000)'),
                line = list(color = 'rgb(255,098,000)'))  %>%
      add_trace(y = ~MonthvsWeek()$`Product page app`, 
                type='scatter', 
                mode ='lines',
                name="Product pages app",
                text = MonthvsWeek()$`Product page app`,
                marker = list(color = 'rgb(171,0,102)'),
                line = list(color = 'rgb(171,0,102)'))  %>%
      
      add_trace(y = ~MonthvsWeek()$`Campaign page`, 
                type='scatter', 
                mode ='lines',
                name="Campaign_pages",
                text = MonthvsWeek()$`Campaign page`,
                marker = list(color = 'rgb 168, 168, 168'),
                line = list(color = 'rgb 168, 168, 168'))  %>%
      
      add_trace(y = ~MonthvsWeek()$Calculators, 
                type='scatter', 
                mode ='lines',
                name="Calculators",
                text = MonthvsWeek()$Calculators,
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
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

