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
#try_traf <- read_csv("./HL_traf_tool3.csv")
#try_month <- read_csv("./HL_conv_month_tool3.csv")
#try_week <- read_csv("./HL_conv_week_tool3.csv")
#conv_tot <- read_csv("./HL_conv_tool3.csv")

#conv_sales <- read_csv("./HL_conv_tool34.csv")
#conv_markchan <- read_csv("./HL_conv_tool35.csv")

testing <- read_csv("./testing.csv")
#testing <- testing %>%
  #dplyr::filter(`Visitor or Client` !="::unspecified::") %>%
 # dplyr::filter(`Visitor or Client` !="None")

testing$year <- year(testing$Date)
testing <- testing %>%
  
  
  dplyr::arrange(Date
  ) %>%
  dplyr::mutate(
    target_PP_23 = case_when(
      year == "2022" ~ 10,
      TRUE ~ target_PP_23
    )
  )


x <- as.data.frame(testing)
#z <- as.data.frame(try_month) 
#y <- as.data.frame(try_week) 






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
        tabPanel("Source over time", plotlyOutput("plot6"),
                 h6("If plot is empty, data not available", align = "left")),
        tabPanel("Application per channel", plotlyOutput("plot7"),
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
             `Visitor or Client` == input$`Visitor or Client`,
            # !is.na(`Calculators`)
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
  
  # create graph 2
  output$plot2 <- renderPlotly ({
  
    df_dat <- filter(x,
                     `Visitor or Client` == "total" &
                     MonthvsWeek == "Month" & 
                     `Last Touch Marketing Channel`=="::unspecified::",
                     !is.na(`Calculators`)) %>%
      group_by(month = lubridate::floor_date(Date, 'month'))
    
  
    
    # plotly object
    plot_ly(x = unique(df_dat$Date[!is.na(df_dat$`Product page CvnC`)])) %>%
      add_trace(y = ~df_dat$`Product page CvnC`, 
                type='bar', 
                name="Product_page",
                #  text = df_dat$`Product page`,
                marker = list(color = 'rgb(82,81,153)')) %>%
      
      add_trace(y = ~df_dat$target_PP_23, 
                type='scatter', 
                mode ='lines',
                name="target_PP",
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
  
  output$plot3 <- renderPlotly({
    
    datalala <- filter(x, 
                       !is.na(Sales),
                       !is.na(`Start application CvnC`),
                       MonthvsWeek == input$MonthvsWeek,
                       Date == input$Date
    ) 
    
    plot_ly() %>%
      add_trace(
        type = "funnel",
        name= "visitor",
        y = c("Product page", "Start application", "Finish application", "Sales"),
        x = c(
          datalala$`Product page CvnC`[datalala$`Visitor or Client`=="visitor"],
          datalala$`Start application CvnC`[datalala$`Visitor or Client`=="visitor"], 
          datalala$`Finish application CvnC`[datalala$`Visitor or Client`=="visitor"],
          datalala$Sales[datalala$`Visitor or Client`=="visitor"]),
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
        x = c(datalala$`Product page CvnC`[datalala$`Visitor or Client`=="client"],
              datalala$`Start application CvnC`[datalala$`Visitor or Client`=="client"], 
              datalala$`Finish application CvnC`[datalala$`Visitor or Client`=="client"],
              datalala$Sales[datalala$`Visitor or Client`=="client"]),
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
    
    MonthvsWeek <- reactive({
      filter(x,
             !is.na(con_1_CvnC),
             `Visitor or Client` != "None",
              MonthvsWeek == input$MonthvsWeek,
             `Visitor or Client` == "total",
             is.na(`Last Touch Marketing Channel`) | `Last Touch Marketing Channel`=="Direct"
            
      )
    })
    observeEvent(MonthvsWeek(), {
      choices <- unique(MonthvsWeek()$Date)
      updateSelectInput(inputId = "Date", choices = choices) 
    })
    
    # plotly object
    plot_ly(x = unique(MonthvsWeek()$Date)) %>%
      add_trace(y = ~MonthvsWeek()$con_1_CvnC, 
                type='bar', 
                name="Start rate",
                # text = df_dat$`Start application`,
                marker = list(color = 'rgb(82,81,153)')) %>%
      
      add_trace(y = ~MonthvsWeek()$target_con1, 
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
    
  #  MonthvsWeek <- reactive({
  #    filter(x,
  #           !is.na(`Product page MC`),
  #           MonthvsWeek == input$MonthvsWeek,
  #           `Visitor or Client` == "total",
  #           !is.na(`Last Touch Marketing Channel`),
  #           Date == input$Date
  #    )
  # })
  #  observeEvent(MonthvsWeek(), {
  #    choices <- unique(MonthvsWeek()$Date)
  #    updateSelectInput(inputId = "Date", choices = choices) 
  #  })
    
    df_dat <- filter(x,
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
    plot_ly(df_dat, 
            #%>%
           # filter(Date == input$Date),
            labels =~`Last Touch Marketing Channel`, 
            values=~`Product page MC`) %>%
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
  
  
  output$plot6 <- renderPlotly ({
    
    # create graph 6
    MonthvsWeek <- reactive({
      filter(x, MonthvsWeek == input$MonthvsWeek,
             `Visitor or Client` == "total",
             !is.na(`Product page MC`),
             !is.na(`Last Touch Marketing Channel`), 
             !is.na(Date)
      )
    })
    observeEvent(MonthvsWeek(), {
      choices <- unique(MonthvsWeek()$Date)
      updateSelectInput(inputId = "Date", choices = choices) 
    })
    
    # plotly object
    plot_ly(MonthvsWeek(), x =unique(MonthvsWeek()$Date)) %>%
      add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Paid Search"], 
                type='scatter', 
                mode ='lines',
                name="Paid Search",
                marker = list(color = 'rgb(52, 150, 81)'),
                line = list(color = 'rgb(52, 150, 81)'))  %>%
      add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Direct"], 
                type='scatter', 
                mode ='lines',
                name="Direct",
                marker = list(color = 'rgb(85, 155, 209)'),
                line = list(color = 'rgb(85, 155, 209)'))  %>%
     # add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Display"],
    #            type='scatter', 
     #           mode ='lines',
      #          name="Display",
      #          marker = list(color = 'rgb(171,0,102)'),
       #         line = list(color = 'rgb(171,0,102)'))  %>%
      
     # add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Referring Domains"],
    #            type='scatter', 
     #           mode ='lines',
    #            name="Referring Domains",
    #            marker = list(color = 'rgb 215, 0, 0 '),
    #            line = list(color = 'rgb 215, 0, 0 '))  %>%
      
      add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Natural Search"],
                type='scatter', 
                mode ='lines',
                name="Natural Search",
                marker = list(color = 'rgb(255, 98,0)'),
                line = list(color = 'rgb(255, 98,0)'), dash="dash")  %>%
      
      add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Email"],
                type='scatter', 
                mode ='lines',
                name="Email",
                marker = list(color = 'rgb(208, 217, 60)'),
                line = list(color = 'rgb(208, 217, 60)'), dash="dash")  %>%
      
     # add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Affiliate"],
     #           type='scatter', 
     #           mode ='lines',
     #           name="Affiliate",
    #            marker = list(color = 'rgb(82, 81, 153)'),
     #           line = list(color = 'rgb(82, 81, 153)'), dash="dash")  %>%
      
    #  add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Mobile App"],
    #            type='scatter', 
    #            mode ='lines',
    #            name="Mobile App",
    #            marker = list(color = 'rgb(52, 150, 81)'),
    #            line = list(color = 'rgb(52, 150, 81)'), dash="dash")  %>%
      
     # add_trace(y = ~MonthvsWeek()$`Product page MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Social Networks"],
     #           type='scatter', 
     #           mode ='lines',
     #           name="Social Networks",
     #           marker = list(color = 'rgb(105, 105, 105)'),
     #           line = list(color = 'rgb(105, 105, 105)'), dash="dash")  %>%
      
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
  
  output$plot7 <- renderPlotly ({
    
    # create graph 7
    MonthvsWeek <- reactive({
      filter(x, MonthvsWeek == input$MonthvsWeek,
             `Visitor or Client` == "total",
             !is.na(`Start application MC`),
             !is.na(`Last Touch Marketing Channel`), 
             !is.na(Date)
      )
    })
    observeEvent(MonthvsWeek(), {
      choices <- unique(MonthvsWeek()$Date)
      updateSelectInput(inputId = "Date", choices = choices) 
    })
    
    # plotly object
    plot_ly(MonthvsWeek(), x =unique(MonthvsWeek()$Date)) %>%
      add_trace(y = ~MonthvsWeek()$`Start application MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Direct"], 
                type='bar', 
                name="Direct",
                marker = list(color = 'rgb(85, 155, 209)')) %>%
      
      add_trace(y = ~MonthvsWeek()$`Start application MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Paid Search"], 
                type='bar', 
                name="Paid search",
                marker = list(color = 'rgb(52, 150, 81)')) %>%
      
      add_trace(y = ~MonthvsWeek()$`Start application MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Email"], 
                type='bar', 
                name="Email",
                marker = list(color = 'rgb(208, 217, 60)')) %>%
      
      add_trace(y = ~MonthvsWeek()$`Start application MC`[MonthvsWeek()$`Last Touch Marketing Channel`=="Natural Search"], 
                type='bar', 
                name="Natural search",
                marker = list(color = 'rgb(255, 98,0)')) %>%
    
      
      layout(title = "", barmode = 'stack',
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

