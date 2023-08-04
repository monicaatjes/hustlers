Number <- c(10, 20, 30 , 40, 50 ,60, 70, 80, 90,100,110,120,130,140)
Att1 <- c('a','a','a','a','a','a','a','b','b','b','b','b','b','b')
Att2 <- c('c','c','c','d','d','d','d','e','e','e','g','g','g','g')
Index <- c('I1','I2','I3','I4', 'I5','I6','I7','I8','I9','I10', 'I11','I12','I13','I14')
df <- data.frame(Number, Att1, Att2, Index)

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


x2 <- x %>% filter(MonthvsWeek=="Week", `Visitor or Client` == "total",  
                   !is.na(`Product page MC`), 
                   `Last Touch Marketing Channel` != "::unspecified::",
                   !is.na(`Last Touch Marketing Channel`), 
                   !is.na(Date)) %>%
  select(MonthvsWeek, Date, `Product page MC`, `Last Touch Marketing Channel`) %>%
  spread(`Last Touch Marketing Channel`, `Product page MC`)


library(shiny)
library(data.table)
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
library(DT)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("App"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Att1", "Choose Att1", choices = c("NULL", as.character(unique(df$Att1))), selected = "NULL"),
      uiOutput("c"),
      uiOutput("d")),
    # Show a plot of the generated distribution
    mainPanel( DT::dataTableOutput("table")
               
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selectedData <- reactive({
    if(input$Att1 == "NULL") Ddata <- df  #Keep full data set if NULL
    else Ddata <- subset(df, Att1 == input$Att1)
    
    Ddata
  })
  
  ######################
  output$c <- renderUI({selectInput("Att2", "Choose Att2", choices = c("NULL", as.character(unique(selectedData()$Att2))), selected = "NULL")})
  
  selectedData2 <- reactive({
    if(input$Att2 == "NULL") Vdata <- selectedData()
    else Vdata <- subset(selectedData(), Att2 == input$Att2)
    
    Vdata
  })
  ######################
  
  #=====================
  output$d <- renderUI({selectInput("Index", "Choose Index", choices = c("NULL", as.character(unique(selectedData2()$Index))), selected = "NULL")})
  
  selectedData3 <- reactive({
    if(input$Index == "NULL") Fdata <- selectedData2()
    else Fdata <- subset(selectedData2(), Index == input$Index)
    
    Fdata
  })
  #=====================
  
  output$table <- DT::renderDataTable({
    head(selectedData3(), n = 10)
  })
}

# Run the application 
runApp(shinyApp(ui = ui, 
                server = server), launch.browser=TRUE
)
Jake 458
SCORE:3

There is a specific shiny function to update the content of a SelectInput: updateSelectInput().

If used inside an observe it can be used exactly for what you are trying to do:
  
  server <- function(input, output, session) {
    
    observe({
      input$Att1
      
      x <- df[df$Att1 == input$Att1, 'Att2']
      xs <- as.character(unique(x))
      updateSelectInput(session, 'Att2', choices = xs)
    })
    
    selectedData <- reactive({
      df[df$Att2 == input$Att2, ]
    })
    
    output$table <- DT::renderDataTable({
      head(selectedData(), n = 10)
    })       
    
  }