

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


# open file

HL_SR_1 <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 12)
HL_FF_1 <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 34)
HL_PP_N2B_1 <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 64)
HL_PP_sub_N2B_1 <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 75)
HL_CP_N2B_1 <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 88)
HL_CP_E2B_1 <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 106)
HL_PP_guide_1 <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 118)
HL_PP_app <- read_csv("~/Documents/hustlers/data/Home Loan traffic, lead conversion rate, apply funnel (weekly)  - ING Direct Production Report Suite - Jan 30, 2023.csv", skip = 174)

# open file 
HL_conv_1 <- read_csv("~/Documents/hustlers/data/HL - conversion - weekly - ING Direct Production Report Suite - Feb 1, 2023.csv", skip=12)

# SF & FF
HL_SR_1 <- HL_SR_1[1:14,]
HL_SR_1 <- HL_SR_1[-1,]

HL_FF_1 <- HL_FF_1[1:14,]
HL_FF_1 <- HL_FF_1[-1,]

# Conversion
HL_conv <- bind_cols(HL_SR_1, HL_FF_1)

HL_conv <- HL_conv %>%
  dplyr::select(-...5) %>%
  dplyr::mutate(
    Date = as.Date(...1, format="%Y-%m-%d"),
    channel = "app"
  ) %>%
 # dplyr::mutate(
 #   Month = format(Date, "%B")
 # ) %>%
  dplyr::select(-...1, -`HL app intro / Product views`) %>%
  dplyr::mutate(
    con_1 = round((as.numeric(as.character(`HL app intro`)) /as.numeric(as.character(`Product Views`))), digits=2),
    con_2 = round((as.numeric(as.character(`HL app complete`)) / as.numeric(as.character(`HL app intro`))), digits=2)
  )
  
# Detailed product pages
HL_PP_N2B_1 <- HL_PP_N2B_1[1:4,]

HL_PP_N2B_1 <- HL_PP_N2B_1 %>%
  dplyr::mutate(
    NewvsCurrent = "N2B",
    product_page = case_when(
      str_detect(Page, "simplifier") ~ "Mortgage simplifier",
      str_detect(Page, "advantage") ~ "Orange advantage",
      str_detect(Page, "fixed") ~ "Fixed rate HL",
      str_detect(Page, "compare|home-loans") ~ "Comparison page"
    )
  ) %>%
  dplyr::rename(
    "visits" = colnames(HL_PP_N2B_1[,2])
    ) %>%
  dplyr::select(-Page)


HL_CP_E2B_1 <- HL_CP_E2B_1[1:4,]

HL_CP_E2B_1 <- HL_CP_E2B_1 %>%
  dplyr::mutate(
    NewvsCurrent = "E2B",
    product_page = case_when(
      str_detect(Page, "simplifier") ~ "Mortgage simplifier",
      str_detect(Page, "advantage") ~ "Orange advantage",
      str_detect(Page, "fixed") ~ "Fixed rate HL",
      str_detect(Page, "home-loans") ~ "Comparison page"
    )
  ) %>%
  dplyr::rename(
    "visits" = colnames(HL_CP_E2B_1[,2])
  ) %>%
  dplyr::select(-Page)

HL_PP_tot <- full_join(HL_CP_E2B_1, HL_PP_N2B_1, by = c("visits", "NewvsCurrent", "product_page"))

HL_PP_tot <- HL_PP_tot %>%
  dplyr::group_by(product_page, NewvsCurrent) %>%
  dplyr::summarise(
    visits = sum(as.numeric(as.character(visits)))
  )

HL_PP_app <- HL_PP_app[1:4,]
HL_PP_app <- HL_PP_app[-1,]

HL_PP_app <- HL_PP_app %>%
  dplyr::mutate(
  Device = "app",
  product_page = case_when(
    str_detect(...1, "Simplifier") ~ "Mortgage simplifier",
    str_detect(...1, "Advantage") ~ "Orange advantage",
    str_detect(...1, "Fixed") ~ "Fixed rate HL"
  )
) %>%
  dplyr::rename(
    "visits" = colnames(HL_PP_app[,2])
  ) %>%
  dplyr::mutate(
    visits =as.numeric(as.character(visits))
  ) %>%
  dplyr::select(product_page, visits, Device)


HL_PP_tot <- full_join(HL_PP_tot, HL_PP_app)



HL_PP_sub_N2B_1 <- HL_PP_sub_N2B_1[1:6,]

# Campaign pages
HL_CP_N2B_1 <- HL_CP_N2B_1[1:11,]



HL_PP_E2B_1 <- HL_PP_E2B_1[1:7,]
HL_PP_E2B_1 <- HL_PP_E2B_1[-1,]

#### CONVERSION NEW #####

HL_conv_1 <- HL_conv_1 %>%
  dplyr::mutate(
    Date = as.Date(Week, format="%Y-%m-%d"),
    channel = "app"
  ) %>%
  dplyr::mutate(
   Month = format(Date, "%B")
   ) %>%
  dplyr::rename(
    "visits" = colnames(HL_conv_1[,2]),
    "Start application" = colnames(HL_conv_1[,3]),
    "Finish application" = colnames(HL_conv_1[,4])
  ) %>%
  dplyr::mutate(
    con_1 = round((as.numeric(as.character(`Start application`)) /as.numeric(as.character(visits))), digits=2) *100,
    con_2 = round((as.numeric(as.character(`Finish application`)) / as.numeric(as.character(`Start application`))), digits=2) *100
  )

HL_conv_tool <- write_csv(HL_conv_1, "HL_conv_1.csv")
  