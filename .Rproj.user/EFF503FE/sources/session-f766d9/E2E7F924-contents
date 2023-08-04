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
library(tidyr)
#library(shiny)
#library(shinydashboard)
library(xfun)
library(bslib)
#library(stdlib.h)

sales1 <- read_excel("~/Documents/hustlers/data/DigitalSales_daily.xlsx")

#adobe_sales <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jun 13, 2023.xlsx")

adobe_sales_CA_app_completed <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A14:C45")
colnames(adobe_sales_CA_app_completed) <- c("product","device","Sales")
adobe_sales_CA_app_completed$`Visitor or Client` <- "visitor"

adobe_sales_SA_app_completed <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A61:C95")
colnames(adobe_sales_SA_app_completed) <- c("product","device","Sales")
adobe_sales_SA_app_completed$`Visitor or Client` <- "client"

adobe_sales_INS_policy_completed <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A109:C132")
colnames(adobe_sales_INS_policy_completed) <- c("product","device","Sales")
adobe_sales_INS_policy_completed$`Visitor or Client` <- "visitor"

adobe_sales_INS_policy_completed_total <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A146:B149")
colnames(adobe_sales_INS_policy_completed_total) <- c("device","Sales")
adobe_sales_INS_policy_completed_total$product <- "insurance_total"

adobe_sales_CL_app_submitted <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A163:C174")
colnames(adobe_sales_CL_app_submitted) <- c("product","device","Sales")

adobe_sales_CL_total <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A183:B186")
colnames(adobe_sales_CL_total) <- c("device","Sales")
adobe_sales_CL_total$product <- "lending_total"

adobe_device_tot <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A198:B203")
colnames(adobe_device_tot) <- c("device","Sales", "total")

adobe_app_product_tot <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A218:B221")
colnames(adobe_app_product_tot) <- c("product","Sales")
adobe_app_product_tot$device <- "native app"

adobe_app_launches <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="A236:B239")
colnames(adobe_app_launches) <- c("date","launches")
adobe_app_launches$device <- "native app"

adobe_sales <- dplyr::full_join(adobe_sales_CA_app_completed, adobe_sales_SA_app_completed)
adobe_sales <- dplyr::full_join(adobe_sales, adobe_sales_INS_policy_completed)
adobe_sales <- dplyr::full_join(adobe_sales, adobe_sales_CL_app_submitted)
#adobe_sales <- dplyr::full_join(adobe_sales, adobe_sales_INS_policy_completed_total)
#adobe_sales <- dplyr::full_join(adobe_sales, adobe_sales_CL_total)
adobe_sales <- dplyr::full_join(adobe_sales, adobe_device_tot)
adobe_sales <- dplyr::full_join(adobe_sales, adobe_app_product_tot)

date <- read_excel("~/Documents/hustlers/data/_Sales by Device - Last Month - ING Direct Production Report Suite - Jul 3, 2023.xlsx", range="B3:B4")
colnames(date) <- NULL
rownames(date) <- NULL
adobe_sales$Date <- as.character(date)


adobe_sales_re <- adobe_sales %>%
  dplyr::filter(device!=("Mobile Device Type")) %>%
  dplyr::filter(product!=("insurance_total")) %>%
  dplyr::mutate(
    Date1 = as.Date(Date, format="%Y - %B %d")
  ) %>%
  dplyr::mutate(
    Date = cut(as.POSIXct(Date1), "month")
  ) %>%
  dplyr::select(-Date1) %>%
  dplyr::mutate(Category = case_when(
    product == "orange everyday" ~ "Payment Accounts",
    str_detect(product, "saving") ~ "Savings",
    str_detect(product, "home loan") ~ "Mortgages",
    str_detect(product, "super") ~ "Investment Products",
    str_detect(product, "insurance|Insurance") ~ "Insurance",
    str_detect(product, "personal loan|credit card") ~ "Unsecured Lending"
  )) %>%
  dplyr::filter(!is.na(Category)) %>%
  dplyr::group_by(Date, Category, `Visitor or Client`, device) %>%
  dplyr::summarise(
    sales = sum(Sales)
  ) %>%
  as.data.frame() %>%
  dplyr::mutate(
    Date =as.Date(Date)
  )

adobe_sales_re$kind <- "adobe"

## add proportions to adobe file

adobe_sales_re <- adobe_sales_re %>%
  tidyr::spread(device, sales)
adobe_sales_re[is.na(adobe_sales_re)] <- 0

adobe_sales_re <- adobe_sales_re %>%
  dplyr::mutate(
    total = `Mobile Phone` + `native app` + Other + Tablet 
  ) %>%
  dplyr::mutate(
    mobile_share = 1-(Other/ total)
  ) %>%
  dplyr::mutate(
    `Visitor or Client` = case_when(
      `Visitor or Client` == 0 ~ "visitor",
      TRUE ~ `Visitor or Client`
    )
  ) %>%
  dplyr::select(-kind)



### Actual sales 
sales_month <- sales1 %>%
  dplyr::mutate(product = case_when(
    PRODUCT_NAME == "INSURANCE" ~ "Insurance",
    PRODUCT_NAME == "LIVING SUPER" ~ "Superannuation",
    PRODUCT_NAME == "MORTGAGES" ~ "Mortgages",
    PRODUCT_NAME == "ORANGE EVERYDAY" ~ "Current account",
    PRODUCT_NAME == "ORANGE ONE" ~ "Credit card",
    PRODUCT_NAME == "PTD" ~ "Personal Term Deposit",
    PRODUCT_NAME == "SA" ~ "Savings Accounts",
    PRODUCT_NAME == "SM" ~ "Savings Maximiser",
    PRODUCT_NAME == "PERSONAL LOANS" ~ "Personal loan")
  ) %>%
  dplyr::mutate(
    Date = as.Date(Last_Refreshed, format="%d-%B-%Y")
  ) %>%
  dplyr::mutate(
    week_Date = cut(as.POSIXct(Date), "week"),
    month_Date = cut(as.POSIXct(Date), "month"),
  ) %>%
  dplyr::mutate(
    `Visitor or Client` = case_when(
      NTB == "Existing" ~ "client",
      NTB == "NTB" ~ "visitor",
      NTB == "NULL" ~ "::unspecified::",
      is.na(NTB) ~ "::unspecified::"
    )
  ) %>%
  dplyr::mutate(Device = case_when(
    CHANNEL_DESCRIPTION =="E-QUOTE" ~ "EQuote",
    CHANNEL_DESCRIPTION =="NOT SUPPLIED" ~ "UNK",
    CHANNEL_DESCRIPTION =="BROKER" ~ "Broker",
    CHANNEL_DESCRIPTION =="DIRECT" ~ "Direct",
    CHANNEL_DESCRIPTION =="MOBILE APP" ~ "app",
    CHANNEL_DESCRIPTION =="ONLINE" ~ "web"
  )) %>%
  dplyr::select(product, month_Date, Sales, `Visitor or Client`, Device) %>%
  dplyr::filter(Device =="app"|Device=="web") %>%
  dplyr::group_by(month_Date, product, `Visitor or Client`, Device) %>%
  dplyr::summarise(
    Sales = sum(Sales)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    Date = as.Date(month_Date)
  ) %>%
  dplyr::rename(
    device = Device
  ) 


sales_total <- sales_month %>%
  dplyr::rename(
    Category = product
  ) %>%
  dplyr::group_by(Category, Date, device) %>%
  dplyr::summarise(
    Sales = sum(Sales)
  )
sales_total$`Visitor or Client` <- "total"
sales <- dplyr::full_join(sales_month, sales_total)

sales <- sales %>%
  dplyr::mutate(
    Category = case_when(
      product == "Credit card" ~ "Unsecured Lending",
      product == "Current account" ~ "Payment Accounts",
      product == "Personal loan" ~ "Unsecured Lending",
      product == "Personal Term Deposit" ~ "Savings",
      product == "Savings " ~ "Savings",
      product == "Mortgages" ~ "Mortgages",
      str_detect(product, "Savings") ~ "Savings"
    )
  ) %>%
  dplyr::group_by(Date, Category, `Visitor or Client`, device) %>%
  summarise(
    sales= sum(Sales)
  ) %>%
  as.data.frame() %>%
  dplyr::filter(Date == "2023-06-01")

#sales$kind <-"internal"


sales <- sales %>%
  tidyr::spread(device, sales)
sales[is.na(sales)] <- 0

total_file <- dplyr::full_join(adobe_sales_re, sales) %>%
  dplyr::mutate(
    sales_non_desktop = case_when(
      !is.na(web) ~ web * mobile_share
    )
  ) %>%
  dplyr::mutate(
    mobile_sales = round(sales_non_desktop + app, digits=0)
  ) 



