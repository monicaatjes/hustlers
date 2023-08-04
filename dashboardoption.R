# open libraries

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


# open file

# too many sources needed already:

# total traffic part different pages

## PP ##
HL_total_dif_pages <- read.csv("~/Documents/hustlers/data/Hustlers Key Metrics Data (Moniek) - ING Direct Production Report Suite - Feb 16, 2023 (2).csv", skip = 10)

HL_total_dif_pages_PP_web <- HL_total_dif_pages[3,]
HL_total_dif_pages_PP_app <- HL_total_dif_pages[15,]
HL_total_dif_pages_PP_calc <- HL_total_dif_pages[27,]
HL_total_dif_pages_PP_camp <- HL_total_dif_pages[39,]

HL_total_dif_pages <- bind_rows(HL_total_dif_pages_PP_web, HL_total_dif_pages_PP_app)
HL_total_dif_pages <- bind_rows(HL_total_dif_pages, HL_total_dif_pages_PP_calc)
HL_total_dif_pages <- bind_rows(HL_total_dif_pages, HL_total_dif_pages_PP_camp)

HL_total_dif_pages <- as.data.frame(t(HL_total_dif_pages))
HL_total_dif_pages <- HL_total_dif_pages 

colnames(HL_total_dif_pages) <- HL_total_dif_pages[1,]
HL_total_dif_pages <- HL_total_dif_pages[-1, ] 

HL_total_dif_pages$Month <- rownames(HL_total_dif_pages)
rownames(HL_total_dif_pages) <- NULL

HL_total_dif_pages <- HL_total_dif_pages %>%
  dplyr::mutate(
    Month = as.yearmon(Month, format ="%b.%Y"),
    Product_pages_web = as.numeric(as.character(`Key home-loans product pages (web)`)),
    Product_pages_app = as.numeric(as.character(`Home loans product pages (app)`)),
    Calculator = as.numeric(as.character(`home loan - calculator`)),
    Campaign_pages = as.numeric(as.character(`campaigns:ingdirect:home-loans`))
  ) %>%
  dplyr::mutate(
    Product_pages_target = Product_pages_web * 1.23
  ) %>%
  dplyr::select(Month, Product_pages_web, Product_pages_app, Calculator, Campaign_pages, Product_pages_target) %>%
  dplyr::arrange(Month, decreasing=T)

HL_total_dif_pages <- write_csv(HL_total_dif_pages, "HL_total_dif_pages.csv")

HL_PP_app <- read.csv("~/Documents/hustlers/data/Home Loan traffic - product pages - app - Mobile- Native App - Feb 16, 2023.csv", skip = 12)
HL_PP_web <- read.csv("~/Documents/hustlers/data/Home Loan traffic - product pages - web - ING Direct Production Report Suite - Feb 16, 2023.csv", skip = 10)
HL_traffic_sources_web <- read.csv("~/Documents/hustlers/data/PP - traffic resources - ING Direct Production Report Suite - Feb 16, 2023.csv", skip = 11)
# conversion part
HL_conv_web <- read.csv("~/Documents/hustlers/data/Home Loan traffic -conversion - ING Direct Production Report Suite - Feb 16, 2023.csv", skip = 10)
HL_conv_traffic <- read.csv("~/Documents/hustlers/data/Home Loan traffic -conversion (traffic source) - ING Direct Production Report Suite - Feb 16, 2023.csv", skip = 11)
# sales
sales <- read.csv("~/Documents/hustlers/data/Digital Sales Daily_data.csv")


sales <- sales %>%
  dplyr::mutate(Category = case_when(
    Product.Name == "INSURANCE" ~ "Insurance",
    Product.Name == "LIVING SUPER" ~ "Superannuation",
    Product.Name == "MORTGAGES" ~ "Mortgages",
    Product.Name == "ORANGE EVERYDAY" ~ "Current account",
    Product.Name == "ORANGE ONE" ~ "SomeProduct",
    Product.Name == "PTD" ~ "SomeProduct",
    Product.Name == "SA" ~ "SomeProduct",
    Product.Name == "SM" ~ "SomeProduct",
    Product.Name == "PERSONAL LOANS" ~ "SomeProduct")
  ) %>%
  dplyr::mutate(
    Date = as.Date(Day.of.Calendar.Date, format="%d %B %Y")
  ) %>%
  dplyr::mutate(
    week_nr = week(ymd(Date)),
    year = year(Date),
    Month = format(Date, "%B")
    ) %>%
  dplyr::mutate(
    week_year = str_c(week_nr, year, sep = "_")
  ) %>%
  dplyr::mutate(
    N2BvsE2B = case_when(
      NTB..group. == "Existing" ~ "E2B",
      NTB..group. == "NTB" ~ "N2B",
      NTB..group. == "NULL" ~ "UNK",
      is.na(NTB..group.) ~ "UNK"
    )
  ) %>%
  dplyr::mutate(Device = case_when(
    Channel.Description =="E-QUOTE" ~ "EQuote",
    Channel.Description =="NOT SUPPLIED" ~ "UNK",
    Channel.Description =="BROKER" ~ "Broker",
    Channel.Description =="DIRECT" ~ "Direct",
    Channel.Description =="MOBILE APP" ~ "app",
    Channel.Description =="ONLINE" ~ "web"
  )) %>%
  dplyr::select(Category, week_year, Sales, Month, N2BvsE2B, Device) %>%
  dplyr::filter(Category =="Mortgages") %>%
  dplyr::filter(Device =="Direct") %>%
  dplyr::group_by(week_year, Month, Category, N2BvsE2B) %>%
  dplyr::summarise(
    Sales = sum(Sales)
  ) %>% 
  dplyr::ungroup()

sales <- write_csv(sales, "sales.csv")

HL_PP_app_OA <- HL_PP_app[,1:3]
HL_PP_app_MS <- HL_PP_app[,c(1,4:5)]  
HL_PP_app_FR <- HL_PP_app[,c(1,6:7)] 

HL_PP_web_OA <- HL_PP_web[,1:4]
HL_PP_web_MS <- HL_PP_web[,c(1,5:6)]  
HL_PP_web_FR <- HL_PP_web[,c(1,7:8)] 

HL_conv_web_PP <- HL_conv_web[,1:4]
HL_conv_web_SF <- HL_conv_web[,c(1,5:7)]
HL_conv_web_FF <- HL_conv_web[,c(1,8:10)]

HL_conv_web_SEO <- HL_conv_traffic[,1:4]
HL_conv_web_Direct <- HL_conv_traffic[,c(1,5:7)]
HL_conv_web_SEA <- HL_conv_traffic[,c(1,8:10)]
HL_conv_web_referrals <- HL_conv_traffic[,c(1,11:13)]
HL_conv_web_affiliates <- HL_conv_traffic[,c(1,14:16)]


## Conversion traffic sources ##
HL_conv_web_SEO <- HL_conv_web_SEO  %>%
  dplyr::rename(
    "week" =X,
    "Product page" =Natural.Search,
    "Start application" =Natural.Search.1,
    "Finish application" =Natural.Search.2
  ) %>%
  dplyr::mutate(
    `Traffic source` = "SEO",
    Device = "web"
  )
HL_conv_web_SEO <- HL_conv_web_SEO[-c(1:2),]

HL_conv_web_SEA <- HL_conv_web_SEA  %>%
  dplyr::rename(
    "week" =X,
    "Product page" =Paid.Search,
    "Start application" =Paid.Search.1,
    "Finish application" =Paid.Search.2
  ) %>%
  dplyr::mutate(
    `Traffic source` = "SEA",
    Device = "web"
  )
HL_conv_web_SEA <- HL_conv_web_SEA[-c(1:2),]

HL_conv_web_Direct <- HL_conv_web_Direct  %>%
  dplyr::rename(
    "week" =X,
    "Product page" =Direct,
    "Start application" =Direct.1,
    "Finish application" =Direct.2
  ) %>%
  dplyr::mutate(
    `Traffic source` = "Direct",
    Device = "web"
  )
HL_conv_web_Direct <- HL_conv_web_Direct[-c(1:2),]

HL_conv_web_referrals <- HL_conv_web_referrals  %>%
  dplyr::rename(
    "week" =X,
    "Product page" =Referring.Domains,
    "Start application" =Referring.Domains.1,
    "Finish application" =Referring.Domains.2
  ) %>%
  dplyr::mutate(
    `Traffic source` = "referrals",
    Device = "web"
  )
HL_conv_web_referrals <- HL_conv_web_referrals[-c(1:2),]

HL_conv_web_affiliates <- HL_conv_web_affiliates  %>%
  dplyr::rename(
    "week" =X,
    "Product page" =Affiliate,
    "Start application" =Affiliate.1,
    "Finish application" =Affiliate.2
  ) %>%
  dplyr::mutate(
    `Traffic source` = "affiliates",
    Device = "web"
  )
HL_conv_web_affiliates <- HL_conv_web_affiliates[-c(1:2),]


HL_conv_web_traf <- dplyr::full_join(HL_conv_web_SEO, HL_conv_web_SEA)
HL_conv_web_traf <- dplyr::full_join(HL_conv_web_traf, HL_conv_web_Direct)
HL_conv_web_traf <- dplyr::full_join(HL_conv_web_traf, HL_conv_web_affiliates)
HL_conv_web_traf <- dplyr::full_join(HL_conv_web_traf, HL_conv_web_referrals)

HL_conv_web_traf <- HL_conv_web_traf %>%
  dplyr::mutate(
    week = as.Date(week, format="%Y-%m-%d"),
    week_nr = week(ymd(week)),
    year = year(week),
    `Product page` = as.numeric(as.character(`Product page`)),
    `Start application` = as.numeric(as.character(`Start application`)),
    `Finish application` = as.numeric(as.character(`Finish application`))
  ) %>%
  dplyr::mutate(
    week_year = str_c(week_nr, year, sep = "_")
  ) %>%
  dplyr::group_by(week_year, `Traffic source`, Device) %>%
  dplyr::summarise(
    `Product page` = sum(`Product page`),
    `Start application` = sum(`Start application`),
    `Finish application` = sum(`Finish application`)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1 = round((as.numeric(as.character(`Start application`)) /as.numeric(as.character(`Product page`))), digits=2) *100,
    con_2 = round((as.numeric(as.character(`Finish application`)) / as.numeric(as.character(`Start application`))), digits=2) *100
  )
  


## TRAFFIC SOURCES##

HL_traffic_sources_web <- HL_traffic_sources_web[-c(1:2),]

HL_traffic_sources_web <- HL_traffic_sources_web %>%
  dplyr::mutate(
    week = as.Date(X, format="%Y-%m-%d"),
    SEO = as.numeric(as.character(Natural.Search)),
    SEA = as.numeric(as.character(Paid.Search)),
    Direct = as.numeric(as.character(Direct)),
    Referrals = as.numeric(as.character(Referring.Domains)),
    Affiliates = as.numeric(as.character(Affiliate)),
    product = "all",
    N2BvsE2B = "N2BandE2B",
    Month = format(week, "%B"),
    Device ="web"
  ) %>%
  dplyr::select(week, SEO, SEA, Direct, Referrals, Affiliates, product, N2BvsE2B, Month, Device) %>%
  tidyr::gather(key =traffic, `Product page`, SEO:Affiliates, factor_key = T) %>%
  dplyr::mutate(
    week_nr = week(ymd(week)),
    year = year(week),
  ) %>%
  dplyr::mutate(
    week_year = str_c(week_nr, year, sep = "_")
  ) %>%
  dplyr::select(-week, -week_nr, -year)



## APP ##
HL_PP_app_OA <- HL_PP_app_OA %>%
  dplyr::mutate(
    product = "Orange Advantage"
  ) %>%
  dplyr::rename(
    "week" =X,
    "visits" =Orange.Advantage,
    "CTA" =Orange.Advantage.1
  )

HL_PP_app_OA <- HL_PP_app_OA[-c(1:2),]

HL_PP_app_MS <- HL_PP_app_MS %>%
  dplyr::mutate(
    product = "Mortgage simplifier"
  ) %>%
  dplyr::rename(
    "week" =X,
    "visits" =Mortgage.Simplifier,
    "CTA" =Mortgage.Simplifier.1
  )

HL_PP_app_MS <- HL_PP_app_MS[-c(1:2),]

HL_PP_app_FR <- HL_PP_app_FR %>%
  dplyr::mutate(
    product = "Fixed rate"
  ) %>%
  dplyr::rename(
    "week" = X,
    "visits" =Fixed.Rate,
    "CTA" =Fixed.Rate.1
  )

HL_PP_app_FR <- HL_PP_app_FR[-c(1:2),]

HL_PP_app_comb <- dplyr::full_join(HL_PP_app_OA, HL_PP_app_MS)
HL_PP_app_comb <- dplyr::full_join(HL_PP_app_comb, HL_PP_app_FR)

HL_PP_app_comb <- HL_PP_app_comb %>%
  dplyr::mutate(
    N2BvsE2B = "E2B",
    Device = "Native_app",
    Category = "Mortgages"
  )

## WEB ##
HL_PP_web_OA <- HL_PP_web_OA %>%
  dplyr::mutate(
    product = "Orange Advantage"
  ) %>%
  dplyr::rename(
    "week" =X,
    "N2BandE2B" =ingdirect.home.loans,
    "N2B" =ingdirect.home.loans.orange.advantage,
    "E2B" =ingdirect.banking.products.home.loans.orange.advantage
  )

HL_PP_web_OA <- HL_PP_web_OA[-c(1:2),]

HL_PP_web_OA <- HL_PP_web_OA %>%
  tidyr::gather(key =N2BvsE2B, visits, N2BandE2B:E2B, factor_key = T)

HL_PP_web_MS <- HL_PP_web_MS %>%
  dplyr::mutate(
    product = "Mortgage simplifier"
  ) %>%
  dplyr::rename(
    "week" =X,
    "N2B" =ingdirect.home.loans.mortgage.simplifier,
    "E2B" =ingdirect.banking.products.home.loans.mortgage.simplifier
  )

HL_PP_web_MS <- HL_PP_web_MS[-c(1:2),]

HL_PP_web_MS <- HL_PP_web_MS %>%
  tidyr::gather(key =N2BvsE2B, visits, N2B:E2B, factor_key = T)

HL_PP_web_FR <- HL_PP_web_FR %>%
  dplyr::mutate(
    product = "Fixed rate"
  ) %>%
  dplyr::rename(
    "week" =X,
    "N2B" =ingdirect.home.loans.fixed.rate.home.loan,
    "E2B" =ingdirect.banking.products.home.loans.fixed.rate.home.loan
  )

HL_PP_web_FR <- HL_PP_web_FR[-c(1:2),]

HL_PP_web_FR <- HL_PP_web_FR %>%
  tidyr::gather(key =N2BvsE2B, visits, N2B:E2B, factor_key = T)

HL_PP_web_comb <- dplyr::full_join(HL_PP_web_OA, HL_PP_web_MS)
HL_PP_web_comb <- dplyr::full_join(HL_PP_web_comb, HL_PP_web_FR)

HL_PP_web_comb <- HL_PP_web_comb %>%
  dplyr::mutate(
    Category = "Mortgages",
    Device = "web"
  )

## APP & WEB ##
HL_PP_comb <- dplyr::full_join(HL_PP_web_comb, HL_PP_app_comb)

HL_PP_comb <- HL_PP_comb %>%
  dplyr::mutate(
   visits = as.numeric(as.character(visits)),
   Date = as.Date(week, format="%Y-%m-%d"),
   week = as.Date(week, format="%Y-%m-%d"),
   Month = format(Date, "%B")
  ) %>%
  dplyr::mutate(
    week_nr = week(ymd(week)),
    year = year(week),
  ) %>%
  dplyr::mutate(
    week_year = str_c(week_nr, year, sep = "_")
  ) %>%
  dplyr::select(-week, -week_nr, -year)


## CONV WEB traffic E2B vs N2B ##

HL_conv_web_PP <- HL_conv_web_PP  %>%
  dplyr::rename(
    "week" =X,
    "N2B" =Product...HL.1,
    "E2B" =Product...HL,
    "UNK" =Product...HL.2
  )

HL_conv_web_PP <- HL_conv_web_PP[-c(1:3),]

HL_conv_web_PP <- HL_conv_web_PP %>%
  tidyr::gather(key =N2BvsE2B, visits, E2B:UNK, factor_key = T) %>%
  dplyr::rename(
    "Product page" = visits
  )


HL_conv_web_SF <- HL_conv_web_SF  %>%
  dplyr::rename(
    "week" =X,
    "E2B" =Product...HL.3,
    "N2B" =Product...HL.4,
    "UNK" =Product...HL.5
  )

HL_conv_web_SF <- HL_conv_web_SF[-c(1:3),]

HL_conv_web_SF <- HL_conv_web_SF %>%
  tidyr::gather(key =N2BvsE2B, visits, E2B:UNK, factor_key = T) %>%
  dplyr::rename(
    "Start application" = visits
  )

HL_conv_web_FF <- HL_conv_web_FF  %>%
  dplyr::rename(
    "week" =X,
    "E2B" =Product...HL.6,
    "N2B" =Product...HL.7,
    "UNK" =Product...HL.8
  )

HL_conv_web_FF <- HL_conv_web_FF[-c(1:3),]

HL_conv_web_FF <- HL_conv_web_FF %>%
  tidyr::gather(key =N2BvsE2B, visits, E2B:UNK, factor_key = T) %>%
  dplyr::rename(
    "Finish application" = visits
  )

## COMBINE CONVERSION ##

HL_conv_comb <- dplyr::full_join(HL_conv_web_PP, HL_conv_web_SF)
HL_conv_comb <- dplyr::full_join(HL_conv_comb, HL_conv_web_FF)

HL_conv_comb <- HL_conv_comb %>%
  dplyr::mutate(
    `Product page` = as.numeric(as.character(`Product page`)),
    `Start application` = as.numeric(as.character(`Start application`)),
    `Finish application` = as.numeric(as.character(`Finish application`)),
    Date = as.Date(week, format="%Y-%m-%d"),
    Month = format(Date, "%B")
  ) %>%
  dplyr::mutate(
    week_nr = week(ymd(week)),
    year = year(Date)
  ) %>%
  dplyr::mutate(
    week_year = str_c(week_nr, year, sep = "_")
  ) %>%
  dplyr::select(-week_nr, -year)


HL_conv_comb <- HL_conv_comb %>%
  dplyr::mutate(
    Category = "Mortgages",
    Device = "web",
    con_1 = round((as.numeric(as.character(`Start application`)) /as.numeric(as.character(`Product page`))), digits=2) *100,
    con_2 = round((as.numeric(as.character(`Finish application`)) / as.numeric(as.character(`Start application`))), digits=2) *100,
    week = as.Date(week, format="%Y-%m-%d"),
    ) %>% 
  dplyr::select(-Date, -week)

HL_conv_comb1 <- dplyr::left_join(HL_conv_comb, sales, by =c("week_year", "N2BvsE2B", "Month", "Category"))

# Connect conv with sources #
#HL_conv_tool <- dplyr::full_join(HL_conv_comb, HL_traffic_sources_web)
# One file for conversion (as does not contain product split)
HL_conv_tool <- write_csv(HL_conv_comb1, "HL_conv_comb.csv")


HL_conv_web_traf1 <- write_csv(HL_conv_web_traf, "HL_conv_web_traf.csv")

# Connect PP with sources #
HL_PP_comb <- dplyr::full_join(HL_PP_comb, HL_traffic_sources_web)

# One file for product pages 
HL_PP_comb <- write_csv(HL_PP_comb, "HL_PP_comb.csv")
