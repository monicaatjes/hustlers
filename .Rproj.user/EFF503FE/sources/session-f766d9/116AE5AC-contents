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

## conv ##
HL_total_conversion <- read_excel("~/Documents/hustlers/data/HL_conversion.xlsx", sheet="HL_conversion")
HL_total_traffic_web <- read_excel("~/Documents/hustlers/data/HL_conversion.xlsx", sheet="HL_main_traffic")
HL_total_traffic_app <- read_excel("~/Documents/hustlers/data/HL_conversion.xlsx", sheet="Native_app_pp")
HL_total_con_markchan <- read_excel("~/Documents/hustlers/data/HL_conversion.xlsx", sheet="HL_marketingchan")

HL_traf_targ <- read_excel("~/Documents/hustlers/data/HL_conversion.xlsx", sheet="HL_traf")



sales <- read_excel("~/Documents/hustlers/data/DigitalSales_daily.xlsx")

## converting sales ############################################

## sales - week level - including client vs non client split ##
sales_week <- sales %>%
  dplyr::mutate(Category = case_when(
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
    Date = as.Date(Last_Refreshed, format="%d-%m-%Y")
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
  dplyr::select(Category, week_Date, Sales, `Visitor or Client`, Device) %>%
  dplyr::filter(Category =="Mortgages") %>%
  dplyr::filter(Device =="Direct") %>%
  dplyr::group_by(week_Date, Category, `Visitor or Client`) %>%
  dplyr::summarise(
    Sales = sum(Sales)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    Date = as.Date(week_Date)
  )

sales_week$MonthvsWeek <- "Week"

## sales - month level - including client vs non client split ##

sales_month <- sales %>%
  dplyr::mutate(Category = case_when(
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
  dplyr::select(Category, month_Date, Sales, `Visitor or Client`, Device) %>%
  dplyr::filter(Category =="Mortgages") %>%
  dplyr::filter(Device =="Direct") %>%
  dplyr::group_by(month_Date, Category, `Visitor or Client`) %>%
  dplyr::summarise(
    Sales = sum(Sales)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    Date = as.Date(month_Date)
  )

sales_month$MonthvsWeek <- "Month"

sales <- dplyr::full_join(sales_week, sales_month) %>%
  dplyr::select(-week_Date, -month_Date)

## sales - total level ##

sales_total <- sales %>%
  dplyr::group_by(Category, Date, MonthvsWeek) %>%
  dplyr::summarise(
    Sales = sum(Sales)
  )
sales_total$`Visitor or Client` <- "total"

sales <- dplyr::full_join(sales, sales_total)


## create targets #############################################

HL_traf_targ <- HL_traf_targ %>%
  dplyr::filter(`Sub Section`!="products") %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::group_by(year, month_Date, month) %>%
  dplyr::summarise(
    `Product page target` = sum(Visits),
  ) %>%
  dplyr::ungroup() 

act <- c(48870,
         49676,
         47273,
         42940,
         54134,
         55868,
         68059,
         70011,
         59424,
         61619,
         56625,
         48257)


HL_traf_targ$act <- act
HL_traf_targ <- HL_traf_targ %>%
  dplyr::mutate(
    dif = act/ `Product page target`
  ) %>%
  dplyr::mutate(
    target_PP_23 =  act *1.07,
    Date = month_Date,
    `Product page target` = `Product page target` * dif,
    `Visitor or Client` ="total",
    MonthvsWeek ="Month"
  ) %>%
  dplyr::select(month, target_PP_23, `Visitor or Client`, MonthvsWeek,
                `Product page target`)


## CONV mark chan month ####################################

## mark_channel - month level - no client vs non client split, all total ##

HL_total_conversion_month_markchan <- HL_total_con_markchan %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::group_by(month_Date, `Last Touch Marketing Channel`) %>%
  dplyr::summarise(
    `Product page MC` = sum(Visits),
    `Start application MC` = sum(`HL App starts (Hustlers)`),
    `Finish application MC` = sum(`HL app complete`)
  ) %>%
  dplyr::mutate(
    `Product page MC` = `Product page MC` - `Start application MC`,
    `Start application MC` = `Start application MC`/1.2,
    `Finish application MC` = `Finish application MC`/1.1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1_MC = as.numeric(as.character(`Start application MC`)) /as.numeric(as.character(`Product page MC`)) *100,
    con_2_MC = as.numeric(as.character(`Finish application MC`)) / as.numeric(as.character(`Start application MC`)) *100
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::mutate(
    Date= month_Date
  ) %>%
  dplyr::select (-month_Date)

HL_total_conversion_month_markchan$MonthvsWeek <- "Month"
HL_total_conversion_month_markchan$`Visitor or Client` <- "total"

## CONV mark chan week ####################################

## mark_channel - week level -  ##

HL_total_conversion_week_markchan <- HL_total_con_markchan %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week")
  ) %>%
  dplyr::mutate(
    month_year = str_c(month, year, sep = "_"),
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::group_by(week_Date, `Last Touch Marketing Channel`) %>%
  dplyr::summarise(
    `Product page MC` = sum(Visits),
    `Start application MC` = sum(`HL App starts (Hustlers)`),
    `Finish application MC` = sum(`HL app complete`)
  ) %>%
  dplyr::mutate(
    `Product page MC` = `Product page MC` - `Start application MC`,
    `Start application MC` = `Start application MC`/1.2,
    `Finish application MC` = `Finish application MC`/1.1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1_MC = as.numeric(as.character(`Start application MC`)) /as.numeric(as.character(`Product page MC`)) *100,
    con_2_MC = as.numeric(as.character(`Finish application MC`)) / as.numeric(as.character(`Start application MC`)) *100
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::mutate(
    Date= week_Date
  ) %>%
  dplyr::select (-week_Date)

HL_total_conversion_week_markchan$MonthvsWeek <- "Week"
HL_total_conversion_week_markchan$`Visitor or Client` <- "total"

## CONV month total ####################################

## total - month level - no client vs non client split ##

HL_total_conversion_month_target_total <- HL_total_conversion %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::group_by(month_Date) %>%
  dplyr::summarise(
    `Product page CvnC` = sum(`Visits`),
    `Start application CvnC` = sum(`HL App starts (Hustlers)`),
    `Finish application CvnC` = sum(`HL app complete`)
  ) %>%
  dplyr::mutate(
    `Product page CvnC` = `Product page CvnC` - `Start application CvnC`,
    `Start application CvnC` = `Start application CvnC`/1.2,
    `Finish application CvnC` = `Finish application CvnC`/1.1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1_CvnC = (`Start application CvnC` /`Product page CvnC`) *100,
    con_2_CvnC = (`Finish application CvnC` / `Start application CvnC`) *100
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::mutate(
    target_con1 = 5,
    Date = month_Date
  ) %>%
  dplyr::select (-month_Date)

HL_total_conversion_month_target_total$MonthvsWeek <- "Month"
HL_total_conversion_month_target_total$`Visitor or Client` <- "total"

#HL_total_conversion_month_target_total <- dplyr::left_join(HL_total_conversion_month_target_total, HL_traf_targ)

## CONV month CvnC ####################################

## total - month level - client vs non client split ##

HL_total_conversion_month_target_CvnC <- HL_total_conversion %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::group_by(month_Date, `Visitor or Client`) %>%
  dplyr::summarise(
    `Product page CvnC` = sum(`Visits`),
    `Start application CvnC` = sum(`HL App starts (Hustlers)`),
    `Finish application CvnC` = sum(`HL app complete`)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1_CvnC = (`Start application CvnC` /`Product page CvnC`) *100,
    con_2_CvnC = (`Finish application CvnC` / `Start application CvnC`) *100
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::mutate(
   # target_PP = case_when(year=="2022" ~ `Product page` * 1.07),
    target_con1 = 5,
    Date = month_Date
    #target_FF = case_when(year=="2022" ~ `Finish application` * 1.38)
  ) %>%
  dplyr::select (-month_Date)

HL_total_conversion_month_target_CvnC$MonthvsWeek <- "Month" 

#HL_total_conversion_month_target <- dplyr::full_join(HL_total_conversion_month_target_CvnC, HL_total_conversion_month_target_total)

## CONV week total ####################################

## total - week level - client vs non client all total ##

HL_total_conversion_week_total <- HL_total_conversion %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week")
  ) %>%
   dplyr::mutate(
     `Device Type` = case_when(
       `Device Type` =="Tablet portrait" ~ "Tablet",
       `Device Type` =="Tablet landscape" ~ "Tablet",
       `Device Type` =="Mobile/Smartphone" ~ "Mobile",
       TRUE ~ `Device Type`
     )
   ) %>%
  dplyr::mutate(
    month_year = str_c(month, year, sep = "_"),
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::group_by(week_Date) %>%
  dplyr::summarise(
    `Product page CvnC` = sum(`Visits`),
    `Start application CvnC` = sum(`HL App starts (Hustlers)`),
    `Finish application CvnC` = sum(`HL app complete`)
  ) %>%
  dplyr::mutate(
    `Product page CvnC` = `Product page CvnC` - `Start application CvnC`,
    `Start application CvnC` = `Start application CvnC`/1.2,
    `Finish application CvnC` = `Finish application CvnC`/1.1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1_CvnC = (`Start application CvnC` /`Product page CvnC`) *100,
    con_2_CvnC = (`Finish application CvnC` / `Start application CvnC`) *100
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::mutate(
    #target_PP = case_when(year=="2022" ~ `Product page` * 1.07),
    target_con1 = 5,
    Date = week_Date
   # target_FF = case_when(year=="2022" ~ `Finish application` * 1.38)
  ) %>%
  dplyr::select (-week_Date)

HL_total_conversion_week_total$MonthvsWeek <- "Week"
HL_total_conversion_week_total$`Visitor or Client` <- "total"

## CONV week CvnC ####################################

## total - week level - client vs non client split ##

HL_total_conversion_week_CvnC <- HL_total_conversion %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::mutate(
    `Device Type` = case_when(
      `Device Type` =="Tablet portrait" ~ "Tablet",
      `Device Type` =="Tablet landscape" ~ "Tablet",
      `Device Type` =="Mobile/Smartphone" ~ "Mobile",
      TRUE ~ `Device Type`
    )
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::group_by(week_Date, `Visitor or Client`) %>%
  dplyr::summarise(
    `Product page CvnC` = sum(`Visits`),
    `Start application CvnC` = sum(`HL App starts (Hustlers)`),
    `Finish application CvnC` = sum(`HL app complete`)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1_CvnC = (`Start application CvnC` /`Product page CvnC`) *100,
    con_2_CvnC = (`Finish application CvnC` / `Start application CvnC`) *100
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::mutate(
    #target_PP = case_when(year=="2022" ~ `Product page` * 1.07),
    target_con1 = 5,
    Date = week_Date
    #target_FF = case_when(year=="2022" ~ `Finish application` * 1.38)
  ) %>%
  dplyr::select (-week_Date)

HL_total_conversion_week_CvnC$MonthvsWeek <- "Week"

### Combine conversion ###

HL_total_conv_mark_chan <- dplyr::full_join(HL_total_conversion_week_markchan, HL_total_conversion_month_markchan)

#HL_cvsv_conv_mark_chan <- dplyr::full_join(HL_conv_month_markchan_cvsv, HL_conv_week_markchan_cvsv)

#HL_total_conv_mark_chan <- dplyr::full_join(HL_total_conv_mark_chan, HL_cvsv_conv_mark_chan)

target <-dplyr::full_join(HL_total_conversion_week_total, HL_total_conversion_month_target_total)

HL_total_conv_CvnC <- dplyr::full_join(HL_total_conversion_month_target_CvnC, HL_total_conversion_week_CvnC)                                         

## split cvnc & month, week & sales (not target for traffic)
HL_TOTAL_CvnC <- dplyr::full_join(HL_total_conv_CvnC, target) 
HL_TOTAL_CvnC$Category <- "Mortgages"


HL_TOTAL_CvnC$Date <- as.Date(HL_TOTAL_CvnC$Date)

HL_TOTAL_CvnC_sal <- dplyr::left_join(HL_TOTAL_CvnC, sales) 



# mutate traffic app
HL_total_traffic_app$`Visitor or Client` <- "client"

HL_total_traffic <- dplyr::full_join(HL_total_traffic_web, HL_total_traffic_app) 

HL_total_traffic_total <- HL_total_traffic %>%
  dplyr::mutate(page_category = case_when(
  Page = 
    str_detect(Page, "ingdirect:home-loans:f") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:m") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:o") ~ "Product page open",
    str_detect(Page, "ingdirect:banking:products:home-loans") ~ "Product page secured",
    str_detect(Page, "campaigns") ~ "Campaign page",
    str_detect(Page, "native_app:banking:products") ~ "Product page app",
    str_detect(Page, "home-loans:calculators") ~ "Calculators",
    str_detect(Page, "ingdirect:home-loans") ~ "Product page open",
   )
  ) %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::group_by(week_Date, page_category) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::spread(page_category, Visits) %>%
  dplyr::mutate(
    Date = week_Date,
    `Product page target` = `Campaign page` + `Product page open` + `Product page secured`
  )

HL_total_traffic_total$`Visitor or Client` <- "total"

HL_total_traffic_total <- HL_total_traffic_total %>%
 # dplyr::filter(!is.na(`Product page open`)) %>%
  dplyr::select(-week_Date)

HL_total_traffic_total$MonthvsWeek <- "Week"

HL_total_traffic_CvnC <- HL_total_traffic %>%
  dplyr::mutate(page_category = case_when(
    Page = 
    str_detect(Page, "ingdirect:home-loans:f") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:m") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:o") ~ "Product page open",
    str_detect(Page, "ingdirect:banking:products:home-loans") ~ "Product page secured",
    str_detect(Page, "campaigns") ~ "Campaign page",
    str_detect(Page, "native_app:banking:products") ~ "Product page app",
    str_detect(Page, "home-loans:calculators") ~ "Calculators",
    str_detect(Page, "ingdirect:home-loans") ~ "Product page open",
  )
  ) %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::mutate(`Visitor or Client` = case_when(
    str_detect(Page, "native") ~ "client",
    TRUE ~ `Visitor or Client`
  )) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::group_by(week_Date, `Visitor or Client`, page_category) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::spread(page_category, Visits) %>%
  dplyr::mutate(
    Date = week_Date,
    `Product page target` = `Campaign page` + `Product page open` + `Product page secured`
  )

HL_total_traffic_CvnC$MonthvsWeek <- "Week"

HL_total_traffic_CvnC <- HL_total_traffic_CvnC %>%
  dplyr::select(-week_Date)


##
HL_total_traffic_months <- HL_total_traffic %>%
  dplyr::mutate(page_category = case_when(
    Page = 
    str_detect(Page, "ingdirect:home-loans:f") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:m") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:o") ~ "Product page open",
    str_detect(Page, "ingdirect:banking:products:home-loans") ~ "Product page secured",
    str_detect(Page, "campaigns") ~ "Campaign page",
    str_detect(Page, "native_app:banking:products") ~ "Product page app",
    str_detect(Page, "home-loans:calculators") ~ "Calculators",
    str_detect(Page, "ingdirect:home-loans") ~ "Product page open")
  ) %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::group_by(month_Date, page_category) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::spread(page_category, Visits) %>%
  dplyr::mutate(
    Date = month_Date,
    `Product page target` = `Campaign page` + `Product page open` + `Product page secured`
  )

HL_total_traffic_months$`Visitor or Client` <- "total"
HL_total_traffic_months$MonthvsWeek <- "Month"

HL_total_traffic_months <- HL_total_traffic_months %>%
  dplyr::select(-month_Date)

HL_total_traffic_CvnC_months <- HL_total_traffic %>%
  dplyr::mutate(page_category = case_when(
    Page = 
    str_detect(Page, "ingdirect:home-loans:f") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:m") ~ "Product page open",
    str_detect(Page, "ingdirect:home-loans:o") ~ "Product page open",
    str_detect(Page, "ingdirect:banking:products:home-loans") ~ "Product page secured",
    str_detect(Page, "campaigns") ~ "Campaign page",
    str_detect(Page, "native_app:banking:products") ~ "Product page app",
    str_detect(Page, "home-loans:calculators") ~ "Calculators",
    str_detect(Page, "ingdirect:home-loans") ~ "Product page open")
  ) %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::group_by(month_Date, `Visitor or Client`, page_category,) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::spread(page_category, Visits) %>%
  dplyr::mutate(
    Date = month_Date,
    `Product page target` = `Campaign page` + `Product page open` + `Product page secured`
  ) 

HL_total_traffic_CvnC_months$MonthvsWeek <- "Month"

HL_total_traffic_CvnC_months <- HL_total_traffic_CvnC_months %>%
  dplyr::select(-month_Date)
##


HL_total_traffic_totall <- dplyr::full_join(HL_total_traffic_total, HL_total_traffic_CvnC)
HL_total_traffic_totall <- dplyr::full_join(HL_total_traffic_totall, HL_total_traffic_CvnC_months)
HL_total_traffic_totall <- dplyr::full_join(HL_total_traffic_totall, HL_total_traffic_months)

HL_total_traffic_totall <- HL_total_traffic_totall %>%
  dplyr::mutate(
    month = format(as.Date(Date), "%B")
  ) 

HL_total_traffic_totall <- dplyr::full_join(HL_total_traffic_totall, HL_traf_targ)

HL_total_traffic_totall <- HL_total_traffic_totall %>%
 # dplyr::arrange(month) %>%
  dplyr::group_by(month) %>%
  fill(target_PP_23, .direction = c( "up")) %>%
  dplyr::ungroup() %>%
  dplyr::filter(`Visitor or Client` !="::unspecified::") %>%
  dplyr::filter(!is.na(Date))


HL_traf_tool3 <- write_csv(HL_total_traffic_totall, "HL_traf_tool3.csv")
#HL_conv_week_tool3 <- write_csv(HL_total_conversion_week, "HL_conv_week_tool3.csv")
#HL_conv_month_tool3 <- write_csv(HL_total_conversion_month_target, "HL_conv_month_tool3.csv")
#HL_conv_tool3 <- write_csv(HL_total_conv, "HL_conv_tool3.csv")  

## markchan


## conv + sales
HL_conv_tool34 <- write_csv(HL_TOTAL_CvnC_sal, "HL_conv_tool34.csv") 
HL_conv_tool35 <- write_csv(HL_total_conv_mark_chan, "HL_conv_tool35.csv")

HL_conv_tool35$Date <- as.Date(HL_conv_tool35$Date)

testing <- full_join(HL_conv_tool34, HL_conv_tool35)

HL_traf_tool3$Date <- as.Date(HL_traf_tool3$Date)

testing <- full_join(testing, HL_traf_tool3) %>%
  dplyr::filter(`Visitor or Client` != "None") #%>%
 # dplyr::filter(!is.na(month))

testing <- write_csv(testing, "testing.csv")
