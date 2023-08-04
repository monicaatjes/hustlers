

sales <- read_excel("~/Documents/hustlers/data/DigitalSales_daily.xlsx")



## sales - month level - including client vs non client split ##

# attemopt 1: total sales: YoY

sales_month_overview_yoy <- sales %>%
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
    year_date = cut(as.POSIXct(Date), "year")
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
  dplyr::select(Category, year_date, Sales, `Visitor or Client`, Device) %>%
  dplyr::filter(Device != "Broker") %>%
  dplyr::filter(`Visitor or Client` =="client") %>%
  dplyr::group_by(year_date, Category) %>%
  dplyr::summarise(
    Sales = sum(Sales)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    year = year(year_date)
  ) %>% 
  tidyr::spread(Category, Sales) %>%
  dplyr::arrange(year_date)



p1 <- plot_ly(sales_month_overview_yoy, x= ~unique(month_Date),
         y = ~na.omit(`Current account`), 
         type='scatter', 
         mode ='lines',
         name="Current account",
         marker = list(color = 'rgb(82,81,153)'),
         line = list(color = 'rgb(82,81,153)')) %>%
  add_trace(y = ~`Credit card`, 
            type='scatter', 
            mode ='lines',
            name="Credit card",
            marker = list(color = 'rgb(51, 51, 51)'),
            line = list(color = 'rgb(51, 51, 51)')) %>%
  add_trace(y = ~`Mortgages`, 
            type='scatter', 
            mode ='lines',
            name="Mortgages",
            marker = list(color = 'rgb(85, 155, 209)'),
            line = list(color = 'rgb(85, 155, 209)')) %>%
  add_trace(y = ~`Personal loan`, 
            type='scatter', 
            mode ='lines',
            name="Personal loan",
            marker = list(color = 'rgb(171, 0, 102)'),
            line = list(color = 'rgb(171, 0, 102)')) %>%
  add_trace(y = ~`Personal Term Deposit`, 
            type='scatter', 
            mode ='lines',
            name="Personal Term Deposit",
            marker = list(color = 'rgb(208, 217, 60)'),
            line = list(color = 'rgb(208, 217, 60)')) %>%
  add_trace(y = ~`Savings Accounts`, 
            type='scatter', 
            mode ='lines',
            name="Savings Accounts",
            marker = list(color = 'rgb(52, 150, 81)'),
            line = list(color = 'rgb(52, 150, 81)')) %>%
  add_trace(y = ~`Savings Maximiser`, 
            type='scatter', 
            mode ='lines',
            name="Savings Maximiser",
            marker = list(color = 'rgb(255, 98, 0)'),
            line = list(color = 'rgb(255, 98, 0)')) %>%
  add_trace(y = ~`Superannuation`, 
            type='scatter', 
            mode ='lines',
            name="Superannuation",
            marker = list(color = 'rgb(215, 0,0)'),
            line = list(color = 'rgb(215, 0,0)')) %>%
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
           legend =list(orientation = 'left', font = list(family = "ING me", size = 12, color = 'rgb(105, 105, 105)')))
  p1





  # attemopt 2: total sales: YoY without product split
  
  sales_month_overview_yoy <- sales %>%
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
      year_date = cut(as.POSIXct(Date), "year")
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
    dplyr::select(Category, year_date, Sales, `Visitor or Client`, Device) %>%
    dplyr::filter(Device != "Broker") %>%
    dplyr::group_by(year_date, `Visitor or Client`) %>%
    dplyr::summarise(
      Sales = sum(Sales)
    ) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(
      year = year(year_date)
    ) %>% 
    tidyr::spread(`Visitor or Client`, Sales) %>%
    dplyr::arrange(year_date)

  
conv <- read_excel("~/Documents/hustlers/Mobile Product Pages Conversion Summary - Mobile- Native App - Jul 13, 2023.xlsx")


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
   )