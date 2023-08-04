
rates <- read_excel("~/Documents/hustlers/data/ING Rate Changes 01012023 to 19062023 (excl RBA).xlsx", range="A2:B12")

rates <- rates %>%
  dplyr::mutate(
  date = as.Date(Date, format="%d-%m-%Y")
  ) %>%
  dplyr::mutate(
    Date = cut(as.POSIXct(date), "week")
  ) %>%
  dplyr::mutate(
    rate_direction = case_when(
      str_detect(`Change +/-` , "OA -") ~ "down",
      str_detect(`Change +/-` , "OA +") ~ "up",
      str_detect(`Change +/-` , "FRL +") ~ "up"
    )
  ) %>%
  dplyr::select(date, rate_direction) %>%
  dplyr::rename(
    Date = date
  ) %>%
  as.data.frame() %>%
  dplyr::mutate(
    interest = case_when(
      Date =="2023-02-02" ~ 5 -0.2,
      Date =="2023-02-14" ~ 4.98 +0.25,
      Date =="2023-03-02" ~ 5.23 +0.25,
      Date =="2023-03-14" ~ 5.48 +0.25,
      Date =="2023-03-30" ~ 5.73- 0.2,
      Date =="2023-05-09" ~ 5.53 + 0.25,
      Date =="2023-05-16" ~ 5.78 + 0.1,
      Date =="2023-06-01" ~ 5.88 + 0.2,
      Date =="2023-06-06" ~ 6.08 + 0.1,
      Date =="2023-06-14" ~ 6.18 + 0.25
    )
  )


  
MonthvsWeek <- filter(testing,
                      MonthvsWeek == "Week",
                      #Date > "2023-03-06",
                      `Visitor or Client` == "total",
                      !is.na(`Product page MC`),
                      !is.na(con_1_CvnC),
                      con_1_CvnC >0,
                      !is.na(Date),
                      !is.na(`Last Touch Marketing Channel`)) %>%
  as.data.frame()

MonthvsWeek <- dplyr::full_join(MonthvsWeek, rates) %>%
  dplyr::arrange(Date) %>%
  #dplyr::filter(Date > "2023-03-06") %>%
  fill(interest, .direction ="up") %>%
  dplyr::filter(is.na(rate_direction), !is.na(con_1_CvnC))
  
  





fig <- plot_ly(MonthvsWeek, x = ~unique(Date)) %>%
  
  add_trace(
      y = ~con_1_MC[`Last Touch Marketing Channel`=="Natural Search"], 
      name = 'Natural Search', type = 'scatter', mode = 'lines+markers',
      marker = list(color = 'rgb(255, 98,0)'),
      line = list(color = 'rgb(255, 98,0)', width = 4)) %>%
  add_trace(y = ~con_1_MC[`Last Touch Marketing Channel`=="Paid Search"], 
      type='scatter', 
      mode ='lines+markers',
      name="Paid Search",
      marker = list(color = 'rgb(52, 150, 81)'),
      line = list(color = 'rgb(52, 150, 81)', width = 4)) %>%
  add_trace(y = ~con_1_MC[`Last Touch Marketing Channel`=="Direct"], 
      type='scatter', 
      mode ='lines+markers',
      name="Direct",
      marker = list(color = 'rgb(85, 155, 209)'),
      line = list(color = 'rgb(85, 155, 209)', width = 4)) %>%
 # add_trace(y = ~interest[`Last Touch Marketing Channel`=="Direct"],
#      type='scatter', 
 #     mode ='lines+markers',
  #    name="interest",
   #   marker = list(color = 'red'),
    #  line = list(color = 'red', width = 4)) %>%
  
 # add_trace(y = ~con_1_MC[`Last Touch Marketing Channel`=="Email"],
#            type='scatter', 
 #           mode ='lines+markers',
#            name="Email",
 #           marker = list(color = 'rgb(208, 217, 60)'),
#            line = list(color = 'rgb(208, 217, 60)'))  %>%
  
 # add_trace(y = ~con_1_MC[`Last Touch Marketing Channel`=="Affiliate"],
  #          type='scatter', 
  #          mode ='lines',
  #          name="Affiliate",
  #          marker = list(color = 'rgb(82, 81, 153)'),
  #          line = list(color = 'rgb(82, 81, 153)'), dash="dash")  %>%
#  add_trace(y = ~con_1_MC[`Last Touch Marketing Channel`=="Mobile App"],
  #          type='scatter', 
  #          mode ='lines',
  #          name="Mobile App",
  #          marker = list(color = 'rgb(52, 150, 81)'),
  #          line = list(color = 'rgb(52, 150, 81)'), dash="dash")  %>%
  
  #add_trace(y = ~con_1_MC[`Last Touch Marketing Channel`=="Social Networks"],
  #          type='scatter', 
  #          mode ='lines',
  #          name="Social Networks",
  #          marker = list(color = 'rgb(105, 105, 105)'),
  #          line = list(color = 'rgb(105, 105, 105)'), dash="dash")  %>%
  #add_segments(x = "2023-02-13", xend ="2023-02-13", y = 0, yend = 8, name ="OA MS +0.25") %>%
  #add_segments(x = "2023-02-27", xend ="2023-02-27", y = 0, yend = 8, name ="FRL +0.10 to 0.35") %>%
  #add_segments(x = "2023-03-13", xend ="2023-03-13", y = 0, yend = 8, name ="OA MS +0.25") %>%
  #add_segments(x = "2023-03-27", xend ="2023-03-27", y = 0, yend = 8, name ="FRL -0.20") %>%
  #add_segments(x = "2023-05-07", xend ="2023-05-07", y = 0, yend = 8, name ="OA MS +0.25") %>%
  #add_segments(x = "2023-05-14", xend ="2023-05-14", y = 0, yend = 8, name ="OA MS +0.10") %>%
  #add_segments(x = "2023-05-28", xend ="2023-05-28", y = 0, yend = 8, name ="FRL + 0.20-0.55") %>%
  #add_segments(x = "2023-06-04", xend ="2023-06-04", y = 0, yend = 8, name ="OA MS +0.10") %>%
  #add_segments(x = "2023-06-11", xend ="2023-06-11", y = 0, yend = 8, name ="OA MS +0.25") %>%
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
                      range =c(2,8),
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
fig

  
MonthvsWeek <- filter(testing,
                      MonthvsWeek == "Week",
                      #Date > "2023-03-06",
                      `Visitor or Client` == "total"
                      )

MonthvsWeek <- as.data.frame(MonthvsWeek)

MonthvsWeek <- dplyr::full_join(MonthvsWeek, rates) %>%
  dplyr::arrange(Date) %>%
  #dplyr::filter(Date > "2023-03-06") %>%
  fill(interest, .direction ="up") %>%
  dplyr::filter(is.na(rate_direction), !is.na(con_1_CvnC))


fig <- plot_ly(MonthvsWeek, x = ~unique(Date)) %>%
  add_trace(
    y = ~unique(con_1_CvnC[`Visitor or Client`=="total"]),
    marker = list(color = 'rgb(255, 98,0)'),
    line = list(color = 'rgb(255, 98,0)', width = 4),
    name="start application rate") %>%
  add_trace(
    y = ~(interest[`Last Touch Marketing Channel`=="Direct"]),
    marker = list(color = 'red'),
    name="indicative_interest",
    line = list(color = 'red', width = 4)) %>%
  
   # add_segments(x = "2023-03-13", xend ="2023-03-13", y = 0, yend = 8, name ="OA MS +0.25") %>%
  #  add_segments(x = "2023-03-27", xend ="2023-03-27", y = 0, yend = 8, name ="FRL -0.20") %>%
  #  add_segments(x = "2023-05-07", xend ="2023-05-07", y = 0, yend = 8, name ="OA MS +0.25") %>%
  #  add_segments(x = "2023-05-14", xend ="2023-05-14", y = 0, yend = 8, name ="OA MS +0.10") %>%
  #  add_segments(x = "2023-05-28", xend ="2023-05-28", y = 0, yend = 8, name ="FRL + 0.20-0.55") %>%
  #  add_segments(x = "2023-06-04", xend ="2023-06-04", y = 0, yend = 8, name ="OA MS +0.10") %>%
  #  add_segments(x = "2023-06-11", xend ="2023-06-11", y = 0, yend = 8, name ="OA MS +0.25") %>%
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
                      range =c(3,6.5),
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
fig

try <- MonthvsWeek %>%
  dplyr::filter(`Visitor or Client`=="total") %>%
  dplyr::select(Date, interest, con_1_CvnC) %>%
  distinct() %>%
  
  
  correlate(try$interest, try$con_1_CvnC)
cor(try$interest, try$con_1_CvnC, method = c("pearson"))



HL_total_conversion_week_device <- HL_total_conversion %>%
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
  dplyr::group_by(week_Date, `Device Type`) %>%
  dplyr::summarise(
    `Product page` = sum(`Visits`),
    `Start application` = sum(`HL App starts (Hustlers)`),
    `Finish application` = sum(`HL app complete`)
  ) %>%
  dplyr::mutate(
    `Product page` = `Product page` - `Start application`,
    `Start application` = `Start application`/1.2,
    `Finish application` = `Finish application`/1.1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Category = "Mortgages",
    con_1 = (`Start application` /`Product page`) *100,
    con_2 = (`Finish application` / `Start application`) *100
  ) %>%
  dplyr::arrange(week_Date) %>%
  dplyr::mutate(
    #target_PP = case_when(year=="2022" ~ `Product page` * 1.07),
    target_con1 = 5,
    Date = week_Date
    # target_FF = case_when(year=="2022" ~ `Finish application` * 1.38)
  ) %>%
  dplyr::select (-week_Date) %>%
  dplyr::filter(`Device Type` !="::unspecified::")

fig <- plot_ly(HL_total_conversion_week_device, x = ~unique(Date)) %>%
  add_trace(
    y = ~ con_1[`Device Type`=="Desktop"],
    line = list(color = 'rgb(255, 98,0)', width = 4),
    name="Desktop") %>%
  add_trace(
    y = ~con_1[`Device Type`=="Mobile"],
    line = list(color = 'rgb(52, 150, 81)', width = 4),
    name="Mobile") %>%
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
                        range =c(3,6.5),
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
  fig
