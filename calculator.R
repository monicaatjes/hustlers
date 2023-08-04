
HL_calc <- read_excel("~/Documents/hustlers/data/HL_conversion.xlsx", sheet="calculator")

HL_calcu <- HL_calc %>%
  dplyr::mutate(
    calc_type = case_when(
      Page =
      str_detect(Page, "extra-loan-repayments") ~ "Extra repayments calculator",
      str_detect(Page, "ingdirect:home-loans:calculators:lump-sum") ~ "Lump sum calculator",
      str_detect(Page, "borrowing-power") ~ "Borrowing power calculator",
      str_detect(Page, "offset") ~ "Offset calculator",
      str_detect(Page, "where-can-i-buy") ~ "Where can I buy calculator",
      str_detect(Page, "calculators:stamp-duty") ~ "Stamp duty calculator",
      str_detect(Page, "calculators:split-loan") ~ "Split loan calculator",
      str_detect(Page, "calculators:buying-cost") ~ "Buying cost calculator",
      str_detect(Page, "calculators:repayments") ~ "Repayments calculator",
      str_detect(Page, "calculators:rental-yield-calculator") ~ "Rental yield calculator",
      str_detect(Page, "calculators:refinance") ~ "Refinance calculator",
      str_detect(Page, "calculators:equity") ~ "Equity calculator"
    )
  ) %>%
  dplyr::mutate(
    year = year(Day),
    month = format(Day, "%B"),
    month_year= as.yearmon(Day),
    week_Date = cut(as.POSIXct(Day), "week"),
    month_Date = cut(as.POSIXct(Day), "month")
  ) %>%
  dplyr::arrange(month_Date) %>%
  dplyr::group_by(month_Date, calc_type) %>%
  dplyr::summarise(
    Visits = sum(Visits)
  ) %>%
  dplyr::ungroup() %>%
  tidyr::spread(calc_type, Visits) %>%
  dplyr::mutate(
    combined_calc = `Extra repayments calculator` + `Offset calculator` + `Lump sum calculator`,
    total_calc = `Borrowing power calculator` + `Buying cost calculator` + `Equity calculator` +
      `Extra repayments calculator` + `Lump sum calculator` + `Offset calculator` + `Refinance calculator` +
      `Rental yield calculator` + `Repayments calculator` + `Stamp duty calculator`,
    shared_calc =  combined_calc / total_calc * 100
  )
