# Plots of NJ housing data from Realtor.com
library(tidyverse)
library(lubridate)
library(plotly)

housing <- read_csv('Housing.csv')

# Add a day value to X axis so it will plot properly
housing$month_date_yyyymm <- ymd(paste0(housing$month_date_yyyymm,'01'))

housing_2020 <- housing %>% filter(month_date_yyyymm > '2019-12-31')

housing_2019 <- housing %>% filter(month_date_yyyymm < '2020-01-01')


# Plot of change in median listing price in dollars from Jan,2020 to present

fig1 <- plot_ly(housing_2020, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_listing_price, name = ~'Median Listing Price',mode = 'lines') %>%
  layout(
    title = "Median Listing Price Changes During COVID-19 in New Jersey",
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(housing_2020$month_date_yyyymm), max(housing_2020$month_date_yyyymm)),
    yaxis = list(title = "Median Listing Price")
    ))
fig1


# Plot of percent change month-to-month in median listing price from Jan,2020 to present

fig2 <- plot_ly(housing_2020, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_listing_price_yy, name = ~'Change Versus 2019',mode = 'lines') %>%
  add_trace(y = ~median_listing_price_mm, name = ~'Change Since Month-to-month',mode = 'lines') %>%
  layout(
    title = "Median Listing Price Percent Change During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Relative to 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(housing_2020$month_date_yyyymm), max(housing_2020$month_date_yyyymm))
    ))
fig2