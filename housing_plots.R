# Plots of NJ housing data from Realtor.com
library(tidyverse)
library(lubridate)
library(plotly)


# Define custom colours

teal <- '#00aaa0'
pink <- 'e54060'
blue <- '0c5c80'


data <- read_csv('https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_State_History.csv')

housing <- data %>% filter(state_id == 'nj')

# Add a day value to X axis so it will plot properly
housing$month_date_yyyymm <- ymd(paste0(housing$month_date_yyyymm,'01'))

# Subset by year
housing_2020 <- housing %>% filter(month_date_yyyymm > '2019-12-31')
housing_2019 <- housing %>% filter(month_date_yyyymm < '2020-01-01')


# Plot of change in median listing price in dollars from Jan,2020 to present

fig1 <- plot_ly(housing_2020, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_listing_price, name = 'Median Listing Price', type = 'scatter', mode = 'lines', line = list(color=teal)) %>%
  layout(
    title = "Median Listing Price Changes During COVID-19 in New Jersey", titlefont = list(size = 18),
    yaxis = list(title = "Median Listing Price", titlefont = list(size = 40), tickfont = list(size = 40)),
    xaxis = list(
      type = "date",
      title = "Month",
      titlefont = list(size = 40), tickfont = list(size = 40),
      range = c(min(housing_2020$month_date_yyyymm), max(housing_2020$month_date_yyyymm))
    ))
fig1


# Plot of percent change month-to-month in median listing price from Jan,2020 to present

fig2 <- plot_ly(housing_2020, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_listing_price_yy, name = ~'Change relative to 2019',mode = 'lines', line = list(color=teal)) %>%
  add_trace(y = ~median_listing_price_mm, name = ~'Change month-to-month',mode = 'lines', line = list(color=pink)) %>%
  layout(
    title = "Median Listing Price Percent Change During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change",
                 titlefont = list(size = 40), tickfont = list(size = 40),
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      titlefont = list(size = 40), tickfont = list(size = 40),
      range=c(min(housing_2020$month_date_yyyymm), max(housing_2020$month_date_yyyymm))
    ))
fig2


# Plot of active listings percent change 

fig3 <- plot_ly(housing_2020, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~active_listing_count_yy, name = ~'Change relative to 2019',mode = 'lines', line = list(color=teal)) %>%
  add_trace(y = ~active_listing_count_mm, name = ~'Change month-to-month',mode = 'lines', line = list(color=pink)) %>%
  layout(
    title = "Percent Change in Active Listings During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change",
                 titlefont = list(size = 40), tickfont = list(size = 40),
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      titlefont = list(size = 40), tickfont = list(size = 40),
      range=c(min(housing_2020$month_date_yyyymm), max(housing_2020$month_date_yyyymm))
    ))
fig3


# Plot of active listings percent change 

fig4 <- plot_ly(housing_2020, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_days_on_market_yy, name = ~'Change relative to 2019',mode = 'lines', line = list(color=teal)) %>%
  add_trace(y = ~median_days_on_market_mm, name = ~'Change month-to-month',mode = 'lines', line = list(color=pink)) %>%
  layout(
    title = "Percent Change in Median Days on the Market During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change",
                 titlefont = list(size = 40), tickfont = list(size = 40),
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      titlefont = list(size = 40), tickfont = list(size = 40),
      range=c(min(housing_2020$month_date_yyyymm), max(housing_2020$month_date_yyyymm))
    ))
fig4


# New Listings

fig5 <- plot_ly(housing_2020, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~new_listing_count_yy, name = ~'Change relative to 2019',mode = 'lines', line = list(color=teal)) %>%
  add_trace(y = ~new_listing_count_mm, name = ~'Change month-to-month',mode = 'lines', line = list(color=pink)) %>%
  layout(
    title = "Percent Change in New Listings During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change",
                 titlefont = list(size = 40), tickfont = list(size = 40),
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      titlefont = list(size = 40), tickfont = list(size = 40),
      range=c(min(housing_2020$month_date_yyyymm), max(housing_2020$month_date_yyyymm))
    ))
fig5