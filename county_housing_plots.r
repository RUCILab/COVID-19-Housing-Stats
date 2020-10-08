# Plots demonstrating NJ real estate trends by county / region

library(tidyverse)
library(lubridate)
library(plotly)

# Custom colours

teal <- '#00aaa0'
pink <- 'e54060'
blue <- '0c5c80'
brown <- '#80300c'

# Read in data and county mapping file
data <- read_csv('https://econdata.s3-us-west-2.amazonaws.com/Reports/Core/RDC_Inventory_Core_Metrics_County_History.csv')

# Get county-level data for NJ by filtering FIPS code
housing <- data %>% filter(str_detect(county_fips, "^34"))

# Read in mapping of counties to regions derived from NJ state shapefile
counties <- read_csv('nj_counties2.csv')

# Add a day value to X axis so it will plot properly
housing$month_date_yyyymm <- ymd(paste0(housing$month_date_yyyymm,'01'))

# Join data with county mapping
joined <- housing %>% left_join(counties, by=c('county_fips' = 'FIPSCO'))



# Aggregate by region and day, filter to 2020

summed <- joined %>% group_by(month_date_yyyymm,REGION) %>% 
  summarise(median_listing_price_yy=sum(median_listing_price_yy),
            median_listing_price_mm=sum(median_listing_price_mm),
            median_days_on_market_yy=sum(median_days_on_market_yy),
            median_days_on_market_mm=sum(median_days_on_market_mm),
            new_listing_count_yy=sum(new_listing_count_yy),
            new_listing_count_mm=sum(new_listing_count_mm)) %>% 
  filter(month_date_yyyymm > '2019-12-31') %>% 
  na.omit()

# Ungroup before plotting to prevent blank plots

summed <- ungroup(summed)


# Figure 6 - Median List Price Change Relative to 2019

fig6 <- plot_ly(summed, x = ~month_date_yyyymm, y = ~median_listing_price_yy, color = ~REGION, type='scatter', mode = 'lines', line = list(color=teal,pink, width = 4)) %>%
layout(
    title = "Median Listing Price Change by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(summed$month_date_yyyymm), max(summed$month_date_yyyymm))
    ))
fig6


# Figure 7 - Median List Price Change Month to Month
fig7 <- plot_ly(summed, x = ~month_date_yyyymm, y = ~median_listing_price_mm, color = ~REGION, type='scatter', mode = 'lines') %>%
  layout(
    title = "Median Listing Price Change by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since Prior Month",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(summed$month_date_yyyymm), max(summed$month_date_yyyymm))
    ))
fig7


# Figure 8 - Median days on the market relative to 2019
fig8 <- plot_ly(summed, x = ~month_date_yyyymm, y = ~median_days_on_market_yy, color = ~REGION, type='scatter', mode = 'lines') %>%
  layout(
    title = "Median Days on Market by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(summed$month_date_yyyymm), max(summed$month_date_yyyymm))
    ))
fig8


# Figure 9 - Median days on the market month-to-month
fig9 <- plot_ly(summed, x = ~month_date_yyyymm, y = ~median_days_on_market_mm, color = ~REGION, type='scatter', mode = 'lines') %>%
  layout(
    title = "Median Days on Market by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since Prior Month",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(summed$month_date_yyyymm), max(summed$month_date_yyyymm))
    ))
fig9


# Figure 10 - New listing count since 2019

fig10 <- plot_ly(summed, x = ~month_date_yyyymm, y = ~new_listing_count_yy, color = ~REGION, type='scatter', mode = 'lines') %>%
  layout(
    title = "New Listings by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(summed$month_date_yyyymm), max(summed$month_date_yyyymm))
    ))
fig10

# Figure 11 - New listing count month-to-month

fig11 <- plot_ly(summed, x = ~month_date_yyyymm, y = ~new_listing_count_mm, color = ~REGION, type='scatter', mode = 'lines') %>%
  layout(
    title = "New Listings by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since Prior Month",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(summed$month_date_yyyymm), max(summed$month_date_yyyymm))
    ))
fig11



