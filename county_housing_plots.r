library(tidyverse)
library(lubridate)
library(plotly)

# Read in data and county mapping file
housing <- read_csv('NJHousing_by_county.csv')
counties <- read_csv('nj_counties.csv')

# Add a day value to X axis so it will plot properly
housing$month_date_yyyymm <- ymd(paste0(housing$month_date_yyyymm,'01'))

# Join data with county mapping
joined <- housing %>% left_join(counties, by=c('county_fips' = 'FIPSSTCO'))



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

# Split summed dataframe by county regions

central <- summed %>% filter(REGION == 'CENTRAL')
coastal <- summed %>% filter(REGION == 'COASTAL')
southern <- summed %>% filter(REGION == 'SOUTHERN')
northeastern <- summed %>% filter(REGION == 'NORTHEASTERN')
northwestern <- summed %>% filter(REGION == 'NORTHWESTERN')

# Figure 6 - Median List Price Change Relative to 2019
fig6 <- plot_ly(coastal, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_listing_price_yy, name = 'Coastal', type='scatter', mode = 'lines') %>%
  add_trace(y = central$median_listing_price_yy, name = 'Central', type='scatter', mode = 'lines') %>%
  add_trace(y = southern$median_listing_price_yy, name = 'Southern', type='scatter', mode = 'lines') %>%
  add_trace(y = northeastern$median_listing_price_yy, name = 'Northeastern', type='scatter', mode = 'lines') %>%
  add_trace(y = northwestern$median_listing_price_yy, name = 'Northwestern', type='scatter', mode = 'lines') %>%
  
  layout(
    title = "Median Listing Price Change by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(coastal$month_date_yyyymm), max(coastal$month_date_yyyymm))
    ))
fig6


# Figure 7 - Median List Price Change Month to Month
fig7 <- plot_ly(coastal, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_listing_price_mm, name = 'Coastal', type='scatter', mode = 'lines') %>%
  add_trace(y = central$median_listing_price_mm, name = 'Central', type='scatter', mode = 'lines') %>%
  add_trace(y = southern$median_listing_price_mm, name = 'Southern', type='scatter', mode = 'lines') %>%
  add_trace(y = northeastern$median_listing_price_mm, name = 'Northeastern', type='scatter', mode = 'lines') %>%
  add_trace(y = northwestern$median_listing_price_mm, name = 'Northwestern', type='scatter', mode = 'lines') %>%
  
  layout(
    title = "Median Listing Price Change by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since Prior Month",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(coastal$month_date_yyyymm), max(coastal$month_date_yyyymm))
    ))
fig7


# Figure 8 - Median days on the market relative to 2019
fig8 <- plot_ly(coastal, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_days_on_market_yy, name = 'Coastal', type='scatter', mode = 'lines') %>%
  add_trace(y = central$median_days_on_market_yy, name = 'Central', type='scatter', mode = 'lines') %>%
  add_trace(y = southern$median_days_on_market_yy, name = 'Southern', type='scatter', mode = 'lines') %>%
  add_trace(y = northeastern$median_days_on_market_yy, name = 'Northeastern', type='scatter', mode = 'lines') %>%
  add_trace(y = northwestern$median_days_on_market_yy, name = 'Northwestern', type='scatter', mode = 'lines') %>%
  
  layout(
    title = "Median Days on Market by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(coastal$month_date_yyyymm), max(coastal$month_date_yyyymm))
    ))
fig8


# Figure 8 - Median days on the market relative to 2019
fig8 <- plot_ly(coastal, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_days_on_market_yy, name = 'Coastal', type='scatter', mode = 'lines') %>%
  add_trace(y = central$median_days_on_market_yy, name = 'Central', type='scatter', mode = 'lines') %>%
  add_trace(y = southern$median_days_on_market_yy, name = 'Southern', type='scatter', mode = 'lines') %>%
  add_trace(y = northeastern$median_days_on_market_yy, name = 'Northeastern', type='scatter', mode = 'lines') %>%
  add_trace(y = northwestern$median_days_on_market_yy, name = 'Northwestern', type='scatter', mode = 'lines') %>%
  
  layout(
    title = "Median Days on Market by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(coastal$month_date_yyyymm), max(coastal$month_date_yyyymm))
    ))
fig8


fig9 <- plot_ly(coastal, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~median_days_on_market_mm, name = 'Coastal', type='scatter', mode = 'lines') %>%
  add_trace(y = central$median_days_on_market_mm, name = 'Central', type='scatter', mode = 'lines') %>%
  add_trace(y = southern$median_days_on_market_mm, name = 'Southern', type='scatter', mode = 'lines') %>%
  add_trace(y = northeastern$median_days_on_market_mm, name = 'Northeastern', type='scatter', mode = 'lines') %>%
  add_trace(y = northwestern$median_days_on_market_mm, name = 'Northwestern', type='scatter', mode = 'lines') %>%
  
  layout(
    title = "Median Days on Market by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since Prior Month",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(coastal$month_date_yyyymm), max(coastal$month_date_yyyymm))
    ))
fig9

fig10 <- plot_ly(coastal, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~new_listing_count_yy, name = 'Coastal', type='scatter', mode = 'lines') %>%
  add_trace(y = central$new_listing_count_yy, name = 'Central', type='scatter', mode = 'lines') %>%
  add_trace(y = southern$new_listing_count_yy, name = 'Southern', type='scatter', mode = 'lines') %>%
  add_trace(y = northeastern$new_listing_count_yy, name = 'Northeastern', type='scatter', mode = 'lines') %>%
  add_trace(y = northwestern$new_listing_count_yy, name = 'Northwestern', type='scatter', mode = 'lines') %>%
  
  layout(
    title = "New Listings by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since 2019",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(coastal$month_date_yyyymm), max(coastal$month_date_yyyymm))
    ))
fig10

fig11 <- plot_ly(coastal, x = ~month_date_yyyymm) %>% 
  add_trace(y = ~new_listing_count_mm, name = 'Coastal', type='scatter', mode = 'lines') %>%
  add_trace(y = central$new_listing_count_mm, name = 'Central', type='scatter', mode = 'lines') %>%
  add_trace(y = southern$new_listing_count_mm, name = 'Southern', type='scatter', mode = 'lines') %>%
  add_trace(y = northeastern$new_listing_count_mm, name = 'Northeastern', type='scatter', mode = 'lines') %>%
  add_trace(y = northwestern$new_listing_count_mm, name = 'Northwestern', type='scatter', mode = 'lines') %>%
  
  layout(
    title = "New Listings by Region During COVID-19 in New Jersey",
    yaxis = list(title = "Percent Change Since Prior Month",
                 tickformat = "%"),
    xaxis = list(
      type = "date",
      title = "Month",
      range=c(min(coastal$month_date_yyyymm), max(coastal$month_date_yyyymm))
    ))
fig11



