library(dplyr)
library(readr)
library(lubridate)
library(openmeteo)
library(tidymodels)
library(slider)
library(jsonlite)
library(bonsai)
library(lightgbm)

##test xgboost
period_start = today() - years(1) - months(8)

data <- read_delim("https://opendata.elia.be/api/explore/v2.1/catalog/datasets/ods003/exports/csv?lang=en&timezone=Europe%2FBrussels&use_labels=true&delimiter=%3B", delim = ';') %>% 
  rename(DateTime = Datetime,
         `Total Load` = `Elia Grid Load`) %>% 
  janitor::clean_names() %>% 
  mutate(datetime = lubridate::ymd_hms(date_time),
         date = date(datetime),
         year = year(datetime),
         month = month(datetime),
         wday = wday(datetime),
         day = day(datetime),
         hour = hour(datetime),
         doy = yday(datetime),
         quarter = quarter(datetime),
         period=paste(hour, minute(datetime)))

## Weather

get_last_10_days <- function(location){
  
  location_tibble <- openmeteo::geocode(location)
  lat <- location_tibble$latitude %>% round(2)
  lon <- location_tibble$longitude %>% round(2)
  
  url = paste0("https://api.open-meteo.com/v1/forecast?latitude=", lat, "&longitude=", lon, "&past_days=10&hourly=temperature_2m,weathercode")
  
  
  data <- jsonlite::fromJSON(url)
  
  res <- tibble(datetime = ymd_hm(data$hourly$time),
                temperature_2m = data$hourly$temperature_2m,
                weather_code = data$hourly$weathercode)
  
  return(res)}

brux_wx <- weather_history(c("Brussels"),
                           start = period_start,
                           end = today()-days(10),
                           hourly = c("temperature_2m", "weathercode"))

brug_wx <- weather_history(c("Bruges"),
                           start = period_start,
                           end = today()-days(10),
                           hourly = c("temperature_2m", "weathercode"))

brux_wx_10 <- get_last_10_days('Brussels')
brug_wx_10 <- get_last_10_days('Bruges')

bel_wx <- brug_wx %>% 
  left_join(brux_wx, by=c('datetime'), relationship = "many-to-many") %>%
  left_join(brux_wx_10) %>% 
  left_join(brug_wx_10) %>% 
  mutate(average_temp = (hourly_temperature_2m.x + hourly_temperature_2m.y)/2,
         temp_diff = hourly_temperature_2m.x - hourly_temperature_2m.y,
         brux_wc = as.factor(hourly_weathercode.y),
         brug_wc = as.factor(hourly_weathercode.x))

data_joined <- data %>% 
  left_join(bel_wx) %>% 
  arrange(datetime) %>% 
  fill(c(average_temp, temp_diff)) %>% 
  mutate(date = ymd(date))

## Holidays
belgian_holidays <- read_csv("belgian_holidays.csv") %>% 
  mutate(holiday_flag = as.factor(holiday_flag),
         date = ymd(date))


data_joined <- data_joined %>% 
  left_join(belgian_holidays, by='date') |> 
  mutate(h_sin = sin((hour/23 )*2*pi),
         period = as.factor(period),
         time_human = as.factor(case_when(hour >4 ~'early morning',
                                          hour <= 8 ~ 'morning',
                                          hour <=11 ~ 'morning',
                                          hour <=13 ~ 'noon',
                                          hour <=17 ~ 'afternoon',
                                          hour <= 21~ 'evening',
                                          hour <= 23 ~'night',
                                          TRUE~'sleep'))) 

start <- period_start
end <- data_joined$date_time %>% max()

fit_inp <- data_joined %>% 
  filter(date_time >= ymd(start)) %>%
  filter(date_time <= ymd_hms(end)) %>% 
  filter(!is.na(total_load)) %>% 
  mutate(weekday = as.factor(if_else(wday %in% c(1, 7), 0, 1)),
         cooling = pmax(average_temp, 15),
         dow = as.factor(wday))

max_past_24 = slider::slide(fit_inp$total_load, max, .before=96*3) %>% purrr::flatten_dbl()
min_past_24 = slider::slide(fit_inp$total_load, min, .before=96*3)%>% purrr::flatten_dbl()
mean_week = slider::slide(fit_inp$total_load, mean, .before=4*24*7)%>% purrr::flatten_dbl()
last_period = fit_inp$total_load %>% lag(96*3)

fit_inp <- fit_inp %>% 
  bind_cols(max_past_24 = max_past_24,
            min_past_24 = min_past_24,
            mean_week = mean_week,
            last_period = last_period)
###
fit_inp <- fit_inp %>% 
  mutate(label = if_else(date_time <end-days(1), 'train', 'test'))
###

lightgbm_model_tuned <- 
  boost_tree(
    mode = "regression",
    trees = 1800, 
    tree_depth = 13, 
    learn_rate = 0.07558388, 
    min_n = 16, 
    loss_reduction = 7.9627e-10) %>%
  set_engine("lightgbm")

lightgbm_wf <- 
  workflow() %>%
  add_model(lightgbm_model_tuned) %>% 
  add_formula(total_load ~ year + month + day + hour + dow + average_temp + 
                temp_diff + holiday_flag + cooling + mean_week + last_period + 
                period + doy + quarter + h_sin + time_human + min_past_24 + 
                max_past_24 + brux_wc + brug_wc)


mod <- lightgbm_wf %>% 
  fit(filter(fit_inp, label=='train'))


bel_wx_forecast <- weather_forecast('Brussels', 
                                     start = today(), 
                                     end = today() + days(3), 
                                     hourly=c("temperature_2m", "weathercode")) %>% 
  left_join(weather_forecast('Bruges', start = today(), 
                              end = today() + days(3), 
                              hourly=c("temperature_2m", "weathercode")), 
            by=c('datetime')) %>%
  mutate(average_temp = (hourly_temperature_2m.x + hourly_temperature_2m.y)/2,
         temp_diff = hourly_temperature_2m.x - hourly_temperature_2m.y,
         brux_wc = as.factor(hourly_weathercode.y),
         brug_wc = as.factor(hourly_weathercode.x))

future <- tibble(date_time = seq(as.POSIXct(paste(today(), "0:00")), 
                                 as.POSIXct(paste(today()+days(2), "23:45")), 
                                 by = "15 mins")) %>% 
  mutate(datetime = lubridate::ymd_hms(date_time),
         date = date(datetime),
         year = year(datetime),
         month = month(datetime),
         wday = wday(datetime),
         day = day(datetime),
         hour = hour(datetime),
         dow = as.factor(wday),
         doy = yday(datetime),
         quarter = quarter(datetime),
         period=paste(hour, minute(datetime))) %>%  
  left_join(bel_wx_forecast) %>% 
  arrange(datetime) %>% 
  fill(c(average_temp, temp_diff)) %>% 
  mutate(date = ymd(date)) %>% 
  left_join(belgian_holidays, by='date') 

max_past_24 = slider::slide(tail(fit_inp$total_load, 24*4*3), max, .before=96*3) %>% purrr::flatten_dbl()
min_past_24 = slider::slide(tail(fit_inp$total_load, 24*4*3), min, .before=96*3)%>% purrr::flatten_dbl()
mean_week = slider::slide(tail(fit_inp$total_load, 24*4*3), mean, .before=24*4*7)%>% purrr::flatten_dbl()
last_period = tail(lag(fit_inp$total_load, 96*3), 24*4*3)

future <- future %>% 
  bind_cols(max_past_24 = max_past_24,
          min_past_24 = min_past_24,
          mean_week = mean_week,
          last_period = last_period) %>% 
  mutate(dow=as.factor(wday), 
         cooling = pmax(average_temp, 15),
         h_sin = sin((hour/23 )*2*pi),
         period = as.factor(period),
         time_human = as.factor(case_when(hour >4 ~'early morning',
                                          hour <= 8 ~ 'morning',
                                          hour <=11 ~ 'morning',
                                          hour <=13 ~ 'noon',
                                          hour <=17 ~ 'afternoon',
                                          hour <= 21~ 'evening',
                                          hour <= 23 ~'night',
                                          TRUE~'sleep')))

preds <- predict(mod, future) %>% 
  bind_cols(future) %>% 
  mutate(type='preds')


##backfit
backfit <- predict(mod, filter(fit_inp, label=='test')) %>% 
  bind_cols(filter(fit_inp, label=='test')) %>%
  mutate(type = 'backfit')

results <- preds %>%
  bind_rows(backfit)

write_csv(results, 'data/results.csv')