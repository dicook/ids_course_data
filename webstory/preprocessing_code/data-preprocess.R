library(tidyverse)
library(lubridate)
library(tsibble)
library(glue)
library(bomrang)
library(hms)
library(sugrrants)
library(viridis)

# preprocessing data
elec <- read_csv(here::here("", "data", "meter_01.csv"), 
                 skip=2,
                 col_names = c("id", "date",  1:48)) %>% 
  filter(id == "300") %>% 
  mutate(date = ymd(date))

# Victorian holidays dataset
vic_holidays <- holiday_aus(2017:2019, state = "VIC")

# transform from wide to long
elec_long <- elec %>%
  gather(time, kwh, -id, -date) %>%
  mutate(kwh = as.numeric(kwh)) %>% 
  mutate(
    # time based transformations
    time = as.integer(time),
    hour = ceiling(time/ 2 - 1),
    min = ifelse(time %% 2, 30, 0),
    time = hms(minutes = min, hours = hour)
  ) %>%
  select(-hour, -min) %>%
  arrange(date, time) %>%
  mutate(
    # date based transformations
    wday = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    month = month(date, label = TRUE, abbr = TRUE),
    year = year(date),
    year_factor = factor(year, ordered = TRUE),
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "summer",
      month %in% c("Mar", "Apr", "May") ~ "autumn",
      month %in% c("Jun", "Jul", "Aug") ~ "winter",
      month %in% c("Sep", "Oct", "Nov") ~ "spring",
    ),
    season = factor(season, 
                    levels = c("summer", "autumn", "winter", "spring"),
                    ordered = TRUE),
    work = case_when(
      wday %in% c("Sat", "Sun") ~ "holiday",
      date %in% vic_holidays$date ~ "holiday",
      TRUE ~ "work day"
    )
  )
# Check data
elec_long %>% count(year)
daily <- elec_long %>% 
  group_by(date) %>% 
  summarise(daily_kwh = sum(kwh, na.rm = TRUE))

p <- elec_long %>% 
  left_join(daily, by = "date") %>%
  frame_calendar(time, kwh, date, ncol = 4) %>%
  ggplot(aes(x=.time, y=.kwh, group = date)) +
  geom_line(aes(colour = work)) +
  scale_colour_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")
prettify(p)

# weather data for Melbourne Aiport
station_id <- sweep_for_stations(latlon = c(-37.8136, 144.9631)) %>%
  filter(name == "MELBOURNE AIRPORT") %>%
  pull(site)
# download all weather mesurements for the site

# rainfall measured in mm
rain <- get_historical(stationid = station_id) %>%
  mutate(date = ymd(str_c(year, month, day, sep="-"))) %>%
  filter(between(date, min(elec_long$date), max(elec_long$date)))%>%
  select(date, rainfall, rainfall_quality = quality)

# daily max temp measured in celsius
max_temp <- get_historical(stationid = station_id, type = "max") %>%
  mutate(date = ymd(str_c(year, month, day, sep="-"))) %>%
  filter(between(date, min(elec_long$date), max(elec_long$date))) %>%
  select(date, max_temperature, max_temp_quality = quality)

min_temp <- get_historical(stationid = station_id, type = "min") %>%
  mutate(date = ymd(str_c(year, month, day, sep="-"))) %>%
  filter(between(date, min(elec_long$date), max(elec_long$date))) %>%
  select(date, min_temperature, min_temp_quality = quality)

# solar exposure measured in MJ/m^2
solar <- get_historical(stationid = station_id, type = "solar")%>%
  mutate(date = ymd(str_c(year, month, day, sep="-"))) %>%
  filter(between(date, min(elec_long$date), max(elec_long$date))) %>%
  select(date, solar_exposure)

# join by day
weather <- rain  %>%
  inner_join(max_temp) %>%
  inner_join(min_temp) %>%
  inner_join(solar)

# add all weather variables
elec_long <- elec_long %>%
  left_join(weather, by = "date") %>% 
  mutate(
    air_con = if_else(max_temperature > 30 | min_temperature < 5, "yes", "no"),
    air_con = as.factor(air_con)
  )
# Check data
p <- elec_long %>% 
  left_join(daily, by = "date") %>%
  frame_calendar(time, kwh, date, ncol = 4) %>%
  ggplot(aes(x=.time, y=.kwh, group = date)) +
  geom_line(aes(colour = max_temperature)) +
  scale_colour_viridis_c(option="magma") +
  theme(legend.position = "bottom")
prettify(p)

write_rds(elec_long, here::here("data", "elec_all.rds"))
