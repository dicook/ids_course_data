# remove side bar, and place controllers on the top
# "Model & Predict" -> "Predict"
# Cache and store the weather data with scripts
# Provide a pre-computed model object: dummy "air con" max_temp > 30, min_temp <= 5
# Input time frame and temp to predict daily usage
# Your new tab "Explanation"

library(tidyverse)
library(lubridate)
library(tsibble)
library(sugrrants)
library(hms)
library(broom)

elec_long <- read_rds(here::here("data", "elec_all.rds"))

elec_sub <- elec_long %>%
  mutate(time = as.factor(as.character(time))) %>% 
  mutate_if(is.ordered, ~ factor(., ordered = FALSE)) %>% 
  filter(between(date, ymd("2018-01-01"), ymd("2018-12-31"))) 

elec_new_data <- elec_long %>% 
  mutate(time = as.factor(as.character(time))) %>% 
  mutate_if(is.ordered, ~ factor(., ordered = FALSE)) %>% 
  filter(date > ymd("2018-12-31"))

fit <- lm(log1p(kwh) ~ work * time * month * air_con, data = elec_sub)
write_rds(fit, "data/model_fit.rds", compress = "xz")
glance(fit)

input_date <- ymd("2019-01-01")
input_temp <- 32
vic_holidays <- holiday_aus(year(input_date), state = "VIC")
user_input_data <- 
  tibble(
    date = input_date,
    time = as.factor(unique(as.character(elec_long$time)))
  ) %>% 
  mutate(
    # date based transformations
    wday = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    month = month(date, label = TRUE, abbr = TRUE),
    work = case_when(
      wday %in% c("Sat", "Sun") ~ "holiday",
      date %in% vic_holidays$date ~ "holiday",
      TRUE ~ "work day"
    ),
    air_con = as.factor(if_else(input_temp > 30 | input_temp < 5, "yes", "no"))
  )
elec_pred <- augment(fit, newdata = user_input_data) %>% 
  mutate(.fitted = exp(.fitted) - 1)

sum(elec_pred$.fitted)

daily_2018 <- elec_sub %>%
  group_by(date) %>% 
  summarise(daily = sum(kwh))
quantile(daily_2018$daily)
elec_aug <- augment(fit, data = elec_sub)
elec_aug1 <- augment(fit, newdata = elec_new_data)

p <- elec_aug1 %>%
  mutate(.fitted = exp(.fitted) - 1) %>% 
  # filter(date < ymd("2018-04-01")) %>% 
  pivot_longer(c(".fitted", "kwh"), names_to = "type", values_to = "values") %>% 
  frame_calendar(x = time, y = values, date = date) %>% 
  ggplot(aes(x = .time, y = .values, colour = type, group = interaction(date, type))) +
  geom_line()
prettify(p)

