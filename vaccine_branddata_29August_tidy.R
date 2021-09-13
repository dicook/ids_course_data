# Code thanks to Mitch O'Hara-Wild
library(tidyverse)
x <- read_lines("https://raw.githubusercontent.com/caseybriggs/vaccine_branddata_29August/main/vaccine_branddata_29August.csv")

column_names <- c(
  "state", 
  "Weeks 1-25_2021-02-22_Pfizer", "Weeks 1-25_2021-02-22_AZ", 
  "Week 26_2021-08-16_Pfizer", "Week 26_2021-08-16_AZ", 
  "Week 27_2021-08-23_Pfizer", "Week 27_2021-08-23_AZ"
)

tidy_vaccines <- bind_rows(
  General = read_csv(str_c(x[3:11], collapse = "\n"), col_names = column_names),
  `Aged Care & Disability (residents and staff)` = 
    read_csv(str_c(x[14:22], collapse = "\n"), col_names = column_names),
  `GPs, CVCs, ACCHS & Pharmacies` = 
    read_csv(str_c(x[25:33], collapse = "\n"), col_names = column_names),
  `Other (incl. South Pacific, DFAT, Defence, Olympics)` = 
    read_csv(paste0(x[35], "\n"), col_names = column_names),
  .id = "category"
) %>% 
  select(category, column_names) %>% 
  pivot_longer(column_names[-1], names_to = c("week", "date", "brand"), 
               names_sep = "_", values_to = "vaccines",
               names_transform = list(date = as.Date)) %>% 
  filter(state != "NationalTotal")
