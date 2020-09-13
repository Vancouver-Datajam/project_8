##########################################
######Combine the data sets;
#########################################

library(readr)
library(dplyr)
library(here)
library(tidyverse)
library(lubridate)

gcsc_clean <- read_csv(file = "cleaned_gcsc_data.csv")

ban_data <- read_csv(file = here("ban_data.csv")) %>% 
  rename("ban_type" = Type,
         "effective_date" = `Effective Date`)

ban_wide <- ban_data %>% 
  pivot_wider(names_from = ban_type, 
              values_from = effective_date) %>% 
  mutate_at(3:5, funs(dmy)) %>% 
  mutate(ban_date = coalesce(`Plastics straws`, `Plastic bags`, `Food packaging`))

gcsc_ban_com <- gcsc_clean %>% 
  rename("City" = nearest_city, "Province" = province) %>% 
  left_join(ban_wide, by =c("City", "Province")) %>%
  select(Province, City, cleanup_date, date, ban_date, `Plastics straws`, `Plastic bags`, `Food packaging`, everything()) %>% 
  mutate(ban_in_place = if_else((date <= ban_date|is.na(ban_date)==T), "N", "Y"), 
         straw_ban = ifelse((`Plastics straws` > date|is.na(`Plastics straws`)==T), "N", "Y"), 
         bags_ban = ifelse((`Plastic bags` > date|is.na(`Plastic bags`)==T), "N", "Y"), 
         food_pack_ban = ifelse((`Food packaging` > date|is.na(`Food packaging`)==T), "N", "Y")
         ) %>%
  select(Province, City, cleanup_date, date, ban_date, ban_in_place, straw_ban, bags_ban, food_pack_ban, everything()) %>% 
  select(-c(`Plastics straws`, `Plastic bags`, `Food packaging`)) %>% 
  rename(cleanup_date_tm = cleanup_date, 
         cleanup_date = date)


gcsc_ban_com %>% 
  mutate(bags_personkm=bags_plastic_plastic_bags_from_2017_onwards/(adults_2017_onwards_participant_count*kilometers)) %>%
  mutate(straws_personkm=straws_stirrers_straws_from_2017_onwards/(adults_2017_onwards_participant_count*kilometers)) %>%
  mutate(foodwrappers_personkm=food_wrappers_containers/(adults_2017_onwards_participant_count*kilometers)) %>%
  group_by(bags_ban, Province) %>%
    summarize(mean_bags_adj = mean(bags_personkm, na.rm=TRUE),
              mean_straws_adj = mean(straws_personkm, na.rm=TRUE),
              mean_foodwrappers_adj = mean(food_wrappers_containers, na.rm=TRUE))
  
  
