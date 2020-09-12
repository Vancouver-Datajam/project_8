## Plastic Ban Data Script

getwd()
ban_data <- read.csv("ban_data.csv")

library(ggplot2)
library(dplyr)
library(tidyverse)

# Converting date to proper format
ban_data$Effective.Date <- as.Date(ban_data$Effective.Date, format = '%d-%b-%y')
str(ban_data)

# # Sorting by ban types per province
# num_bans <- ban_data %>% 
#   group_by(Type, Province) %>% 
#   tally()

# Create dataframe to count and reorder ban counts per province
bans_prov <- ban_data %>% 
  count(Province) %>% 
  arrange(desc(n))

# Plot reordered ban counts per province
ggplot(bans_prov, aes(x = reorder(Province, -n), y = n)) +
  geom_bar(stat = "identity")

#Create dataframe with prov and plastic type
bans_prov_type <- ban_data %>% 
  count(Province, Type) %>% 
  arrange(desc(n))

