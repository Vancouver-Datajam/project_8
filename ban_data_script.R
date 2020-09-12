## Plastic Ban Data Script

getwd()
ban_data <- read.csv("ban_data.csv")
str(ban_data)

library(ggplot2)
library(dplyr)
library(tidyverse)


# Converting date to proper format
ban_data$Effective.Date <- as.Date(ban_data$Effective.Date, format = '%d-%b-%y')
str(ban_data)


#---------------------------------------------------------------------------------
#Create dataframe with prov and plastic type
bans_prov_type <- ban_data %>% 
  count(Province, Type) %>% 
  arrange(desc(n))

ggplot(bans_prov_type, aes(x = reorder(Province, -n), y = n, fill = Type)) +
  geom_bar(stat="identity") + 
  labs(title = "Comparing the count and types of plastic bans by province (2007-2019)", y="Number of bans", x="Province")


#---------------------------------------------------------------------------------
# Add variable "year" to the dataset
ban_data$Year <- format(as.Date(ban_data$Effective.Date,format='%d-%b-%y'), '%Y')

ban_data <- ban_data %>% 
  add_row(Year = c("2011", "2013", "2014", "2015","2017"))

#---------------------------------------------------------------------------------
#Create dataframe with year and plastic type
bans_year_type <- ban_data %>% 
  count(Year, Type)

ggplot(bans_year_type, aes(x = Year, y = n, fill = Type)) +
  geom_bar(stat="identity") +
  scale_fill_discrete(na.translate = F) + 
  labs(title = "Comparing the count and types of plastic bans across all provinces (2007 to 2019)", y="Number of bans")

#---------------------------------------------------------------------------------

# Create dataframe with year and prov
bans_year_prov <- ban_data %>% 
  count(Year, Province)

ggplot(bans_year_prov, aes(x = Year, y = n, fill = Province)) +
  geom_bar(stat="identity") +
  scale_fill_discrete(na.translate = F) + 
  labs(title = "Comparing the count of plastic bans by province (2007-2019)", y="Number of bans")

#---------------------------------------------------------------------------------

# BC type of ban through time
bc <- ban_data %>%
  filter(Province=="BC") %>%
  count(Year, Type)

ggplot(bc, aes(fill=Type, x=Year, y=n)) +
  geom_bar(position = "dodge", stat="identity") + 
  labs(title = "Comparing the count and types of plastic bans in BC (2007-2019)", y="Number of bans")




