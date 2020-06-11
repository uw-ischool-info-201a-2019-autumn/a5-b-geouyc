# This is my R script 'analysis' where all the plots are made and the analysis is done

library(ggplot2)
library(leaflet)
library(dplyr)
library(knitr)
library(tidyverse)
library(stringr)

# Add to and edit the dataframe to include more relevant data
shootings_2018 <- read.csv('data/shootings-2018.csv', stringsAsFactors = FALSE)
shootings_2018$num_victims <- (shootings_2018$num_killed + shootings_2018$num_injured)
shootings_2018$month <- str_extract(shootings_2018$date, "\\w+")

# Calculate variables for the summary information paragraph
num_instances <- nrow(shootings_2018)
total_killed <- sum(shootings_2018$num_killed)
total_victims <- sum(shootings_2018$num_victims)
data_by_city <- shootings_2018 %>%
  group_by(city) %>%
  summarise(
    killed = sum(num_killed),
    injured = sum(num_injured),
    victims = sum(num_victims))
data_by_state <- shootings_2018 %>%
  group_by(state) %>%
  summarise(
    killed = sum(num_killed),
    injured = sum(num_injured),
    victims = sum(num_victims))
city_most_affected <- data_by_city[data_by_city$killed == max(data_by_city$killed), "city"]
num_most_affected <- data_by_city[data_by_city$killed == max(data_by_city$killed), "killed"]
state_most_victims <- data_by_state[data_by_state$victims == max(data_by_state$victims), "state"]
num_most_victims <- data_by_state[data_by_state$victims == max(data_by_state$victims), "victims"]

# Variables for the incident description paragraph
incident_date <- shootings_2018[33,1]
incident_state <- shootings_2018[33,2]
incident_city <- shootings_2018[33,3]
incident_address <- shootings_2018[33,4]
incident_killed <- shootings_2018[33,5]


# Create the interactive map with markers portraying incident address and the number of people killed
interactive_map <- leaflet(shootings_2018) %>%
  addTiles() %>%
  addCircleMarkers(lng = shootings_2018$long, lat = shootings_2018$lat,
                   popup = paste0("City: ", shootings_2018$city, "<br/>Number of People Injured: ", shootings_2018$num_injured
                                  , "<br/>Number of People Killed: ", shootings_2018$num_killed),
                                  radius = shootings_2018$num_victims)

# Create dataframe of data by month and create graph of killings by month
data_by_month <- shootings_2018 %>%
  group_by(month) %>%
  summarise(
    killed = sum(num_killed))
order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
data_by_month$month <- factor(data_by_month$month, levels = order)
bargraph_shootings <- ggplot(data = data_by_month, aes(x = month, y = killed)) +
  geom_bar(stat = "identity", fill = "cornflowerblue") +
  theme_minimal() +
  labs(title= "# of People Killed by Month",
         y="# of People", x = "Month")
