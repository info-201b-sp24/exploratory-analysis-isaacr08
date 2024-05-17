# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the CSV data
file_url <- "https://raw.githubusercontent.com/jai-agrawal/nba-player-clutch-stats/master/clutch_data.csv"
clutch_data <- read_csv(file_url)

# Filter data based on minimum minutes and games played
clutch_data_filtered <- clutch_data %>%
  filter(MIN >= 1, GP >= 3)

# Calculate total minutes played for each player
clutch_data_filtered <- clutch_data_filtered %>%
  mutate(total_minutes = MIN * GP)

# Calculate weighted defensive metric
clutch_data_defensive <- clutch_data_filtered %>%
  mutate(defensive_metric = (STL + BLK) / total_minutes)

# Select top 20 players with highest defensive metric
top_20_defensive_players <- clutch_data_defensive %>%
  group_by(PLAYER_NAME) %>%
  summarise(total_defensive_metric = sum(defensive_metric, na.rm = TRUE)) %>%
  top_n(20, wt = total_defensive_metric)

# Merge top 20 players with original clutch data
clutch_data_top_20_defensive <- inner_join(clutch_data_defensive, top_20_defensive_players, by = "PLAYER_NAME")

# Create scatter plot of defensive metric vs FG_PCT for top 20 players
ggplot(clutch_data_top_20_defensive, aes(x = defensive_metric, y = FG_PCT, color = PLAYER_NAME)) +
  geom_point() +
  labs(title = "Defensive Metric vs Field Goal Percentage in Clutch Time (Top 20 Players by Defensive Metric)",
       x = "Defensive Metric",
       y = "Field Goal Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom")
