# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the CSV data
file_url <- "https://raw.githubusercontent.com/jai-agrawal/nba-player-clutch-stats/master/clutch_data.csv"
clutch_data <- read_csv(file_url)

# Calculate weighted statistics based on minutes played
clutch_data <- clutch_data %>%
  mutate(weighted_tov = TOV * MIN / GP)

# Filter data based on minimum minutes and games played
clutch_data <- clutch_data %>%
  filter(MIN >= 1, GP >= 3)

# Group data by team and calculate mean and standard deviation of turnovers
turnover_stats <- clutch_data %>%
  group_by(TEAM_ABBREVIATION) %>%
  summarise(avg_weighted_tov = mean(weighted_tov, na.rm = TRUE),
            sd_weighted_tov = sd(weighted_tov, na.rm = TRUE))

# Create bar chart with standard deviation
ggplot(turnover_stats, aes(x = TEAM_ABBREVIATION, y = avg_weighted_tov, fill = TEAM_ABBREVIATION)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_weighted_tov - sd_weighted_tov, ymax = avg_weighted_tov + sd_weighted_tov), width = 0.8, position = position_dodge(0.9)) +
  labs(title = "Average Turnovers in Clutch Time",
       x = "Team Abbreviation",
       y = "Average Turnovers") +
  theme_minimal() +
  theme(legend.position = "none")
