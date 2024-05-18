# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the CSV data
file_url <- "https://raw.githubusercontent.com/jai-agrawal/nba-player-clutch-stats/master/clutch_data.csv"
clutch_data <- read_csv(file_url)

# Filter data based on minimum minutes and games played
clutch_data <- clutch_data %>%
  filter(MIN >= 1, GP >= 3)

# Group data by team and calculate average points scored per game and standard deviation
points_stats <- clutch_data %>%
  group_by(TEAM_ABBREVIATION) %>%
  summarise(avg_points = mean(PTS, na.rm = TRUE),
            sd_points = sd(PTS, na.rm = TRUE))

# Create bar chart of average points scored per game with error bars for standard deviation
ggplot(points_stats, aes(x = reorder(TEAM_ABBREVIATION, -avg_points), y = avg_points, fill = TEAM_ABBREVIATION)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = avg_points - sd_points, ymax = avg_points + sd_points), width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Average Points Scored per Game in Clutch Time by Team",
       x = "Team Abbreviation",
       y = "Average Points Scored") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

ggsave(filename = "chart3.png", plot = p, width = 8, height = 6, dpi = 300)

