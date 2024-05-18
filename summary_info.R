# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load the CSV data
file_url <- "https://raw.githubusercontent.com/jai-agrawal/nba-player-clutch-stats/master/clutch_data.csv"
clutch_data <- read_csv(file_url)

# Calculate weighted statistics based on minutes played
clutch_data <- clutch_data %>%
  mutate(weighted_tov = TOV * MIN / GP,
         weighted_ast = AST * MIN / GP,
         weighted_stl = STL * MIN / GP,
         weighted_ft_pct = FT_PCT * MIN / GP,
         weighted_blk = BLK * MIN / GP)

# Filter data based on minimum minutes and games played
clutch_data <- clutch_data %>%
  filter(MIN >= 2, GP >= 15)

# Group data by player and calculate total weighted statistics
player_stats <- clutch_data %>%
  group_by(PLAYER_NAME) %>%
  summarise(total_weighted_tov = sum(weighted_tov, na.rm = TRUE),
            total_weighted_ast = sum(weighted_ast, na.rm = TRUE),
            total_weighted_stl = sum(weighted_stl, na.rm = TRUE),
            avg_weighted_ft_pct = mean(weighted_ft_pct, na.rm = TRUE),
            total_weighted_blk = sum(weighted_blk, na.rm = TRUE))

# Normalize each statistic
player_stats <- player_stats %>%
  mutate(norm_weighted_tov = (total_weighted_tov - min(total_weighted_tov)) / (max(total_weighted_tov) - min(total_weighted_tov)),
         norm_weighted_ast = (total_weighted_ast - min(total_weighted_ast)) / (max(total_weighted_ast) - min(total_weighted_ast)),
         norm_weighted_stl = (total_weighted_stl - min(total_weighted_stl)) / (max(total_weighted_stl) - min(total_weighted_stl)),
         norm_weighted_ft_pct = (avg_weighted_ft_pct - min(avg_weighted_ft_pct)) / (max(avg_weighted_ft_pct) - min(avg_weighted_ft_pct)),
         norm_weighted_blk = (total_weighted_blk - min(total_weighted_blk)) / (max(total_weighted_blk) - min(total_weighted_blk)))

# Calculate the composite score as the average of the normalized statistics
player_stats <- player_stats %>%
  rowwise() %>%
  mutate(composite_score = mean(c(norm_weighted_tov, norm_weighted_ast, norm_weighted_stl, norm_weighted_ft_pct, norm_weighted_blk), na.rm = TRUE)) %>%
  ungroup()

# Select the top 20 players based on the composite score
top_20_players <- player_stats %>%
  arrange(desc(composite_score)) %>%
  slice_head(n = 20)

# Select relevant columns to display
top_20_players <- top_20_players %>%
  select(PLAYER_NAME, total_weighted_tov, total_weighted_ast, total_weighted_stl, avg_weighted_ft_pct, total_weighted_blk, composite_score)

# Print the top 20 players
print(top_20_players)