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
            total_weighted_blk = sum(weighted_blk, na.rm = TRUE)) %>%
  arrange(desc(total_weighted_tov), desc(total_weighted_ast), desc(total_weighted_stl), desc(avg_weighted_ft_pct), desc(total_weighted_blk))

# Select top ten players for each statistic
top_ten_tov <- head(player_stats[order(-player_stats$total_weighted_tov), c("PLAYER_NAME", "total_weighted_tov")], 10)
top_ten_ast <- head(player_stats[order(-player_stats$total_weighted_ast), c("PLAYER_NAME", "total_weighted_ast")], 10)
top_ten_stl <- head(player_stats[order(-player_stats$total_weighted_stl), c("PLAYER_NAME", "total_weighted_stl")], 10)
top_ten_ft_pct <- head(player_stats[order(-player_stats$avg_weighted_ft_pct), c("PLAYER_NAME", "avg_weighted_ft_pct")], 10)
top_ten_blk <- head(player_stats[order(-player_stats$total_weighted_blk), c("PLAYER_NAME", "total_weighted_blk")], 10)

# Print the top ten players for each statistic
cat("Top 10 Players for Turnovers (TOV):\n")
print(top_ten_tov)

cat("\nTop 10 Players for Assists (AST):\n")
print(top_ten_ast)

cat("\nTop 10 Players for Steals (STL):\n")
print(top_ten_stl)

cat("\nTop 10 Players for Free Throw Percentage (FT_PCT):\n")
print(top_ten_ft_pct)

cat("\nTop 10 Players for Blocks (BLK):\n")
print(top_ten_blk)