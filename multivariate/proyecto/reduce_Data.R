#################
# Christian Badillo
# 16/11/2024
# This script does fun stuffs.

library(tidyverse)

# Read Data.

raw.data <- read.csv("fifa_data_clean.csv")
positions.players <- read.csv("data_player_position.csv")

# Left Join

data <- left_join(positions.players, by = join_by(player == player))

laLiga.teams <- c("Villarreal CF ", "Real Sociedad ", "Real Madrid ", "Atlético de Madrid ", "FC Barcelona ",
                  "Athletic Club ", "RCD Espanyol ", "Sevilla FC ", "Real Sociedad ", "Rayo Vallecano ", 
                  "Valencia CF ", "Getafe CF ", "CA Osasuna ", "Real Betis ", "RC Celta ", "R. Valladolid CF ",
                  "Cádiz CF ", "RCD Mallorca ", "UD Las Palmas ", "UD Almería ", "Granada CF ", "Deportivo Alavés ")


Laliga_players <- raw.data[raw.data$club %in% laLiga.teams, ] %>%
    left_join(positions.players, by = join_by(player == player))

Laliga_players <- Laliga_players %>%
    distinct(player, .keep_all = T)

write.csv(Laliga_players, "spain_teams_player_data.csv")
