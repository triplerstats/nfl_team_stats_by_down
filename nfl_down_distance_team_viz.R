setwd("C:/Users/richr/OneDrive/Documents/R")

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gt)
library(ggimage)
library(ggbeeswarm)

#install.packages("devtools")
#library(devtools)
#devtools::install_github("mrcaseb/nflfastR")
library(nflfastR)

# from NFL FastR Readme
## import play-by-play data from specified seasons
## import play-by-play data from specified seasons
season_min = 2021
season_max = 2021

seasons <- season_min:season_max

pbp <- nflfastR::load_pbp(seasons)

# select rush or pass plays
pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(down)) %>%
  filter(!is.na(epa))

pbp_rp <- as_tibble(pbp_rp)

league_down_distance_summary <- pbp_rp %>%
  filter(season == 2021) %>%
  mutate(ydstogo_cut = cut(ydstogo, breaks = c(0, 1, 2, 4, 7, 10, 99))) %>%
  group_by(down, ydstogo_cut) %>%
  summarize(plays = n(),
            epa_per_play = mean(epa),
            rush_epa_per_play = sum(ifelse(rush == 1, epa, 0)) / sum(rush),
            pass_epa_per_play = sum(ifelse(pass == 1, epa, 0)) / sum(pass),
            success_rate = mean(success),
            rush_success_rate = sum(ifelse(rush == 1, success, 0)) / sum(rush),
            pass_success_rate = sum(ifelse(pass == 1, success, 0)) / sum(pass),
            first_down_rate = mean(first_down),
            rush_first_down_rate = sum(ifelse(rush == 1, first_down, 0)) / sum(rush),
            pass_first_down_rate = sum(ifelse(pass == 1, first_down, 0)) / sum(pass),
            rush_pct = mean(rush),
            pass_pct = mean(pass)) %>%
  group_by(down) %>%
  mutate(pct_of_down_plays = plays / sum(plays))

league_down_distance_summary <- as_tibble(league_down_distance_summary)

team_down_distance_summary <- pbp_rp %>%
  filter(season == 2021) %>%
  mutate(ydstogo_cut = cut(ydstogo, breaks = c(0, 1, 2, 4, 7, 10, 99))) %>%
  group_by(posteam, down, ydstogo_cut) %>%
  summarize(plays = n(),
            epa_per_play = mean(epa),
            rush_epa_per_play = sum(ifelse(rush == 1, epa, 0)) / sum(rush),
            pass_epa_per_play = sum(ifelse(pass == 1, epa, 0)) / sum(pass),
            success_rate = mean(success),
            rush_success_rate = sum(ifelse(rush == 1, success, 0)) / sum(rush),
            pass_success_rate = sum(ifelse(pass == 1, success, 0)) / sum(pass),
            first_down_rate = mean(first_down),
            rush_first_down_rate = sum(ifelse(rush == 1, first_down, 0)) / sum(rush),
            pass_first_down_rate = sum(ifelse(pass == 1, first_down, 0)) / sum(pass),
            rush_pct = mean(rush),
            pass_pct = mean(pass)) %>%
  group_by(posteam, down) %>%
  mutate(pct_of_down_plays = plays / sum(plays))

team_down_distance_summary <- as_tibble(team_down_distance_summary)

team_league_combined <- merge(team_down_distance_summary, league_down_distance_summary, by = c("down", "ydstogo_cut"))

# specify summary statistic column names as team or league values
colnames(team_league_combined) <- str_replace(colnames(team_league_combined), ".x$", "_team")
colnames(team_league_combined) <- str_replace(colnames(team_league_combined), ".y$", "_league")

team_league_combined <- team_league_combined %>%
  #group_by(down, ydstogo) %>%
  group_by(down, ydstogo_cut) %>%
  mutate(epa_per_play_segment_z_score = (epa_per_play_team - mean(epa_per_play_team)) / sd(epa_per_play_team)) %>%
  mutate(rush_epa_per_play_segment_z_score = (rush_epa_per_play_team - mean(rush_epa_per_play_team)) / sd(rush_epa_per_play_team)) %>%
  mutate(pass_epa_per_play_segment_z_score = (pass_epa_per_play_team - mean(pass_epa_per_play_team)) / sd(pass_epa_per_play_team)) %>%
  mutate(success_rate_segment_z_score = (success_rate_team - mean(success_rate_team)) / sd(success_rate_team)) %>%
  mutate(rush_success_rate_segment_z_score = (rush_success_rate_team - mean(rush_success_rate_team)) / sd(rush_success_rate_team)) %>%
  mutate(pass_success_rate_segment_z_score = (pass_success_rate_team - mean(pass_success_rate_team)) / sd(pass_success_rate_team)) %>%
  mutate(first_down_rate_segment_z_score = (first_down_rate_team - mean(first_down_rate_team)) / sd(first_down_rate_team)) %>%
  mutate(rush_first_down_rate_segment_z_score = (rush_first_down_rate_team - mean(rush_first_down_rate_team)) / sd(rush_first_down_rate_team)) %>%
  mutate(pass_first_down_rate_segment_z_score = (pass_first_down_rate_team - mean(pass_first_down_rate_team)) / sd(pass_first_down_rate_team)) %>%
  mutate(rush_pct_segment_z_score = (rush_pct_team - mean(rush_pct_team)) / sd(rush_pct_team)) %>%
  mutate(pass_pct_segment_z_score = (pass_pct_team - mean(pass_pct_team)) / sd(pass_pct_team)) %>%
  mutate(pct_of_down_plays_z_score = (pct_of_down_plays_team - mean(pct_of_down_plays_team)) / sd(pct_of_down_plays_team))

team_league_combined <- as_tibble(team_league_combined)

################################################################################
# tile plot by run location and gap
################################################################################

## df is a dataframe containing rushing stats by run location and gap
## variable_name is a string indicating the name of the variable to use as fill
## team_abbreviation is a string indicating the team name - used in plot title
generate_down_and_distance_plot <- function(df, variable_name, team_abbreviation, title_text) {
  
  # string of the variable segment z-score value
  variable_name_z_score <- paste0(variable_name, "_segment_z_score")
  
  # minimum and maximum z-score value for each 
  min_z_score <- min(df[,c(variable_name_z_score)], na.rm = TRUE)
  max_z_score <- max(df[,c(variable_name_z_score)], na.rm = TRUE)
  
  # create dataframe containing only data for specified team
  team_df <- subset(df, df$posteam == team_abbreviation)
  
  p1 <- ggplot(team_df, aes(x = ydstogo_cut, y = down)) +
    geom_tile(aes(fill = get(variable_name_z_score))) +
    scale_fill_gradient2(low = scales::muted("blue"), midpoint = 0, high = scales::muted("red"),
                         limits = c(min_z_score, max_z_score)) +
    geom_text(aes(label = paste0("Team: ", scales::percent(get(paste0(variable_name, "_team")), accuracy = 0.1)), vjust = -1.5)) +
    geom_text(aes(label = paste0("League: ", scales::percent(get(paste0(variable_name, "_league")), accuracy = 0.1)), vjust = 0)) +
    #geom_text(aes(label = paste0("Plays: ", plays_team), vjust = 1.5)) +
    labs(title = paste0(title_text, " - ", team_abbreviation),
         x = "Yards to Go",
         y = "Down",
         fill = "Relative to Average",
         caption = "Color scale based on z-score relative to other teams' values in each down and distance segment.")
  
  return(p1)
}

generate_down_and_distance_plot(df = team_league_combined, variable_name = "pass_pct", team_abbreviation = "BUF",
                                title_text = "Pass Plays as Percent of All Plays")