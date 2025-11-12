setwd("C:/Users/richr/OneDrive/Documents/R")

library(nflreadr)
library(tidyverse)
library(gt)
library(RColorBrewer)
library(nflfastR)
library(webshot2)

# load in play-by-play and player participation (on field for play) data
pbp_data <- load_participation(seasons = c(2021:2022), include_pbp = TRUE)

# load 2021 rosters data
rosters_2021 <- load_rosters(seasons = 2021)
rosters_2022 <- load_rosters(seasons = 2022)

# team logos
nfl_logos <- nflfastR::teams_colors_logos

source("nfl_my_utils.R")

# use my function to create new variables specifying number of players on field at specific positions
pbp_data <- create_pbp_new_players_on_field_variables(pbp_df = pbp_data)

########################################################################################################################
# league-wide conversion rate by down and distance
########################################################################################################################

down_distance_summary <- pbp_data %>%
  filter(season == 2022) %>%
  filter(season_type == "REG") %>%
  filter(!is.na(down)) %>%
  filter(down == 3) %>%
  filter(pass == 1 | rush == 1) %>%
  filter(play_type_nfl != "PENALTY") %>%
  group_by(down, ydstogo) %>%
  summarize(plays = n(),
            yards_per_play = sum(yards_gained, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            rush_plays = sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_plays = sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE))

# with grouping by play type
down_distance_play_type_summary <- pbp_data %>%
  filter(season == 2022) %>%
  filter(season_type == "REG") %>%
  filter(half_seconds_remaining >= 120) %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(!is.na(down)) %>%
  filter(down <= 3) %>%
  filter(pass == 1 | rush == 1) %>%
  filter(play_type_nfl != "PENALTY") %>%
  group_by(play_type, down, ydstogo) %>%
  summarize(plays = n(),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE)) %>%
  group_by(down, ydstogo) %>%
  mutate(pct_of_plays = plays / sum(plays)) %>%
  filter(ydstogo <= 20)

ggplot(data = down_distance_play_type_summary, aes(x = ydstogo, y = first_down_rate)) +
  facet_grid(. ~ down) +
  geom_point(aes(color = play_type, size = plays)) +
  geom_smooth(aes(color = play_type)) +
  scale_color_manual(values = c("blue", "red"))

########################################################################################################################
# team rush and pass efficiency by down
########################################################################################################################

# offense 1st down summary
offense_team_down_run_pass_summary_1 <- pbp_data %>%
  filter(season %in% c(2022)) %>%
  filter(!is.na(down)) %>%
  filter(down == 1) %>%
  filter(pass == 1 | rush == 1) %>%
  #filter(play_type_nfl != "PENALTY") %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(half_seconds_remaining > 120) %>%
  #mutate(down_group = ifelse(down < 3, "early down", "third down")) %>%
  #group_by(defteam) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            rush_percent = mean(rush, na.rm = TRUE),
            pass_percent = mean(pass, na.rm = TRUE),
            expected_pass_percent = mean(xpass, na.rm = TRUE),
            average_yards_to_go = mean(ydstogo, na.rm = TRUE),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE),
            rush_yards_to_go = sum(ydstogo * rush) / sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush) / sum(rush, na.rm = TRUE),
            rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush) / sum(rush, na.rm = TRUE),
            rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)
            )

# offense 2nd down summary
offense_team_down_run_pass_summary_2 <- pbp_data %>%
  filter(season %in% c(2022)) %>%
  filter(!is.na(down)) %>%
  filter(down == 2) %>%
  filter(pass == 1 | rush == 1) %>%
  #filter(play_type_nfl != "PENALTY") %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(half_seconds_remaining > 120) %>%
  #mutate(down_group = ifelse(down < 3, "early down", "third down")) %>%
  #group_by(defteam) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            rush_percent = mean(rush, na.rm = TRUE),
            pass_percent = mean(pass, na.rm = TRUE),
            expected_pass_percent = mean(xpass, na.rm = TRUE),
            average_yards_to_go = mean(ydstogo, na.rm = TRUE),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE),
            rush_yards_to_go = sum(ydstogo * rush) / sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush) / sum(rush, na.rm = TRUE),
            rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush) / sum(rush, na.rm = TRUE),
            rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)
  )

# offense 3rd down summary
offense_team_down_run_pass_summary_3 <- pbp_data %>%
  filter(season %in% c(2022)) %>%
  filter(!is.na(down)) %>%
  filter(down == 3) %>%
  filter(pass == 1 | rush == 1) %>%
  #filter(play_type_nfl != "PENALTY") %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(half_seconds_remaining > 120) %>%
  #mutate(down_group = ifelse(down < 3, "early down", "third down")) %>%
  #group_by(defteam) %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            rush_percent = mean(rush, na.rm = TRUE),
            pass_percent = mean(pass, na.rm = TRUE),
            expected_pass_percent = mean(xpass, na.rm = TRUE),
            average_yards_to_go = mean(ydstogo, na.rm = TRUE),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE),
            rush_yards_to_go = sum(ydstogo * rush) / sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush) / sum(rush, na.rm = TRUE),
            rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush) / sum(rush, na.rm = TRUE),
            rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)
  )

# defense 1st down summary
defense_team_down_run_pass_summary_1 <- pbp_data %>%
  filter(season %in% c(2022)) %>%
  filter(!is.na(down)) %>%
  filter(down == 1) %>%
  filter(pass == 1 | rush == 1) %>%
  #filter(play_type_nfl != "PENALTY") %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(half_seconds_remaining > 120) %>%
  #mutate(down_group = ifelse(down < 3, "early down", "third down")) %>%
  group_by(defteam) %>%
  #group_by(posteam) %>%
  summarize(plays = n(),
            rush_percent = mean(rush, na.rm = TRUE),
            pass_percent = mean(pass, na.rm = TRUE),
            expected_pass_percent = mean(xpass, na.rm = TRUE),
            average_yards_to_go = mean(ydstogo, na.rm = TRUE),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE),
            rush_yards_to_go = sum(ydstogo * rush) / sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush) / sum(rush, na.rm = TRUE),
            rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush) / sum(rush, na.rm = TRUE),
            rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)
            )

# defense 2nd down summary
defense_team_down_run_pass_summary_2 <- pbp_data %>%
  filter(season %in% c(2022)) %>%
  filter(!is.na(down)) %>%
  filter(down == 2) %>%
  filter(pass == 1 | rush == 1) %>%
  #filter(play_type_nfl != "PENALTY") %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(half_seconds_remaining > 120) %>%
  #mutate(down_group = ifelse(down < 3, "early down", "third down")) %>%
  group_by(defteam) %>%
  #group_by(posteam) %>%
  summarize(plays = n(),
            rush_percent = mean(rush, na.rm = TRUE),
            pass_percent = mean(pass, na.rm = TRUE),
            expected_pass_percent = mean(xpass, na.rm = TRUE),
            average_yards_to_go = mean(ydstogo, na.rm = TRUE),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE),
            rush_yards_to_go = sum(ydstogo * rush) / sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush) / sum(rush, na.rm = TRUE),
            rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush) / sum(rush, na.rm = TRUE),
            rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)
  )

# defense 3rd down summary
defense_team_down_run_pass_summary_3 <- pbp_data %>%
  filter(season %in% c(2022)) %>%
  filter(!is.na(down)) %>%
  filter(down == 3) %>%
  filter(pass == 1 | rush == 1) %>%
  #filter(play_type_nfl != "PENALTY") %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(half_seconds_remaining > 120) %>%
  #mutate(down_group = ifelse(down < 3, "early down", "third down")) %>%
  group_by(defteam) %>%
  #group_by(posteam) %>%
  summarize(plays = n(),
            rush_percent = mean(rush, na.rm = TRUE),
            pass_percent = mean(pass, na.rm = TRUE),
            expected_pass_percent = mean(xpass, na.rm = TRUE),
            average_yards_to_go = mean(ydstogo, na.rm = TRUE),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE),
            rush_yards_to_go = sum(ydstogo * rush) / sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush) / sum(rush, na.rm = TRUE),
            rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush) / sum(rush, na.rm = TRUE),
            rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)
  )

########################################################################################################################
# 
########################################################################################################################

# create gt pretty table showing performance by team for given down
offense_team_down_run_pass_summary_3 %>%
  gt(rowname_col = c("posteam")) %>%
  tab_spanner(label = "All Plays",
              columns = vars(plays, rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, first_down_rate, epa_per_play)) %>%
  tab_spanner(label = "Rush",
              columns = vars(rush_yards_to_go, rush_yards_per_play, rush_success_rate, rush_first_down_rate, rush_epa_per_play)) %>%
  tab_spanner(label = "Pass",
              columns = vars(pass_yards_to_go, pass_yards_per_play, pass_success_rate, pass_first_down_rate, pass_epa_per_play)) %>%
  fmt_percent(columns = vars(rush_percent, pass_percent, expected_pass_percent, success_rate, first_down_rate, rush_success_rate, rush_first_down_rate, pass_success_rate, pass_first_down_rate), decimals = 1) %>%
  fmt_number(columns = vars(average_yards_to_go, yards_per_play, rush_yards_to_go, rush_yards_per_play, pass_yards_to_go, pass_yards_per_play), decimals = 1) %>%
  fmt_number(columns = vars(epa_per_play, rush_epa_per_play, pass_epa_per_play), decimals = 2) %>%
  cols_label(plays = "# of Plays",
             rush_percent = "Rush %",
             pass_percent = "Pass %",
             expected_pass_percent = "Expected Pass %",
             average_yards_to_go = "Yards to Go",
             yards_per_play = "Yards Per Play",
             success_rate = "Success Rate",
             first_down_rate = "1D Rate",
             epa_per_play = "EPA Per Play",
             rush_yards_to_go = "Yards to Go",
             rush_yards_per_play = "Yards Per Play",
             rush_success_rate = "Success Rate",
             rush_first_down_rate = "1D Rate",
             rush_epa_per_play = "EPA Per Play",
             pass_yards_to_go = "Yards to Go",
             pass_yards_per_play = "Yards Per Play",
             pass_success_rate = "Success Rate",
             pass_first_down_rate = "1D Rate",
             pass_epa_per_play = "EPA Per Play") %>%
  tab_header(title = "Offensive Efficiency by Team - 3rd Down",
             subtitle = "2021 season. At least 2 minutes remaining in the half, win probability between 0.1 and 0.9."
  ) %>%
  # add outer border to table (gray)
  opt_table_outline(style = "solid", width = px(3)) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "All Plays")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Rush")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Pass")) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_body(columns = vars(rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_labels(columns = vars(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add color scale
  data_color(
    columns = vars(rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, 
                   first_down_rate, epa_per_play, rush_yards_to_go, rush_yards_per_play, rush_success_rate,
                   rush_first_down_rate, rush_epa_per_play, pass_yards_to_go, pass_yards_per_play, pass_success_rate,
                   pass_first_down_rate, pass_epa_per_play),
    colors = scales::col_numeric(
      palette = c("blue", "white", "red"),
      domain = NULL
    )
  ) %>%
  # add left and right borders to body
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(columns = TRUE)) %>%
  tab_source_note(source_note = "Data from @nflfastR.") %>%
  tab_source_note(source_note = "Table by @TripleRSports.") %>%
  gtsave(path = "C:/Users/richr/OneDrive/Documents/R", file = "nfl_team_offense_2021_3rd_down.png", vwidth = 1600, vheight = 1200)

# create gt pretty table showing performance by team for given down
defense_team_down_run_pass_summary_1 %>%
  gt(rowname_col = c("posteam")) %>%
  tab_spanner(label = "All Plays",
              columns = vars(plays, rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, first_down_rate, epa_per_play)) %>%
  tab_spanner(label = "Rush",
              columns = vars(rush_yards_to_go, rush_yards_per_play, rush_success_rate, rush_first_down_rate, rush_epa_per_play)) %>%
  tab_spanner(label = "Pass",
              columns = vars(pass_yards_to_go, pass_yards_per_play, pass_success_rate, pass_first_down_rate, pass_epa_per_play)) %>%
  fmt_percent(columns = vars(rush_percent, pass_percent, expected_pass_percent, success_rate, first_down_rate, rush_success_rate, rush_first_down_rate, pass_success_rate, pass_first_down_rate), decimals = 1) %>%
  fmt_number(columns = vars(average_yards_to_go, yards_per_play, rush_yards_to_go, rush_yards_per_play, pass_yards_to_go, pass_yards_per_play), decimals = 1) %>%
  fmt_number(columns = vars(epa_per_play, rush_epa_per_play, pass_epa_per_play), decimals = 2) %>%
  cols_label(plays = "# of Plays",
             rush_percent = "Rush %",
             pass_percent = "Pass %",
             expected_pass_percent = "Expected Pass %",
             average_yards_to_go = "Yards to Go",
             yards_per_play = "Yards Per Play",
             success_rate = "Success Rate",
             first_down_rate = "1D Rate",
             epa_per_play = "EPA Per Play",
             rush_yards_to_go = "Yards to Go",
             rush_yards_per_play = "Yards Per Play",
             rush_success_rate = "Success Rate",
             rush_first_down_rate = "1D Rate",
             rush_epa_per_play = "EPA Per Play",
             pass_yards_to_go = "Yards to Go",
             pass_yards_per_play = "Yards Per Play",
             pass_success_rate = "Success Rate",
             pass_first_down_rate = "1D Rate",
             pass_epa_per_play = "EPA Per Play") %>%
  tab_header(title = "Defensive Efficiency by Team - 1st Down",
             subtitle = "2021 season. At least 2 minutes remaining in the half, win probability between 0.1 and 0.9."
  ) %>%
  # add outer border to table (gray)
  opt_table_outline(style = "solid", width = px(3)) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "All Plays")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Rush")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Pass")) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_body(columns = vars(rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_labels(columns = vars(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add color scale
  data_color(
    columns = vars(rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, 
                   first_down_rate, epa_per_play, rush_yards_to_go, rush_yards_per_play, rush_success_rate,
                   rush_first_down_rate, rush_epa_per_play, pass_yards_to_go, pass_yards_per_play, pass_success_rate,
                   pass_first_down_rate, pass_epa_per_play),
    colors = scales::col_numeric(
      palette = c("blue", "white", "red"),
      domain = NULL
    )
  ) %>%
  # add left and right borders to body
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(columns = TRUE)) %>%
  tab_source_note(source_note = "Data from @nflfastR.") %>%
  tab_source_note(source_note = "Table by @TripleRSports.") %>%
  gtsave(path = "C:/Users/richr/OneDrive/Documents/R", file = "nfl_team_defense_2021_1st_down.png", vwidth = 1600, vheight = 1200)

################################################################################
# 
################################################################################

## should this exclude penalties?
compare_team_down_performance_to_league <- function(pbp_df, seasons = c(2022), 
                                                    team_column_name = "posteam",
                                                    team_weeks = c(1:18),
                                                    league_weeks = c(1:18),
                                                    down_list = c(1,2,3),
                                                    min_wp = 0, max_wp = 1, min_half_seconds_remaining = -1) {
  
  # summary of team performance by down
  team_down_summary <- pbp_df %>%
    filter(!is.na(down)) %>%
    filter(pass == 1 | rush == 1) %>%
    filter(play_type_nfl != "PENALTY") %>%
    filter(season %in% seasons) %>%
    filter(week %in% team_weeks) %>%
    filter(down %in% down_list) %>%
    filter(wp >= min_wp & wp <= max_wp) %>%
    filter(half_seconds_remaining > min_half_seconds_remaining) %>%
    group_by_at(c(team_column_name, "down")) %>%
    summarize(plays = n(),
              rush_percent = mean(rush, na.rm = TRUE),
              pass_percent = mean(pass, na.rm = TRUE),
              expected_pass_percent = mean(xpass, na.rm = TRUE),
              average_yards_to_go = mean(ydstogo, na.rm = TRUE),
              yards_per_play = mean(yards_gained, na.rm = TRUE),
              success_rate = mean(success, na.rm = TRUE),
              #first_down_rate = mean(first_down, na.rm = TRUE),
              epa_per_play = mean(epa, na.rm = TRUE),
              rush_yards_to_go = sum(ydstogo * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              rush_yards_per_play = sum(yards_gained * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              #rush_first_down_rate = sum(first_down * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              #pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE))
  
  # convert team down summary table to data frame
  team_down_summary <- as.data.frame(team_down_summary)
  # change first column name to team
  colnames(team_down_summary)[1] <- "team"
  
  # list of teams
  teams_list <- sort(unique(pbp_df$posteam))
  
  # summarize league-wide performance by play type and down
  league_down_summary <- pbp_df %>%
    filter(!is.na(down)) %>%
    filter(pass == 1 | rush == 1) %>%
    filter(play_type_nfl != "PENALTY") %>%
    filter(season %in% seasons) %>%
    filter(week %in% league_weeks) %>%
    filter(down %in% down_list) %>%
    filter(wp >= min_wp & wp <= max_wp) %>%
    filter(half_seconds_remaining > min_half_seconds_remaining) %>%
    group_by(down) %>%
    summarize(plays = n(),
              rush_percent = mean(rush, na.rm = TRUE),
              pass_percent = mean(pass, na.rm = TRUE),
              expected_pass_percent = mean(xpass, na.rm = TRUE),
              average_yards_to_go = mean(ydstogo, na.rm = TRUE),
              yards_per_play = mean(yards_gained, na.rm = TRUE),
              success_rate = mean(success, na.rm = TRUE),
              #first_down_rate = mean(first_down, na.rm = TRUE),
              epa_per_play = mean(epa, na.rm = TRUE),
              rush_yards_to_go = sum(ydstogo * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              rush_yards_per_play = sum(yards_gained * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              #rush_first_down_rate = sum(first_down * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
              pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              #pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
              pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)) %>%
    mutate(team = "League")
  
  # merge league and team passing summary tables by air yards segment and pass location
  team_league_combined <- rbind(team_down_summary, league_down_summary)
  team_league_combined$team <- factor(team_league_combined$team, levels = c(teams_list, "League"))
  
  team_league_combined <- team_league_combined %>%
    arrange(down, team)
  
  return(team_league_combined)
}

# generate a gt object 
create_team_offense_down_run_pass_gt <- function(comparison_df, min_wp = 0, max_wp = 1, min_half_seconds_remaining = 120, 
                                                 team_name_compare, season_year, first_down_vars_highlight, 
                                                 second_down_vars_highlight, third_down_vars_highlight) {
  
  # find team colors
  team_color1 = nfl_logos$team_color[which(nfl_logos$team_abbr == team_name_compare)]
  team_color2 = nfl_logos$team_color2[which(nfl_logos$team_abbr == team_name_compare)]
  team_color3 = nfl_logos$team_color3[which(nfl_logos$team_abbr == team_name_compare)]
  
  comparison_gt <- team_offense_down_run_pass_summary %>%
    mutate(down_string = ifelse(down == 1, "1st Down",
                                ifelse(down == 2, "2nd Down",
                                       ifelse(down == 3, "3rd Down", paste0(down, "th Down"))))) %>%
    select(-down) %>%
    filter(team == team_name_compare | team == "League") %>%
    group_by(down_string) %>%
    gt(rowname_col = c("team")) %>%
    tab_spanner(label = "All Plays",
                columns = c(plays, rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, epa_per_play)) %>%
    tab_spanner(label = "Rush",
                columns = c(rush_yards_to_go, rush_yards_per_play, rush_success_rate, rush_epa_per_play)) %>%
    tab_spanner(label = "Pass",
                columns = c(pass_yards_to_go, pass_yards_per_play, pass_success_rate, pass_epa_per_play)) %>%
    fmt_percent(columns = c(rush_percent, pass_percent, expected_pass_percent, success_rate, rush_success_rate, pass_success_rate), decimals = 1) %>%
    fmt_number(columns = c(average_yards_to_go, yards_per_play, rush_yards_to_go, rush_yards_per_play, pass_yards_to_go, pass_yards_per_play), decimals = 1) %>%
    fmt_number(columns = c(epa_per_play, rush_epa_per_play, pass_epa_per_play), decimals = 2) %>%
    cols_label(team = "Team",
               plays = "# of Plays",
               rush_percent = "Rush %",
               pass_percent = "Pass %",
               expected_pass_percent = "Expected Pass %",
               average_yards_to_go = "Yards to Go",
               yards_per_play = "Yards Per Play",
               success_rate = "Success Rate",
               #first_down_rate = "First Down Rate",
               epa_per_play = "EPA Per Play",
               rush_yards_to_go = "Yards to Go",
               rush_yards_per_play = "Yards Per Play",
               rush_success_rate = "Success Rate",
               #rush_first_down_rate = "First Down Rate",
               rush_epa_per_play = "EPA Per Play",
               pass_yards_to_go = "Yards to Go",
               pass_yards_per_play = "Yards Per Play",
               pass_success_rate = "Success Rate",
               #pass_first_down_rate = "First Down Rate",
               pass_epa_per_play = "EPA Per Play") %>%
    tab_header(title = html(paste0("Offensive Efficiency by Down - ", team_name_compare), 
                            web_image(url = nfl_logos$team_logo_espn[which(nfl_logos$team_abbr == team_name_compare)], height = px(50))),
               subtitle = paste0(season_year, " season. Penalties removed. At least ", min_half_seconds_remaining, 
                                 " seconds remaining in the half, win probability between ", 
                                 min_wp * 100, "% and ", max_wp * 100, "%.")
    ) %>%
    # add outer border to table (gray)
    opt_table_outline(style = "solid", width = px(3)) %>%
    # add left border to column spanner
    tab_style(
      style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
      locations = cells_column_spanners(spanners = "All Plays")) %>%
    # add left border to column spanner
    tab_style(
      style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
      locations = cells_column_spanners(spanners = "Rush")) %>%
    # add left border to column spanner
    tab_style(
      style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
      locations = cells_column_spanners(spanners = "Pass")) %>%
    # add left and right borders to body
    tab_style(
      style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
      locations = cells_body(columns = c(plays, rush_yards_to_go, pass_yards_to_go))) %>%
    # add left and right borders to column labels
    tab_style(
      style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
      locations = cells_column_labels(columns = c(plays, rush_yards_to_go, pass_yards_to_go))) %>%
    # bold text in column labels
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_column_labels(columns = everything())) %>%
    # add color to team cells
    # tab_style(style = cell_fill(color = "blue"),
    #           locations = cells_body(columns = vars(rush_epa_per_play),
    #                                  rows = (posteam != "League" & down == 1))
    #           ) %>%
    tab_source_note(source_note = "Data from @nflfastR.") %>%
    tab_source_note(source_note = "Table by @TripleRSports.") %>%
    tab_style(
      style = cell_fill(color = team_color1),
      locations = cells_body(
        columns = all_of(first_down_vars_highlight),
        rows = (down_string == "1st Down" & team == team_name_compare)
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = team_color2),
      locations = cells_body(
        columns = all_of(first_down_vars_highlight),
        rows = (down_string == "1st Down" & team == team_name_compare)
      )
    ) %>%
    tab_style(
      style = cell_fill(color = team_color1),
      locations = cells_body(
        columns = all_of(second_down_vars_highlight),
        rows = (down_string == "2nd Down" & team == team_name_compare)
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = team_color2),
      locations = cells_body(
        columns = all_of(second_down_vars_highlight),
        rows = (down_string == "2nd Down" & team == team_name_compare)
      )
    ) %>%
    tab_style(
      style = cell_fill(color = team_color1),
      locations = cells_body(
        columns = all_of(third_down_vars_highlight),
        rows = (down_string == "3rd Down" & team == team_name_compare)
      )
    ) %>%
    tab_style(
      style = cell_text(weight = "bold", color = team_color2),
      locations = cells_body(
        columns = all_of(third_down_vars_highlight),
        rows = (down_string == "3rd Down" & team == team_name_compare)
      )
    ) %>% 
    tab_options(
      table.font.size = "small",
    )
  
  return(comparison_gt)
}

########################################################################################################################
# 
########################################################################################################################

offense_team = "SEA"
min_wp1 = 0.05
max_wp1 = 0.95
min_half_seconds_remaining1 = 0
season_year1 = 2022

# performance on offense by teams by down and play type
team_offense_down_run_pass_summary <- compare_team_down_performance_to_league(pbp_df = pbp_data, 
                                                                              team_column_name = "posteam",
                                                                              seasons = season_year1,
                                                                              #team_weeks = c(11:14),
                                                                              #league_weeks = c(11),
                                                                              min_wp = min_wp1, max_wp = max_wp1, 
                                                                              min_half_seconds_remaining = min_half_seconds_remaining1)

first_down_vars_highlight_list = c("pass_percent", "pass_success_rate")
second_down_vars_highlight_list = c()
third_down_vars_highlight_list = c()

create_team_offense_down_run_pass_gt(comparison_df = team_offense_down_run_pass_summary, min_wp = min_wp1, max_wp = max_wp1,
                                     min_half_seconds_remaining = min_half_seconds_remaining1, 
                                     team_name_compare = offense_team, season_year = season_year1, 
                                     first_down_vars_highlight = first_down_vars_highlight_list, 
                                     second_down_vars_highlight = second_down_vars_highlight_list, 
                                     third_down_vars_highlight = third_down_vars_highlight_list) %>%
  gtsave(path = "C:/Users/richr/OneDrive/Documents/R", 
         file = paste0("nfl_offense_play_type_by_down_", offense_team, "_", season_year1, "_week_1_2.png"), 
         vwidth = 1600, vheight = 1200)

# create gt pretty table for team offense
team_offense_down_run_pass_summary %>%
  mutate(down_string = ifelse(down == 1, "1st Down",
                              ifelse(down == 2, "2nd Down",
                                     ifelse(down == 3, "3rd Down", paste0(down, "th Down"))))) %>%
  select(-down) %>%
  filter(team == offense_team | team == "League") %>%
  group_by(down_string) %>%
  gt(rowname_col = c("team")) %>%
  tab_spanner(label = "All Plays",
              columns = c(plays, rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, first_down_rate, epa_per_play)) %>%
  tab_spanner(label = "Rush",
    columns = c(rush_yards_to_go, rush_yards_per_play, rush_success_rate, rush_first_down_rate, rush_epa_per_play)) %>%
  tab_spanner(label = "Pass",
              columns = c(pass_yards_to_go, pass_yards_per_play, pass_success_rate, pass_first_down_rate, pass_epa_per_play)) %>%
  fmt_percent(columns = c(rush_percent, pass_percent, expected_pass_percent, success_rate, first_down_rate, rush_success_rate, rush_first_down_rate, pass_success_rate, pass_first_down_rate), decimals = 1) %>%
  fmt_number(columns = c(average_yards_to_go, yards_per_play, rush_yards_to_go, rush_yards_per_play, pass_yards_to_go, pass_yards_per_play), decimals = 1) %>%
  fmt_number(columns = c(epa_per_play, rush_epa_per_play, pass_epa_per_play), decimals = 2) %>%
  cols_label(team = "Team",
             plays = "# of Plays",
             rush_percent = "Rush %",
             pass_percent = "Pass %",
             expected_pass_percent = "Expected Pass %",
             average_yards_to_go = "Yards to Go",
             yards_per_play = "Yards Per Play",
             success_rate = "Success Rate",
             first_down_rate = "First Down Rate",
             epa_per_play = "EPA Per Play",
             rush_yards_to_go = "Yards to Go",
             rush_yards_per_play = "Yards Per Play",
             rush_success_rate = "Success Rate",
             rush_first_down_rate = "First Down Rate",
             rush_epa_per_play = "EPA Per Play",
             pass_yards_to_go = "Yards to Go",
             pass_yards_per_play = "Yards Per Play",
             pass_success_rate = "Success Rate",
             pass_first_down_rate = "First Down Rate",
             pass_epa_per_play = "EPA Per Play") %>%
  tab_header(title = html(paste0("Offensive Efficiency by Down - ", offense_team), 
                          web_image(url = nfl_logos$team_logo_espn[which(nfl_logos$team_abbr == offense_team)], height = px(50))),
             subtitle = "2021 season. Penalties removed. At least 2 minutes remaining in the half, win probability between 10% and 90%."
  ) %>%
  # add outer border to table (gray)
  opt_table_outline(style = "solid", width = px(3)) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "All Plays")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Rush")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Pass")) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_body(columns = c(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_labels(columns = c(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(columns = everything())) %>%
  # add color to team cells
  # tab_style(style = cell_fill(color = "blue"),
  #           locations = cells_body(columns = vars(rush_epa_per_play),
  #                                  rows = (posteam != "League" & down == 1))
  #           ) %>%
  tab_source_note(source_note = "Data from @nflfastR.") %>%
  tab_source_note(source_note = "Table by @TripleRSports.") %>%
  tab_style(
    style = cell_fill(color = offense_team_color1),
    locations = cells_body(
      columns = c("pass_epa_per_play"),
      rows = (down_string == "1st Down" & team == offense_team)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = offense_team_color2),
    locations = cells_body(
      columns = c("pass_epa_per_play"),
      rows = (down_string == "1st Down" & team == offense_team)
    )
  ) %>%
  tab_style(
    style = cell_fill(color = offense_team_color1),
    locations = cells_body(
      columns = c(first_down_rate, pass_epa_per_play),
      rows = (down_string == "2nd Down" & team == offense_team)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = offense_team_color2),
    locations = cells_body(
      columns = c(first_down_rate, pass_epa_per_play),
      rows = (down_string == "2nd Down" & team == offense_team)
    )
  ) %>%
  tab_style(
    style = cell_fill(color = offense_team_color1),
    locations = cells_body(
      columns = c(pass_epa_per_play),
      rows = (down_string == "3rd Down" & team == offense_team)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = offense_team_color2),
    locations = cells_body(
      columns = c(pass_epa_per_play),
      rows = (down_string == "3rd Down" & team == offense_team)
    )
  ) %>%
  gtsave(path = "C:/Users/richr/OneDrive/Documents/R", 
         file = paste0("nfl_offense_play_type_by_down_", offense_team, "_2021_week_1_1.png"), vwidth = 1600, vheight = 900)

########################################################################################################################
# 
########################################################################################################################

# performance against (defense) teams by down and play type
team_defense_down_run_pass_summary <- compare_team_down_performance_to_league(pbp_df = pbp, 
                                                                              team_column_name = "defteam", 
                                                                              min_wp = 0.1, max_wp = 0.9, 
                                                                              min_half_seconds_remaining = 120)

defense_team = "PIT"
defense_team_color1 = nfl_logos$team_color[which(nfl_logos$team_abbr == defense_team)]
defense_team_color2 = nfl_logos$team_color2[which(nfl_logos$team_abbr == defense_team)]

# create gt pretty table for team defense
team_defense_down_run_pass_summary %>%
  mutate(down_string = ifelse(down == 1, "1st Down",
                              ifelse(down == 2, "2nd Down",
                                     ifelse(down == 3, "3rd Down", paste0(down, "th Down"))))) %>%
  select(-down) %>%
  filter(team == defense_team | team == "League") %>%
  group_by(down_string) %>%
  gt(rowname_col = c("team")) %>%
  tab_spanner(label = "All Plays",
              columns = vars(plays, rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, first_down_rate, epa_per_play)) %>%
  tab_spanner(label = "Rush",
              columns = vars(rush_yards_to_go, rush_yards_per_play, rush_success_rate, rush_first_down_rate, rush_epa_per_play)) %>%
  tab_spanner(label = "Pass",
              columns = vars(pass_yards_to_go, pass_yards_per_play, pass_success_rate, pass_first_down_rate, pass_epa_per_play)) %>%
  fmt_percent(columns = vars(rush_percent, pass_percent, expected_pass_percent, success_rate, first_down_rate, rush_success_rate, rush_first_down_rate, pass_success_rate, pass_first_down_rate), decimals = 1) %>%
  fmt_number(columns = vars(average_yards_to_go, yards_per_play, rush_yards_to_go, rush_yards_per_play, pass_yards_to_go, pass_yards_per_play), decimals = 1) %>%
  fmt_number(columns = vars(epa_per_play, rush_epa_per_play, pass_epa_per_play), decimals = 2) %>%
  cols_label(team = "Team",
             plays = "# of Plays",
             rush_percent = "Rush %",
             pass_percent = "Pass %",
             expected_pass_percent = "Expected Pass %",
             average_yards_to_go = "Yards to Go",
             yards_per_play = "Yards Per Play",
             success_rate = "Success Rate",
             #first_down_rate = "First Down Rate",
             epa_per_play = "EPA Per Play",
             rush_yards_to_go = "Yards to Go",
             rush_yards_per_play = "Yards Per Play",
             rush_success_rate = "Success Rate",
             #rush_first_down_rate = "First Down Rate",
             rush_epa_per_play = "EPA Per Play",
             pass_yards_to_go = "Yards to Go",
             pass_yards_per_play = "Yards Per Play",
             pass_success_rate = "Success Rate",
             #pass_first_down_rate = "First Down Rate",
             pass_epa_per_play = "EPA Per Play") %>%
  tab_header(title = html(paste0("Defensive Efficiency Against by Down - ", defense_team), 
                          web_image(url = nfl_logos$team_logo_espn[which(nfl_logos$team_abbr == defense_team)], height = px(50))),
             subtitle = "2020 season. Penalties removed. At least 2 minutes remaining in the half, win probability between 10% and 90%."
  ) %>%
  # add outer border to table (gray)
  opt_table_outline(style = "solid", width = px(3)) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "All Plays")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Rush")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Pass")) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_body(columns = vars(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_labels(columns = vars(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(columns = TRUE)) %>%
  # add color to team cells
  # tab_style(style = cell_fill(color = "blue"),
  #           locations = cells_body(columns = vars(rush_epa_per_play),
  #                                  rows = (posteam != "League" & down == 1))
  #           ) %>%
  tab_source_note(source_note = "Data from @nflfastR.") %>%
  tab_source_note(source_note = "Table by @TripleRSports.") %>%
  tab_style(
    style = cell_fill(color = defense_team_color1),
    locations = cells_body(
      columns = vars(rush_yards_per_play, rush_epa_per_play, pass_epa_per_play),
      rows = (down_string == "1st Down" & team == defense_team)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = defense_team_color2),
    locations = cells_body(
      columns = vars(rush_yards_per_play, rush_epa_per_play, pass_epa_per_play),
      rows = (down_string == "1st Down" & team == defense_team)
    )
  ) %>%
  tab_style(
    style = cell_fill(color = defense_team_color1),
    locations = cells_body(
      columns = vars(average_yards_to_go, first_down_rate, rush_epa_per_play, pass_epa_per_play),
      rows = (down_string == "2nd Down" & team == defense_team)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = defense_team_color2),
    locations = cells_body(
      columns = vars(average_yards_to_go, first_down_rate, rush_epa_per_play, pass_epa_per_play),
      rows = (down_string == "2nd Down" & team == defense_team)
    )
  ) %>%
  tab_style(
    style = cell_fill(color = defense_team_color1),
    locations = cells_body(
      columns = vars(pass_percent, average_yards_to_go),
      rows = (down_string == "3rd Down" & team == defense_team)
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = defense_team_color2),
    locations = cells_body(
      columns = vars(pass_percent, average_yards_to_go),
      rows = (down_string == "3rd Down" & team == defense_team)
    )
  ) %>%
  gtsave(path = "E:/sports_analytics/nfl", file = paste0("nfl_defense_play_type_by_down_", defense_team, "_week_1_7_.png"), vwidth = 1600, vheight = 900)

########################################################################################################################
# team comparison across weeks
########################################################################################################################

offense_team = "SEA"
offense_team_color1 = nfl_logos$team_color[which(nfl_logos$team_abbr == offense_team)]
offense_team_color2 = nfl_logos$team_color3[which(nfl_logos$team_abbr == offense_team)]

# offense 1st down summary
offense_team_down_run_pass_summary_by_week <- pbp %>%
  filter(season %in% c(2020)) %>%
  filter(!is.na(down)) %>%
  filter(down %in% c(1, 2, 3)) %>%
  filter(pass == 1 | rush == 1) %>%
  #filter(play_type_nfl != "PENALTY") %>%
  filter(wp >= 0.1 & wp <= 0.9) %>%
  filter(half_seconds_remaining > 120) %>%
  filter(posteam == offense_team) %>%
  filter(week %in% c(10, 11)) %>%
  #mutate(down_group = ifelse(down < 3, "early down", "third down")) %>%
  #mutate(week_group = ifelse(week >= 10, "Week 10+", "Weeks 1-9")) %>%
  #group_by(defteam) %>%
  group_by(week, down) %>%
  summarize(plays = n(),
            rush_percent = mean(rush, na.rm = TRUE),
            pass_percent = mean(pass, na.rm = TRUE),
            expected_pass_percent = mean(xpass, na.rm = TRUE),
            average_yards_to_go = mean(ydstogo, na.rm = TRUE),
            yards_per_play = mean(yards_gained, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE),
            first_down_rate = mean(first_down, na.rm = TRUE),
            epa_per_play = mean(epa, na.rm = TRUE),
            rush_yards_to_go = sum(ydstogo * rush) / sum(rush, na.rm = TRUE),
            rush_yards_per_play = sum(yards_gained * rush) / sum(rush, na.rm = TRUE),
            rush_success_rate = sum(success * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            rush_first_down_rate = sum(first_down * rush) / sum(rush, na.rm = TRUE),
            rush_epa_per_play = sum(epa * rush, na.rm = TRUE) / sum(rush, na.rm = TRUE),
            pass_yards_to_go = sum(ydstogo * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_yards_per_play = sum(yards_gained * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_success_rate = sum(success * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_first_down_rate = sum(first_down * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE),
            pass_epa_per_play = sum(epa * pass, na.rm = TRUE) / sum(pass, na.rm = TRUE)
  )
  

# create gt pretty table for team offense
offense_team_down_run_pass_summary_by_week %>%
  mutate(down_string = ifelse(down == 1, "1st Down",
                              ifelse(down == 2, "2nd Down",
                                     ifelse(down == 3, "3rd Down", paste0(down, "th Down"))))) %>%
  mutate(week_string = paste0("Week ", week)) %>%
  select(-down) %>%
  group_by(down_string) %>%
  select(-week) %>%
  gt(rowname_col = c("week_string")) %>%
  tab_spanner(label = "All Plays",
              columns = vars(plays, rush_percent, pass_percent, expected_pass_percent, average_yards_to_go, yards_per_play, success_rate, first_down_rate, epa_per_play)) %>%
  tab_spanner(label = "Rush",
              columns = vars(rush_yards_to_go, rush_yards_per_play, rush_success_rate, rush_first_down_rate, rush_epa_per_play)) %>%
  tab_spanner(label = "Pass",
              columns = vars(pass_yards_to_go, pass_yards_per_play, pass_success_rate, pass_first_down_rate, pass_epa_per_play)) %>%
  fmt_percent(columns = vars(rush_percent, pass_percent, expected_pass_percent, success_rate, first_down_rate, rush_success_rate, rush_first_down_rate, pass_success_rate, pass_first_down_rate), decimals = 1) %>%
  fmt_number(columns = vars(average_yards_to_go, yards_per_play, rush_yards_to_go, rush_yards_per_play, pass_yards_to_go, pass_yards_per_play), decimals = 1) %>%
  fmt_number(columns = vars(epa_per_play, rush_epa_per_play, pass_epa_per_play), decimals = 2) %>%
  cols_label(week_string = "Week",
             plays = "# of Plays",
             rush_percent = "Rush %",
             pass_percent = "Pass %",
             expected_pass_percent = "Expected Pass %",
             average_yards_to_go = "Yards to Go",
             yards_per_play = "Yards Per Play",
             success_rate = "Success Rate",
             first_down_rate = "First Down Rate",
             epa_per_play = "EPA Per Play",
             rush_yards_to_go = "Yards to Go",
             rush_yards_per_play = "Yards Per Play",
             rush_success_rate = "Success Rate",
             rush_first_down_rate = "First Down Rate",
             rush_epa_per_play = "EPA Per Play",
             pass_yards_to_go = "Yards to Go",
             pass_yards_per_play = "Yards Per Play",
             pass_success_rate = "Success Rate",
             pass_first_down_rate = "First Down Rate",
             pass_epa_per_play = "EPA Per Play") %>%
  tab_header(title = html(paste0("Offensive Efficiency by Down - ", offense_team), web_image(url = nfl_logos$team_logo_espn[which(nfl_logos$team_abbr == offense_team)], height = px(50))),
             subtitle = "2020 season, weeks 10 and 11. Penalties removed. At least 2 minutes remaining in the half, win probability between 10% and 90%."
  ) %>%
  # add outer border to table (gray)
  opt_table_outline(style = "solid", width = px(3)) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "All Plays")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Rush")) %>%
  # add left border to column spanner
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_spanners(spanners = "Pass")) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_body(columns = vars(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = cell_borders(sides = c("left"), color = "#D3D3D3", weight = px(3), style = "solid"),
    locations = cells_column_labels(columns = vars(plays, rush_yards_to_go, pass_yards_to_go))) %>%
  # add left and right borders to body
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(columns = TRUE)) %>%
  # add color to team cells
  # tab_style(style = cell_fill(color = "blue"),
  #           locations = cells_body(columns = vars(rush_epa_per_play),
  #                                  rows = (posteam != "League" & down == 1))
  #           ) %>%
  tab_source_note(source_note = "Data from @nflfastR.") %>%
  tab_source_note(source_note = "Table by @TripleRSports.") %>%
  tab_style(
    style = cell_fill(color = offense_team_color1),
    locations = cells_body(
      columns = vars(rush_epa_per_play),
      rows = (down_string == "1st Down")
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = offense_team_color2),
    locations = cells_body(
      columns = vars(rush_epa_per_play),
      rows = (down_string == "1st Down")
    )
  ) %>%
  tab_style(
    style = cell_fill(color = offense_team_color1),
    locations = cells_body(
      columns = vars(pass_percent, pass_epa_per_play),
      rows = (down_string == "2nd Down")
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = offense_team_color2),
    locations = cells_body(
      columns = vars(pass_percent, pass_epa_per_play),
      rows = (down_string == "2nd Down")
    )
  ) %>%
  tab_style(
    style = cell_fill(color = offense_team_color1),
    locations = cells_body(
      columns = vars(average_yards_to_go, epa_per_play, pass_epa_per_play),
      rows = (down_string == "3rd Down")
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = offense_team_color2),
    locations = cells_body(
      columns = vars(average_yards_to_go, epa_per_play, pass_epa_per_play),
      rows = (down_string == "3rd Down")
    )
  ) %>%
  gtsave(path = "E:/sports_analytics/nfl", file = paste0("offense_play_type_by_down_and_week_", offense_team, "_2020_week_10_11_.png"), vwidth = 1600, vheight = 900)