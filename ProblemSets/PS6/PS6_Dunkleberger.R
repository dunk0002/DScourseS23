#!/usr/bin/env Rscript

#installing necessary packages
install.packages("tidyverse")
install.packages("devtools")
install.packages("sportyR")
devtools::install_github("danmorse314/hockeyR")
install.packages("ggrepel")
library(tidyverse)
library(sportyR)
library(ggrepel)

#NHL Expected Goals vs Goals Scored
#Formatting a goal table
pbp <- load_pbp('2018-19')
goalChart <- pbp %>%
  filter(event_type %in% c("SHOT","MISSED_SHOT","GOAL")) %>%
  filter(season_type == "R" & period_type != "SHOOTOUT") %>%
  group_by(player = event_player_1_name, id = event_player_1_id, season) %>%
  summarize(
    team = last(event_team_abbr),
    goals = sum(event_type == "GOAL"),
    xg = round(sum(xg, na.rm = TRUE),1),
    gax = goals - xg,
    .groups = "drop"
  ) %>%
  arrange(-goals) %>%
  left_join(team_logos_colors, by = c("team" = "team_abbr")) %>%
  filter(goals >=15)
head(goalChart)

#Chart
goalChart %>%
  ggplot(aes(x = goals, y = xg)) +
  geom_hline(yintercept = mean(goalChart$goals), linetype = "dashed") +
  geom_vline(xintercept = mean(goalChart$xg), linetype = "dashed") +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x =Goals Scored",
       y = "Expected Goals",
       title = "2018-2019 Expected Goals vs Goals Scored",
       subtitle = "Minimum 15 goals scored",
       caption = "By Davis Dunkleberger | @soonerfan432") +
  theme(plot.title = element_text( size = 18, hjust = 0.5))

ggsave('PS6a_Dunkleberger.png', dpi = "retina")

#Shot chart for an entire game
# get single game
game <- pbp %>%
  filter(game_date == "2019-05-07" & home_abbreviation == "STL")

# grab team logos & colors
team_logos <- hockeyR::team_logos_colors %>%
  filter(team_abbr == unique(game$home_abbreviation) | team_abbr == unique(game$away_abbreviation)) %>%
  # add in dummy variables to put logos on the ice
  mutate(x = ifelse(full_team_name == unique(game$home_name), 50, -50),
         y = 0)

# add transparency to logo
transparent <- function(img) {
  magick::image_fx(img, expression = "0.3*a", channel = "alpha")
}

# get only shot events
fenwick_events <- c("MISSED_SHOT","SHOT","GOAL")

shots <- game %>% filter(event_type %in% fenwick_events) %>%
  # adding team colors
  left_join(team_logos, by = c("event_team_abbr" = "team_abbr"))

# create shot plot
geom_hockey("nhl") +
  ggimage::geom_image(
    data = team_logos,
    aes(x = x, y = y, image = team_logo_espn),
    image_fun = transparent, size = 0.22, asp = 2.35
  ) +
  geom_point(
    data = shots,
    aes(x_fixed, y_fixed),
    size = 6,
    color = shots$team_color1,
    shape = ifelse(shots$event_type == "GOAL", 19, 1)
  ) +
  labs(
    title = glue::glue("{unique(game$away_name)} @ {unique(game$home_name)}"),
    subtitle = glue::glue(
      "{unique(game$game_date)}\n
    {unique(shots$away_abbreviation)} {unique(shots$away_final)} - {unique(shots$home_final)} {unique(shots$home_abbreviation)}"
    ),
    caption = "data from hockeyR | plot made with sportyR | made by Davis Dunkleberger"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = .9)
  )

ggsave('PS6b_Dunkleberger.png', dpi = 'retina')

#Stars Expected Goals vs Goals Scored
StarsGoalChart <- pbp %>%
  filter(event_type %in% c("SHOT","MISSED_SHOT","GOAL")) %>%
  filter(season_type == "R" & period_type != "SHOOTOUT") %>%
  group_by(player = event_player_1_name, id = event_player_1_id, season) %>%
  summarize(
    team = last(event_team_abbr),
    goals = sum(event_type == "GOAL"),
    xg = round(sum(xg, na.rm = TRUE),1),
    gax = goals - xg,
    .groups = "drop"
  ) %>%
  arrange(-goals) %>%
  left_join(team_logos_colors, by = c("team" = "team_abbr")) %>%
  filter(goals >=5) %>%
  filter(team == "DAL") 

StarsGoalChart %>%
  ggplot(aes(x = goals, y = xg)) +
  geom_hline(yintercept = mean(StarsGoalChart$goals), linetype = "dashed") +
  geom_vline(xintercept = mean(StarsGoalChart$xg), linetype = "dashed") +
  geom_point(color = StarsGoalChart$team_color1) +
  geom_label_repel(aes(label = player), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') +
  theme_bw() +
  labs(x = "Goals Scored",
       y = "Expected Goals",
       title = "Dallas Stars Expected Goals vs Goals Scored",
       subtitle = "2018-2019 Regular Season; Minimum 5 goals scored",
       caption = "By Davis Dunkleberger | @soonerfan432") +
  theme(plot.title = element_text( size = 18, hjust = 0.5))

ggsave('PS6c_Dunkleberger.png', dpi = "retina")

