library(tidyverse)
library(cfbscrapR)
library(ggplot2)
library(ggimage)

alabama_pbp <- data.frame()

for(j in 2014:2019){
  print(j)
  for(i in 1:15){
    print(i)
    data <- cfb_pbp_data(year = j, season_type = "regular", week = i,
                         team = "Alabama", play_type = NULL, 
                         epa_wpa = TRUE)
    alabama_pbp <- bind_rows(alabama_pbp, data)
  }
}

for(i in 2014:2019){
  print(i)
  data <- cfb_pbp_data(year = i, season_type = "postseason",
                       team = "Alabama", play_type = NULL,
                       epa_wpa = TRUE)
  alabama_pbp <- bind_rows(alabama_pbp, data)
}

##make sure to check current week
current_week = 14
for(i in 1:current_week){
  print(i)
  data <- cfb_pbp_data(year = 2020, season_type = "regular", week = i,
                       team = "Alabama", play_type = NULL, 
                       epa_wpa = TRUE)
  alabama_pbp <- bind_rows(alabama_pbp, data)
}


seasons <- 2014:2020

reg_game_info <- map_dfr(seasons, function(x){
  cfb_game_info(x, season_type = "regular")
})

pos_game_info <- map_dfr(seasons, function(x){
  cfb_game_info(x, season_type = "postseason")
})

game_info <- bind_rows(reg_game_info, pos_game_info)

alabama_pbp <- alabama_pbp %>%
  left_join(game_info, by = c("game_id" = "game_id"))

power_5 <- c("SEC", "ACC", "Big Ten", "Big 12", "Pac-12")

alabama_def <- alabama_pbp %>%
  filter(pass == 1 | rush == 1,
         def_pos_team == "Alabama",
         penalty_no_play == FALSE,
         home_conference %in% power_5 & away_conference %in% power_5) %>%
  select(year, pos_team, play_type, play_text, def_EPA, success, rush, pass) %>%
  arrange(year, pos_team)

cfblogos <- read_csv("https://raw.githubusercontent.com/spfleming/CFB/master/logos.csv") %>%
  select(school, logo)
teams <- cfb_team_info() %>%
  mutate(school = ifelse(school == "UT San Antonio", "UTSA", ifelse(school == "Southern Mississippi", "Southern Miss", school)))
primary_colors <- teams$color
names(primary_colors) <- teams$school

def_epa_alabama <- alabama_def %>%
  group_by(year, pos_team) %>%
  summarize(plays = n(),
            def_epa_per_play = mean(def_EPA, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE)) %>%
  left_join(cfblogos, by=c("pos_team" = "school")) %>%
  left_join(teams, by=c("pos_team" = "school")) %>%
  mutate(pos_team = ifelse(pos_team == "UT San Antonio", "UTSA", ifelse(pos_team == "Southern Mississippi", "Southern Miss", pos_team)),
         team_year = paste(pos_team, year)) %>%
  rename(primary_color = color) %>%
  arrange(desc(def_epa_per_play))

by_year <- alabama_def %>%
  filter(!(year == 2020 & pos_team == "Ole Miss")) %>%
  group_by(year) %>%
  summarize(plays = n(),
            def_epa_per_play = mean(def_EPA, na.rm = TRUE),
            success_rate = mean(success, na.rm = TRUE))

def_epa_alabama_top10 <- def_epa_alabama[1:10,]

secondary_colors <- def_epa_alabama_top10$alt_color
names(colors_all) <- teams$school

def_epa_alabama_top10 %>%
  ggplot(aes(x=reorder(team_year, desc(def_epa_per_play)), y=def_epa_per_play, fill = pos_team, color = pos_team)) +
  geom_col(alpha = 1, width=.7) +
  scale_fill_manual(values = primary_colors) +
  scale_color_manual(values = secondary_colors) +
  geom_image(aes(x=reorder(team_year, desc(def_epa_per_play)), y=def_epa_per_play + .02,
                 image = reorder(logo, desc(def_epa_per_play))), size=.08, 
             asp=45/25, inherit.aes=FALSE) +
  scale_y_continuous(expand = expansion(mult=c(0,0.1))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=20, hjust=.5),
        plot.subtitle = element_text(size=12, hjust=.5),
        plot.caption = element_text(size=10),
        legend.position = "none") +
  labs(x="Team and Year", 
       y="EPA Per Play",
       title="Alabama Single Game Defensive Performances",
       subtitle="By Defensive EPA Per Play, 2014–2020",
       caption="Data from @CFB_Data via @cfbscrapR | Graph by @jcarterspires")
