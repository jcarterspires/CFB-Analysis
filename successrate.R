library(tidyverse)
library(cfbscrapR)

pbp20 <- data.frame()
seasons <- 2020
pbp20 <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})

games20 <- cfb_game_info(2020)
pbp20 <- left_join(pbp20, games20, by = c("game_id" = "game_id"))

pbp20_clean <- pbp20 %>%
  filter(pass == 1 | rush == 1) %>%
  filter(!is.na(home_conference) & !is.na(away_conference)) %>%
  mutate(abs_diff = abs(score_diff),
         garbage = ifelse(period == 1 & abs_diff > 43, 1, 
                          ifelse(period == 2 & abs_diff > 37, 1,
                                 ifelse(period == 3 & abs_diff > 27, 1,
                                        ifelse(period == 4 & abs_diff > 22, 1, 0)))),
         success = ifelse(down == 1 & yards_gained > .5*distance, 1,
                          ifelse(down == 2 & yards_gained > .7*distance, 1,
                                 ifelse((down == 3 | down == 4) & yards_gained >=distance, 1, 0))))


offense <- pbp20_clean %>%
  group_by(pos_team) %>%
  summarize(SR = round(sum(success)/length(success), 3)*100,
            rush_sr = round(sum(success[rush == 1])/length(success[rush == 1]), 3)*100,
            pass_sr = round(sum(success[pass == 1])/length(success[pass == 1]), 3)*100) %>%
  mutate(`Offense Rank` = dense_rank(desc(SR)),
         `Rushing Offense Rank` = dense_rank(desc(rush_sr)),
         `Passing Offense Rank` = dense_rank(desc(pass_sr))) %>%
  rename(Team = pos_team,
         `Success Rate (Offense)` = SR,
         `Rushing SR (Offense)` = rush_sr,
         `Passing SR (Offense)` = pass_sr)

defense <- pbp20_clean %>%
  group_by(def_pos_team) %>%
  summarize(SR = round(sum(success)/length(success), 3)*100,
            rush_sr = round(sum(success[rush == 1])/length(success[rush == 1]), 3)*100,
            pass_sr = round(sum(success[pass == 1])/length(success[pass == 1]), 3)*100) %>%
  mutate(`Defense Rank` = dense_rank(SR),
         `Rushing Defense Rank` = dense_rank(rush_sr),
         `Passing Defense Rank` = dense_rank(pass_sr)) %>%
  rename(Team = def_pos_team,
         `Success Rate (Defense)` = SR,
         `Rushing SR (Defense)` = rush_sr,
         `Passing SR (Defense)` = pass_sr)

combined_SR <- left_join(offense, defense, by = c("Team" = "Team")) %>%
  mutate(Differential = `Success Rate (Offense)` - `Success Rate (Defense)`,
         `Differential Rank` = dense_rank(desc(Differential)))

write_csv(combined_SR, "successrates.csv")
