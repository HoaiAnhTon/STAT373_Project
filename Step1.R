match.result <- read.csv("VNLData.csv")
player.roster <- read.csv("df_mens_rosters_21_23.csv")
player.indi <- read.csv("df_mens_indv_21_23.csv")

#install.packages("tidyverse") 
#install.packages("tibble")
#install.packages("scales")
library("tidyverse")

match.result <- match.result %>% 
  select(MATCHID,TEAM1,TEAM2,SCORES,SETSCORES) %>%
  mutate(MATCHID = as.character(MATCHID)) %>%
  filter(startsWith(MATCHID,"13"))
player.indi <- player.indi %>% 
  select(Player_ID,Year,TeamA,TeamB) %>%
  filter(Year == 2022)
player.roster <- player.roster %>%
  filter(Year == 2022)
