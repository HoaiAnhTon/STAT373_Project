draft <- player.roster %>%
  filter(Average.by.Match != "-" & Position != "COACH")
draft$Average.by.Match <- as.numeric(draft$Average.by.Match)
cor(draft$Height,draft$Average.by.Match)
plot(draft$Height,draft$Average.by.Match)
cor.test(draft$Height,draft$Average.by.Match)

player.indi <- player.indi %>%
  mutate(Match = paste(TeamA,TeamB,sep = "-"))
player.indi$Player_ID <- as.character(player.indi$Player_ID)
combine <- full_join(player.indi,player.roster,by="Player_ID")
combine <- combine %>%
  select(Player_ID,Year.x,TeamA,TeamB,Match,No.,Player.Name,Position,
         Country_Name,Height)
combine <- combine %>%
  group_by(Match,Country_Name) %>%
  summarise(Average_Height = mean(Height, na.rm = TRUE))

match.result <- match.result %>%
  mutate(TEAM1 = ifelse(TEAM1 == "United States", "USA", TEAM1)) %>%
  mutate(TEAM2 = ifelse(TEAM2 == "United States", "USA", TEAM2)) %>%
  mutate(Match = paste(TEAM1,TEAM2,sep = "-"))

final.combine <- full_join(match.result,combine,by="Match",
                           relationship = "many-to-many") %>%
  filter(is.na(MATCHID) == FALSE) %>%
  filter(Match != "China-Germany")

compute_score_difference <- function(scores) {
  score_pairs <- str_split(scores, "\n", simplify = TRUE) # Split into sets
  score_diffs <- sapply(score_pairs, function(x) {
    points <- as.numeric(str_split(x, "-", simplify = TRUE)) # Split each set
    points[1] - points[2] # Compute difference
  })
  sum(score_diffs, na.rm = TRUE) # Sum the differences
}

final.combine <- final.combine %>% 
  mutate(Detailed_Score_Difference = sapply(SETSCORES, compute_score_difference))
final.combine<- final.combine %>%
  group_by(Match) %>%
  summarise(Difference_in_Height = Average_Height[1] - Average_Height[2],
            Detailed_Score_Difference = mean(Detailed_Score_Difference)) %>%
  filter(Match != "-", is.na(Detailed_Score_Difference) == FALSE)
cor(final.combine$Difference_in_Height,final.combine$Detailed_Score_Difference)
plot(final.combine$Difference_in_Height,final.combine$Detailed_Score_Difference)

