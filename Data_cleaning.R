library(nflreadr)
library(tidyverse)
library(corrplot)
#load data 2004 to 2023
pbp=load_pbp(seasons = 2006:2023)
#filter to only regular season and no aborted plays
pbp = pbp %>% 
  filter(season_type=="REG", aborted_play==0)

################################## pass and rush epa ################################################################
pass = pbp %>% 
  filter(pass==1, !is.na(epa), !is.na(posteam)) %>% 
  group_by(posteam, season) %>% 
  summarise(avg_pass_epa=mean(epa))
rush = pbp %>% 
  filter(rush==1, !is.na(epa)) %>% 
  group_by(posteam, season) %>% 
  summarise(avg_rush_epa=mean(epa))
################################## pure pass and rush ##########################################################
pure_pass = pbp %>% 
  filter(pass==1, !is.na(epa), !is.na(posteam), xpass>=.7) %>% 
  group_by(posteam, season) %>% 
  summarise(avg_pur_pass_epa=mean(epa))
pure_rush = pbp %>% 
  filter(rush==1, !is.na(epa), xpass<=.3) %>% 
  group_by(posteam, season) %>% 
  summarise(avg_pure_rush_epa=mean(epa))
################################## non pure pass and rush ###################################################
no_pure_pass = pbp %>% 
  filter(pass==1, !is.na(epa), !is.na(posteam), xpass<.7) %>% 
  group_by(posteam, season) %>% 
  summarise(avg_no_pur_pass_epa=mean(epa))
no_pure_rush = pbp %>% 
  filter(rush==1, !is.na(epa), xpass>.3) %>% 
  group_by(posteam, season) %>% 
  summarise(avg_no_pure_rush_epa=mean(epa))
################################# proe ##############################################################################
proe = pbp %>% 
  filter(!is.na(pass_oe), !is.na(epa)) %>% 
  group_by(posteam, season) %>% 
  summarise(proe=mean(pass_oe))
################################# explosive rate ###################################################################
explosive = pbp %>% 
  filter(!is.na(epa), !is.na(posteam),pass==1|rush==1) %>% 
  group_by(posteam, season) %>% 
  mutate(explosive=ifelse((!is.na(rushing_yards) & rushing_yards>10)|(!is.na(passing_yards) & passing_yards>20), 1, 0)) %>% 
  summarise(explosives=sum(explosive), plays=n(), explosive_rate=explosives/plays) %>% 
  select(-explosives, -plays)
################################# turnover rate ###############################################################3###
turnovers = pbp %>% 
  filter(special_teams_play==0, !is.na(epa), !is.na(fumble_lost), !is.na(interception), !is.na(posteam)) %>% 
  group_by(posteam, season ) %>% 
  mutate(turnover=if_else(interception==1|fumble_lost==1, 1, 0)) %>% 
  summarise(turnovers=sum(turnover), plays=n(), turnover_rate=turnovers/plays) %>% 
  select(-turnovers, -plays)
################################# adot ############################################################################
adot = pbp %>% 
  filter(pass==1, !is.na(epa), !is.na(air_yards)) %>%
  group_by(posteam, season) %>% 
  summarise(adot=mean(air_yards)) 
################################## yac epa ###############################################################
yac = pbp %>% 
  filter(complete_pass==1, !is.na(yac_epa)) %>% 
  group_by(posteam, season) %>% 
  summarise(avg_yac_epa=mean(yac_epa))
################################## sack rate #########################################################
sacks = pbp %>% 
  filter(qb_dropback==1, !is.na(epa), !is.na(sack)) %>% 
  group_by(posteam, season) %>%
  summarise(sacks=sum(sack), plays=n(), sack_rate=sacks/plays) %>% 
  select(-sacks, -plays)
################################## pass location % #############################################
pass_epa = pbp %>% 
  filter(pass==1, !is.na(epa), !is.na(pass_location)) %>% 
  group_by(posteam, season) %>% 
  summarise(throws=sum(pass_attempt))
middle_pass_epa = pbp %>% 
  filter(pass==1, !is.na(epa), pass_location=="middle") %>% 
  group_by(posteam, season) %>% 
  summarise(throws_middle=sum(pass_attempt)) %>% 
  left_join(pass_epa, by = c("posteam", "season")) %>% 
  mutate(middle_prop=throws_middle/throws) %>% 
  select(-throws_middle, -throws)
################################# scramble rate ##########################################
scramble = pbp %>% 
  filter(!is.na(epa), qb_dropback==1, !is.na(qb_scramble)) %>% 
  group_by(posteam, season) %>% 
  summarise(scrambles=sum(qb_scramble), plays=n(), scramble_rate=scrambles/plays) %>% 
  select(-scrambles, -plays)
################################# final dataset ##############################################
#combine
data = pass %>% 
  left_join(rush, by = c("posteam", "season")) %>% 
  left_join(pure_pass, by = c("posteam", "season")) %>% 
  left_join(pure_rush, by = c("posteam", "season")) %>% 
  left_join(no_pure_pass, by = c("posteam", "season")) %>% 
  left_join(no_pure_rush, by = c("posteam", "season")) %>% 
  left_join(proe, by = c("posteam", "season")) %>% 
  left_join(explosive, by = c("posteam", "season")) %>% 
  left_join(turnovers, by = c("posteam", "season")) %>% 
  left_join(adot, by = c("posteam", "season")) %>% 
  left_join(yac, by = c("posteam", "season")) %>% 
  left_join(sacks, by = c("posteam", "season")) %>% 
  left_join(middle_pass_epa, by = c("posteam", "season")) %>% 
  left_join(scramble,  by = c("posteam", "season")) 

#check if any NAs
sum(is.na(data))
#corrplot
corrplot(cor(data[,3:16]))
#do we have to remove pass and rush EPA?

#standardize
data_final = data %>% 
  group_by(season) %>% 
  mutate_at(c(3:16), ~(scale(.) %>% as.vector))

write.csv(data_final, 'data_final.csv', row.names = FALSE)
