library(nflreadr)
library(nflverse)
schedule=load_schedules()
#fix offense data
offenses$posteam = data_final$posteam
offenses$season = data_final$season
#add short names to clusters
offenses = offenses %>%
  mutate(cluster_desc= case_match(Cluster,
                                  1~"Schematic Wonder",
                                  2~"Rush Centric",
                                  3~"Elite Passing",
                                  4~"Bad",
                                  5~"Meh Passing"))
#super bowl winner
season = unique(data_final$season)
champion = c("IND", "NYG", "PIT", "NO", "GB", "NYG", "BAL", "SEA", "NE", "DEN", "NE", "PHI", "NE", "KC", "TB", "LA", "KC", "")
sb_champs = data.frame(season, champion)
#add to data
offenses = offenses %>% 
  left_join(sb_champs, by = "season")
#make champs variable
offenses = offenses %>% 
  mutate(champs = if_else(team==paste0(champion, season), 1, 0))
#graph
offenses %>% 
  filter(champs==1) %>% 
  ggplot(aes(x=season, y=cluster_desc)) +
  geom_nfl_logos(aes(team_abbr = posteam), width = .07) +
  theme_classic() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 17)) +
  labs(x='Season', title = 'SB Champion Offensive Clusters')
#add unstandardized data
offenses = offenses %>% 
  left_join(data, by = c("posteam", "season"))

# clean variable names, include only important variables
offenses = offenses %>% 
  rename("Standardized Pass EPA per Play"=1,
         "Standardized Rush EPA per Play"=2,
         "Standardized Pass Rate Over Expected"=3,
         "Standardized Explosive Rate"=4,
         "Cluster_Description"="cluster_desc",
         "Pass EPA per Play"=12,
         "Rush EPA per Play"=13,
         "Pass EPA in Pure Passing Situations"=14,
         "Rush EPA in Pure Rushing Siutations"=15,
         "Pass Rate Over Expected"=18,
         "Explosive Rate"=19,
         "Turnover Rate"=20,
         "ADOT"=21,
         "YAC EPA per Catch"=22,
         "Sack Rate"=23,
         "% of Throws to Middle of Field"=24,
         "Scramble Rate"=25)
