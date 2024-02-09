library(cluster)
library(ggfortify)
library(tidyverse)
library(factoextra)
library(plotly)
library(data.table)
library(knitr)
options(bitmapType='cairo')
#load csv
#data_final = read_csv('data_final.csv')
row.names(data_final)=paste0(data_final$posteam, data_final$season)

#remove names
data_kmeans=data_final[, c(3,4, 9, 10)]
row.names(data_kmeans) = paste0(data_final$posteam, data_final$season)

#determine number of clusters
#wss elbow method
set.seed(121)
fviz_nbclust(data_kmeans, kmeans, method = "wss")
#4?
#silhoutte method
fviz_nbclust(data_kmeans, kmeans, method = "silhouette")
k4 <- kmeans(data_kmeans, centers = 5, nstart = 25)
str(k4)
k4
fviz_cluster(k4, data = data_kmeans, geom = 'point',   ggtheme = theme_minimal())
ggsave('pca.png')
#add to data
offenses = data_kmeans %>% 
  mutate(team=paste0(data_final$posteam, data_final$season),Cluster = k4$cluster)
#how many per cluster
offenses %>% 
  group_by(Cluster) %>% 
  summarise(teams=n())
offenses %>% 
  group_by(Cluster) %>% 
  summarise(pass_epa=mean(avg_pass_epa), rush_epa=mean(avg_rush_epa), explosive=mean(explosive_rate), proe_=mean(proe))
offenses %>% 
  filter(substr(team, 1, 3)=="NYJ") %>% 
  group_by(Cluster) %>% 
  summarise(teams=n())
#plot rush/pass epa by cluster
p=data_final %>%
  as_tibble() %>%
  mutate(cluster = k4$cluster,
         team = paste0(data_final$posteam, data_final$season)) %>%
  ggplot(aes(avg_pass_epa, avg_rush_epa, color = factor(cluster), text=team)) +
  geom_point() 
ggplotly(p, tooltip = 'text') 
p
#plot rush/pass epa by cluster
p_1=data_final %>%
  as_tibble() %>%
  mutate(cluster = k4$cluster,
         team = paste0(data_final$posteam, data_final$season)) %>%
  ggplot(aes(adot, middle_prop, color = factor(cluster), text=team)) +
  geom_point() 
ggplotly(p_1, tooltip = 'text') 
#pca
#add season result/wins
#fantasy impact?

#since 2016, every SB team has been a 2
#4 - you are bad
#3 - good, pass heavy offense (SB team)
#5 - ok, pass heavy offense, not explosive
#2 - rushing team
#1 - very explosive, good at pass/rush but better at rush, likely schematic wonder


plot_ly(x = data_kmeans$avg_pass_epa.x, 
        y = data_kmeans$avg_rush_epa.x, 
        z = data_kmeans$proe, 
        type = "scatter3d", 
        mode = "markers", 
        color = as.factor(k4$cluster)) %>%
  layout(title = "",
         scene = list(xaxis = list(title = "Pass EPA"),
                      yaxis = list(title = "Rush EPA"),
                      zaxis = list(title = "PROE")))
