Yonathan Melamed’s NFL Offensive Clusters
================

## NFL Offenses K Means Clustering

In this short summary, I’ll be writing about what my goals were during
this research, my methodology for arriving at the NFL offensive
clusters, and some takeaways. The app can be found
[here](https://ymelamed.shinyapps.io/CLUSTERS/).

## Goals

After recently viewing a simple clustering analysis on historical NBA
teams, I got inspired to a do a similar analysis on NFL teams. My idea
was to find some clustering method that would allow me to compare teams
from different eras. A couple weeks ago, my Ravens yet again suffered an
unexpected, ugly loss in the playoffs. This gave me a second motivation:
after grouping teams by different clusters, we could look to see if
certain types of offenses were more succesful than others, particularly
in the playoffs.

## Methodology and Results

I utilized data from nflfastR’s play by play dataset, which has been an
awesome resource. I used regular season data only, and included all
seasons 2006-2023, since some of the data is only available starting in
2006. In total, I calculated the following variables, grouping each by
offensive team and season: Pass EPA/Play; Rush EPA/Play: Pass EPA/Play
in pure pass situations (situations where, based on game factors such as
down, distance, time, and score, the expected passing rate would be
greater than 70%); Rush EPA/Play in pure pass situations (situations
where the expected rushing rate would be greater than 70%); Pass rate
over expected (‘PROE’); Explosive play percentage (the rate at which
offensive plays gain at least either 20 passing yards or 10 rushing
yards); Turnover rate; Average depth of target; YAC EPA per catch (on
receptions only); Sack percentage; Percentage of a team’s passing EPA
that is gained through targets to the middle of the field; and scramble
rate (percentage of dropbacks in which the QB rushes). An example code
for explosive play percentage can be found below:

``` r
library(nflreadr)
library(tidyverse)
#load data 2004 to 2023
pbp=load_pbp(seasons = 2006:2023)
#filter to only regular season and no aborted plays
pbp = pbp %>% 
  filter(season_type=="REG", aborted_play==0)
#create df of explosive rate
explosive = pbp %>% 
  filter(!is.na(epa), !is.na(posteam),pass==1|rush==1) %>% 
  group_by(posteam, season) %>% 
  mutate(explosive=ifelse((!is.na(rushing_yards) & rushing_yards>10)|(!is.na(passing_yards) & passing_yards>20), 1, 0)) %>% 
  summarise(explosives=sum(explosive), plays=n(), explosive_rate=explosives/plays) %>% 
  select(-explosives, -plays)
```

Once I created these datasets, I joined them together using leftjoins
and checked for any issues with the data, confirming there were no NAs.
Then, I standardized the data using R’s scale() function. I decided to
standardize each column by each season separately. Doing so allows us to
compare offenses from different eras. For example, The 2023 49ers and
the 2006 Colts are the two best passing offenses when looking at
historical Pass EPA/Play. While the 49ers offense was slightly more
efficient, passing offenses in general are more efficient in the 2023
season than they were in 2006. Therefore, once we normalize the data by
season, we actually get 2006 Colts as the best offense since 2006. It’s
important to remember this is taken in context of the other offenses
during that season.

Next, I moved on to the clustering. After experimenting with different
variables, I decided on including only pass EPA/play, rush EPA/play,
explosive rate, and pass rate over expected (all standardized). I found
that if I included too many variables, the clustering algorithm wasn’t
really showing me much. We could have 8 different clusters showing
different combinations of all the variables, but if we can’t explain why
it’s done and what the clusters are even showing, we’re not doing it
right. I used both the silhoutte method and elbow method to arrive at 5
centers for the k means model, and I found my results to be easily
interpretable. Here, I plot a PCA analysis of the clusters.

<div class="figure">

<img src="Index_files/figure-gfm/pca-1.svg" alt="PCA Analysis" width="100%" />
<p class="caption">
PCA Analysis
</p>

</div>

Once I got my results, I wanted to understand what type of offense each
cluster represented. I found the clusters to be a good portrayal of the
different types of offenses. Here, I present the mean of each
standardized variable by cluster.

| Cluster |   pass_epa |   rush_epa |  explosive |       proe |
|--------:|-----------:|-----------:|-----------:|-----------:|
|       1 |  0.2401789 | -0.7260380 | -0.4814308 |  0.7566518 |
|       2 | -1.1842154 | -0.8765838 | -1.0451861 | -0.0845266 |
|       3 | -0.3738490 |  0.2185049 |  0.0597416 | -0.7311276 |
|       4 |  1.1604965 |  0.6302107 |  0.5676024 |  0.9243529 |
|       5 |  0.5665484 |  1.1898825 |  1.5089295 | -0.9469993 |

Starting with Cluster 2, this cluster shows offenses that we can simply
describe as ‘Bad’. They tend to have lower than average pass and rush
EPA and explosiveness (think Jets). Cluster 3 represents offenses that
tend to excel at rushing, and do so a lot. I call them ‘Rush Centric’
offenses. This year, those offenses include the Browns, Cardinals,
Titans, and others. I tend to struggle with this cluster the most, since
at times it can combine good rushing teams (with little passing
prowress) and teams that just run the ball a lot, but aren’t bad enough
to be under the Cluster 2 umbrella. Next, Cluster 1 represents teams
that tend to have OK passing offenses, but pass the ball a lot. I call
them ‘Meh Offenses’ and you can think of them as teams like the 2023
Jaguars and the 2023 regular season Chiefs (very heavy emphasis on
regular season). Cluster 4 represents Elite Passing attacks. They throw
a whole lot and are pretty good at it (think every other Mahomes
offense). I’ll show later how this cluster has had the most success in
terms of Super Bowl wins. Finally, my favorite, Cluster 5, which I term
“Schematic Wonder”. These attacks excel at both rushing and passing,
though they tend to be a bit better at rushing (meaning they are very,
very good at rushing) and do so a lot. They are also incredibly
explosive. I call them Schematic Wonders both because of my intuition,
that is that in order to be good in all phases, especially rushing, you
have to have some good scheme attached AND because of the playcallers in
this cluster. Consider the 2023 Schematic Wonders: the 49ers, Dolphins,
and Lions, who have arguably the 3 brightest offensive minds in the
league. Kyle Shanahan teams are frequent guests of this cluster; in
fact, since 2019, the 49ers offense has been in this cluster every year
but one. It’s important to stress that not many teams make this cluster,
and that in general offenses aren’t exactly perfectly balanced to each
cluster, although not too unbalanced to worry about the clustering
itself.

| Cluster | Team |
|--------:|-----:|
|       1 |  119 |
|       2 |  117 |
|       3 |  158 |
|       4 |  111 |
|       5 |   71 |

## Takeaways

Using my app, you can compare every variable I collected (including
those not in the actual model and the 4 standardized variables) for
every offense from 2006 to 2023. Additionally, I circled every Super
Bowl winner. As you can see, most of the Super Bowl winners were Elite
Passing offenses. This should come as no surprise, as most of us know by
now that the saying “Defense Wins Championships” is a myth, and that the
best offenses are those that can sling it.

<img src="Index_files/figure-gfm/SB_champs-1.svg" width="100%" />

In fact, since 2016, every Super Bowl winner has had an Elite Passing
offense! It’s beautiful when stats agree with your priors. Nonetheless,
this year that record will break, as we have the Meh Passing offense
Chiefs and the Schematic Wonder 49ers, although the Chiefs obviously
come with an asterix. Finally, to revisit my question about the Ravens’
offense sustainability in the playoffs, we see that the 2023 Ravens were
an Elite Passing offense (although they were close to a Schematic Wonder
with their rushing success, they weren’t quite as explosive as MIA and
SF). I’ll leave you with a list of the 2023 offenses by cluster, thanks
for reading and make sure to check out the
[app](https://ymelamed.shinyapps.io/CLUSTERS/)!

| Team    | Cluster          |
|:--------|:-----------------|
| ARI2023 | Rush Centric     |
| ATL2023 | Rush Centric     |
| BAL2023 | Elite Passing    |
| BUF2023 | Elite Passing    |
| CAR2023 | Bad              |
| CHI2023 | Rush Centric     |
| CIN2023 | Meh Passing      |
| CLE2023 | Rush Centric     |
| DAL2023 | Elite Passing    |
| DEN2023 | Rush Centric     |
| DET2023 | Schematic Wonder |
| GB2023  | Elite Passing    |
| HOU2023 | Rush Centric     |
| IND2023 | Rush Centric     |
| JAX2023 | Meh Passing      |
| KC2023  | Meh Passing      |
| LA2023  | Rush Centric     |
| LAC2023 | Meh Passing      |
| LV2023  | Bad              |
| MIA2023 | Schematic Wonder |
| MIN2023 | Meh Passing      |
| NE2023  | Bad              |
| NO2023  | Rush Centric     |
| NYG2023 | Bad              |
| NYJ2023 | Bad              |
| PHI2023 | Elite Passing    |
| PIT2023 | Rush Centric     |
| SEA2023 | Elite Passing    |
| SF2023  | Schematic Wonder |
| TB2023  | Meh Passing      |
| TEN2023 | Rush Centric     |
| WAS2023 | Meh Passing      |
