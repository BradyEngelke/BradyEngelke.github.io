# Author: Brady Engelke 
# Date: 3/15/20

library(tidyverse)
library(naniar)
library(lubridate)
library(readxl)
library(usmap)

states <- read_excel('data/OES2018.xlsx')
clusters <- read.csv('data/results/clustered_professions.csv')

# clean data
states <- states %>% filter(naics_title == 'Cross-industry')
states$area <- as.integer(states$area)
states <- states %>% filter(area %in% c(1:56))
states <- states %>% filter(area_title != 'District of Columbia')

states <- states %>% 
  select(area_title, occ_title, tot_emp, a_mean, a_median, a_pct10,	a_pct25, a_pct75,	a_pct90)

states$a_pct90 <- na_if(states$a_pct90, '#')
states$a_pct75 <- na_if(states$a_pct75, '#')
states$a_median <- na_if(states$a_median, '#')
states$a_pct25 <- na_if(states$a_pct25, '#')
states$a_pct10 <- na_if(states$a_pct10, '#')
states$a_mean <- na_if(states$a_mean, '#')
states$a_pct90 <- na_if(states$a_pct90, '*')
states$a_pct75 <- na_if(states$a_pct75, '*')
states$a_median <- na_if(states$a_median, '*')
states$a_pct25 <- na_if(states$a_pct25, '*')
states$a_pct10 <- na_if(states$a_pct10, '*')
states$a_mean <- na_if(states$a_mean, '*')

agg_states <- states[grep('Occupations', states$occ_title), ]
ind_states <- subset(states, !(states$occ_title %in% agg_states$occ_title))
ind_states <- ind_states %>% arrange(occ_title)
clusters <- clusters %>% select(occ_title, well_off_metric) %>% arrange(occ_title)

clust_states <- merge(ind_states, clusters, by = 'occ_title')
clust_states <- clust_states %>% select(area_title, well_off_metric, occ_title, tot_emp)
clust_states$tot_emp <- replace_na(clust_states$tot_emp, 0)
clust_states <- clust_states %>% 
  mutate(total_wellness = tot_emp * well_off_metric)

clust_states2 <- clust_states %>%
  group_by(area_title) %>%
  summarize(financial_wellness = sum(total_wellness),
            people = sum(tot_emp))

clust_states2 <- clust_states2 %>%rename(state = area_title)

clust_states2 <- clust_states2 %>%
  mutate(wellness_metric = financial_wellness / people)
clust_states2$wellness_metric <- clust_states2$wellness_metric * 100

clust_states2 <- clust_states2 %>% arrange(-wellness_metric)

# plot the heatmap of the US
plot_usmap(data = clust_states2, values = "financial_wellness", color = "orange") + 
  scale_fill_continuous(
    low = "red", high = "green", name = "Automation Risk", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = clust_states2, values = "wellness_metric", color = "orange") + 
  scale_fill_continuous(
    low = "red", high = "green", name = "Automation Risk", label = scales::comma
  ) + theme(legend.position = "right") 

# find top & bottom 10 states
top_10 <- clust_states2[1:10,]
bottom_10 <- clust_states2[40:50,]

# write.csv(top_10, '/Users/bradyengelke/GitHub/articles/stratification/data/top_states.csv')
# write.csv(bottom_10, '/Users/bradyengelke/GitHub/articles/stratification/data/bottom_states.csv')
