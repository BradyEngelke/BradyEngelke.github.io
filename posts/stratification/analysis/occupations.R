# Author: Brady Engelke 
# Date: 3/15/20

library(tidyverse)
library(naniar)
library(lubridate)
library(readxl)

occupations <- read_excel('data/OES2018.xlsx')

# clean data
occupations <- occupations %>%
  select(area_title, area_type,	naics_title,	occ_title,	tot_emp,	loc_quotient,	
         a_mean,	a_pct10,	a_pct25,	a_median,	a_pct75,	a_pct90)

occupations$a_pct90 <- na_if(occupations$a_pct90, '#')
occupations$a_pct75 <- na_if(occupations$a_pct75, '#')
occupations$a_median <- na_if(occupations$a_median, '#')
occupations$a_pct25 <- na_if(occupations$a_pct25, '#')
occupations$a_pct10 <- na_if(occupations$a_pct10, '#')
occupations$a_mean <- na_if(occupations$a_mean, '#')
occupations$a_pct90 <- na_if(occupations$a_pct90, '*')
occupations$a_pct75 <- na_if(occupations$a_pct75, '*')
occupations$a_median <- na_if(occupations$a_median, '*')
occupations$a_pct25 <- na_if(occupations$a_pct25, '*')
occupations$a_pct10 <- na_if(occupations$a_pct10, '*')
occupations$a_mean <- na_if(occupations$a_mean, '*')

# filter down to subset of interest
us <- occupations %>% filter(naics_title == 'Cross-industry' & area_title == 'U.S.')
# only want rows that apply across the market as whole instead of focusing on one specific industry
us <- us %>% select(occ_title, tot_emp, a_mean:a_pct90)
us <- us %>% arrange(-tot_emp)
us <- us[duplicated(us$occ_title) == FALSE,]
all_emps <- us[1,]
us <- us[2:1102,]

us$occ_title <- as.factor(us$occ_title)
us$tot_emp <- as.integer(us$tot_emp)
us$a_mean <- as.integer(us$a_mean)
us$a_median <- as.integer(us$a_median)
us$a_pct10 <- as.integer(us$a_pct10)
us$a_pct25 <- as.integer(us$a_pct25)
us$a_pct75 <- as.integer(us$a_pct75)
us$a_pct90 <- as.integer(us$a_pct90)

# miss_var_summary(us)
# glimpse(us)

agg_us <- us[grep('Occupations', us$occ_title), ]
ind_us <- subset(us, !(us$occ_title %in% agg_us$occ_title))
clust_us <- ind_us %>% select(occ_title, tot_emp, a_pct10:a_pct75)
clust_us <- na.omit(clust_us)

## check out distributions
# ggplot(agg_us) + geom_density(aes(x = a_mean))

agg_us$a_median1 <- agg_us$a_median / 1000

ggplot(agg_us) + geom_density(aes(x = a_median1), color = 'darkgreen', fill = 'lightgreen') + 
  geom_vline(aes(xintercept = 70), color = 'orangered') +
  labs(title = element_text('Median Income of Occupations in US is Bimodally Distributed', size = 14),
       x = 'Median Income (Ks)', y = 'Occupation Density') + theme_minimal() +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank()) 

# ggplot(ind_us) + geom_density(aes(x = a_mean))
# ggplot(ind_us) + geom_density(aes(x = a_median))

# assign categories
agg_us <- agg_us %>% mutate(top_earners = ifelse(a_median > 70000, 1, 0)) 
# cutoff chosen based on agg_us median distribution
agg_us$top_earners <- as.factor(agg_us$top_earners)

ind_us <- ind_us %>%
  mutate(top_10 = ifelse(all_emps$a_pct90 <= a_mean, 1, 0),
         top_25 = ifelse(all_emps$a_pct75 <= a_mean, 1, 0),
         bottom_10 = ifelse(all_emps$a_pct10 >= a_mean, 1, 0),
         bottom_25 = ifelse(all_emps$a_pct25 >= a_mean, 1, 0))

ind_us$top_10 <- as.factor(ind_us$top_10)
ind_us$top_25 <- as.factor(ind_us$top_25)
ind_us$bottom_10 <- as.factor(ind_us$bottom_10)
ind_us$bottom_25 <- as.factor(ind_us$bottom_25)

# table(agg_us$top_earners)

# create tags for plot
agg_us <- agg_us %>% mutate(occ = ifelse(occ_title == 'Computer and Mathematical Occupations', 'Computing',
    ifelse(occ_title == 'Computer Occupations', 'Computing',
    ifelse(occ_title == 'Management Occupations', 'Management',
    ifelse(occ_title == 'Architecture and Engineering Occupations', 'Engineering',
    ifelse(occ_title == 'Other Management Occupations', 'Management',
    ifelse(occ_title == 'Legal Occupations', 'Legal',
    ifelse(occ_title == 'Miscellaneous Computer Occupations', 'Computing',
    ifelse(occ_title == 'Computer Occupations, All Other', 'Computing',
    ifelse(occ_title == 'Mathematical Science Occupations', 'Computing', 
    ifelse(occ_title == 'Office and Administrative Support Occupations', 'Admin Support',
    ifelse(occ_title == 'Transportation and Material Moving Occupations', 'Trucking',
    ifelse(occ_title == 'Production Occupations', 'Manufacturing',
    ifelse(occ_title == 'Other Production Occupations', 'Manufacturing',
    ifelse(occ_title == 'Education, Training, and Library Occupations', 'Education',
    ifelse(occ_title == 'Other Education, Training, and Library Occupations', 'Education',
    ifelse(occ_title == 'Healthcare Practitioners and Technical Occupations', 'Healthcare Practitioners',
    ifelse(occ_title == 'Other Healthcare Practitioners and Technical Occupations', 'Healthcare Practitioners',
    ifelse(occ_title == 'Construction and Extraction Occupations', 'Construction',
    ifelse(occ_title == 'Installation, Maintenance, and Repair Occupations', 'Construction',
    ifelse(occ_title == 'Other Installation, Maintenance, and Repair Occupations', 'Construction',
    ifelse(occ_title == 'Healthcare Support Occupations', 'Healthcare Support',
    ifelse(occ_title == 'Other Healthcare Support Occupations', 'Healthcare Support',
    ifelse(occ_title == 'Miscellaneous Healthcare Support Occupations', 'Healthcare Support',
    ifelse(occ_title == 'Food Preparation and Serving Related Occupations', 'Food & Beverage',
    ifelse(occ_title == 'Sales and Related Occupations', 'Sales','not include'))))))))))))))))))))))))))

agg_us$occ <- as.factor(agg_us$occ)
agg_us$top_earners <- as.character(agg_us$top_earners)
viz_occ <- agg_us %>%
  filter(occ != 'not include') %>%
  group_by(occ) %>%
  summarize(median_income = mean(a_median),
            mean_income = mean(a_mean),
            tot_emp = sum(tot_emp),
            perc_90 = mean(a_pct90),
            perc_75 = mean(a_pct75),
            perc_25 = mean(a_pct25),
            perc_10 = mean(a_pct10),
            top_occ = max(top_earners)
            )

viz_occ$top_occ <- as.factor(viz_occ$top_occ)
viz_occ$median_income <- viz_occ$median_income / 1000
viz_occ$mean_income <- viz_occ$mean_income / 1000
viz_occ$perc_10 <- viz_occ$perc_10 / 1000
viz_occ$perc_25 <- viz_occ$perc_25 / 1000
viz_occ$perc_75 <- viz_occ$perc_75 / 1000
viz_occ$perc_90 <- viz_occ$perc_90 / 1000
viz_occ$tot_emp <- viz_occ$tot_emp / 1000000

top_occupations <- viz_occ %>% filter(top_occ == 1) %>% select(occ)
rest_occupations <- viz_occ %>% filter(top_occ == 0) %>% select(occ)

viz_occ <- viz_occ %>% # note in article I dropped Healthcare Practitioners
  filter(occ != 'Healthcare Support' & occ != 'Legal' & 
         occ != 'Manufacturing' & occ != 'Healthcare Practitioners') 

viz_occ$top_occ <- ifelse(viz_occ$top_occ == 1, 'Top Earners', 'Rest of Market')

ggplot(viz_occ) + geom_point(aes(x = top_occ, y = median_income, size = tot_emp, color = top_occ), ) +
  geom_text(aes(x = top_occ, y = median_income, label = occ), hjust = -0.2, vjust = -0.2) + 
  labs(title = element_text('Massive Income Gap Between Top Earners & Rest', size = 14),
       y = 'Median Income (Ks)', size = '# of Ppl (Ms)', color = element_blank()) + 
  scale_x_discrete(name = '', labels=c("","")) + theme_minimal() +
  scale_color_manual(values=c("lightskyblue", "orangered"))

income_emp_totals <- viz_occ %>% # 21% of the ppl in this subset make 60% of the money
  mutate(profession_income = mean_income * tot_emp) %>%
  group_by(top_occ) %>%
  summarize(employees = sum(tot_emp),
            income = sum(profession_income))

viz_occ2 <- viz_occ %>%
  gather(key = 'income_type', value = 'amount', median_income, perc_25, perc_75, -occ) %>%
  select(occ, top_occ, income_type:amount)

viz_occ2$income_type <- ifelse(viz_occ2$income_type == 'median_income', 50,
                       ifelse(viz_occ2$income_type == 'perc_25', 25, 75))

viz_occ2$income_type <- as.factor(viz_occ2$income_type)
viz_occ2$occ <- factor(viz_occ2$occ, levels = c('Food & Beverage', 'Admin Support','Trucking',
                                                'Sales', 'Education', 'Construction',
                                                'Engineering', 'Computing', 'Management'))

viz_occ2$top_occ <- as.factor(viz_occ2$top_occ)
ggplot(viz_occ2, aes(x = amount, y = occ)) + 
  geom_line(aes(group = occ, color = top_occ)) +
  geom_point(aes(shape = income_type, color = top_occ), size = 3) + theme_minimal() + 
  labs(title = 'Income Growth Opportunities Drop After Engineering', 
       x = 'Median Income (Ks)', y = 'Profession', 
       shape = 'Income Percentile', color = '') +
  theme(legend.position = 'top') +
  scale_color_manual(values=c("lightskyblue", "orangered")) 

# clustering
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}

clust_data <- clust_us %>%
  select(tot_emp:a_pct75) %>%
  transmute(tot_emp = normalize(tot_emp),
            a_pct10 = normalize(a_pct10),
            a_pct25 = normalize(a_pct25),
            a_median = normalize(a_median),
            a_pct75 = normalize(a_pct75))

SSE_curve <- c()
for (n in 1:10) {
  kcluster <- kmeans(clust_data, n)
  sse <- sum(kcluster$withinss)
  SSE_curve[n] <- sse
}
plot(1:10, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")


k = 3
kcluster <- kmeans(clust_data, k, nstart = 20)

clustered_professions <- clust_us %>% mutate(Cluster = kcluster$cluster)

clustered_means <- clustered_professions %>%
  select(Cluster, tot_emp:a_pct75) %>%
  group_by(Cluster) %>%
  summarise_all("mean") %>%
  mutate(cluster_size = kcluster$size) 

clustered_means2 <- clustered_means %>%
  gather(key = 'income_type', value = 'amount', a_median, a_pct25, a_pct75) %>%
  select(-a_pct10)

clustered_means2$income_type <- ifelse(clustered_means2$income_type == 'a_median', 50,
                               ifelse(clustered_means2$income_type == 'a_pct25', 25, 75))

clustered_means2$income_type <- as.factor(clustered_means2$income_type)
clustered_means2$Cluster <- as.factor(clustered_means2$Cluster)

# glimpse(clustered_means2)
clustered_means2$amount <- clustered_means2$amount / 1000
ggplot(clustered_means2, aes(x = amount, y = Cluster)) + 
  geom_line(aes(group = Cluster), color = 'turquoise3') +
  geom_point(aes(shape = income_type), size = 3, color = 'royalblue2') + theme_minimal() + 
  labs(title = 'Cluster 1 is Way Behind Cluster 3', 
       x = 'Median Income (Ks)', y = 'Cluster', shape = 'Income Percentile') +
  theme(legend.position = 'top') 

clustered_means2 <- clustered_means2 %>% mutate(num_people = tot_emp * cluster_size)

clustered_means3 <- clustered_means2[1:3, ]
clustered_means3 <- clustered_means3 %>% select(Cluster, amount:num_people)

clustered_means3$num_people <- clustered_means3$num_people / 1000000
ggplot(clustered_means3, aes(x = Cluster, y = num_people)) + 
  geom_segment(aes(x = Cluster, xend = Cluster, y = 0, yend = num_people), color = 'turquoise3') + 
  geom_point(color = "royalblue2", size = 5, alpha = 0.6) +
  coord_flip() + theme_minimal() + 
  labs(title = 'A Large % of The Population is Poor', y = '# of People (Ms)', x = 'Cluster')

clust1 <- clustered_professions %>% filter(Cluster == 1) %>% select(occ_title, a_pct25:a_pct75) %>%
  arrange(-a_median)
clust2 <- clustered_professions %>% filter(Cluster == 2) %>% select(occ_title, a_pct25:a_pct75) %>%
  arrange(-a_median)
clust3 <- clustered_professions %>% filter(Cluster == 3) %>% select(occ_title, a_pct25:a_pct75) %>%
  arrange(-a_median)

clust1_bot <- clust1[507:557,]
clust3_top <- clust3[1:50,]

clustered_professions2 <- clustered_professions %>% select(occ_title, Cluster)
clustered_professions3 <- clustered_professions2 %>% 
  mutate(well_off_metric = ifelse(Cluster == 3, 1, 
                           ifelse(Cluster == 2, 0 , -1))) %>%
  select(-Cluster)

# write.csv(clustered_professions3, '/Users/bradyengelke/GitHub/articles/stratification/data/clustered_professions.csv')
# write.csv(clust1_bot, '/Users/bradyengelke/GitHub/articles/stratification/data/bottom_professions.csv')
# write.csv(clust3_top, '/Users/bradyengelke/GitHub/articles/stratification/data/top_professions.csv')
