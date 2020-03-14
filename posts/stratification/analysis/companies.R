# Author: Brady Engelke 
# Date: 3/15/20

library(tidyverse)
library(naniar)
library(lubridate)
library(ggridges)

# load in data
companies <- read.csv('data/simfin/companies.csv', sep = ';')
industries <- read.csv('data/simfin/industries.csv', sep = ';')
income <- read.csv('data/simfin/income.csv', sep = ';')
# data contains all companies that are not banks or insurances

# merge into one clean file
df <- merge(companies, industries, on = 'IndustryId', all.x = TRUE)
df2 <- merge(df, income, on = 'SimFinId')

# filter cols
data <- df2 %>%
  select(Company.Name:Fiscal.Year, Revenue:Research...Development, Income.Tax..Expense..Benefit..Net, Net.Income) %>%
  rename(company = Company.Name, year = Fiscal.Year, rev = Revenue, cost_of_rev = Cost.of.Revenue, gp = Gross.Profit,
         oe = Operating.Expenses, sga = Selling..General...Administrative, rd = Research...Development, 
         income_tax = Income.Tax..Expense..Benefit..Net, net_income = Net.Income)

## filter cols
# unique(data$Currency)
# summary(data$year)
# table(data$year)

data <- data %>%
  select(-Currency) %>% # only USD in data
  filter(year >= 2010 & year < 2019) # between 1000 & 2000 companies between 2010 to 2018

## assess missing data
# miss_var_summary(data)

# drop rd, cost_of_rev cols since 60%, 10%, & 5% of rows are missing and are not critical to analysis
# drop nulls in rest of all cols
data <- data %>% select(-rd, -cost_of_rev, -sga) 
data <- na.omit(data)

## begin analysis
# glimpse(data)
# levels(data$Sector) # 12
# levels(data$Industry) # 71
# levels(data$company) # 2025

other <- data %>% filter(Sector == 'Other') # berkshire is only company in 'Other' Sector
ni_sort <- data %>% arrange(-net_income)
bi <- data %>% filter(Sector == 'Business Services') # Accenture is only consulting company

## What companies have grown by 25% per year in terms of revenue over the past 8 years? ##
# select companies with data from 2011 - 2018
full_companies <- data %>%
  filter(year != 2010) %>%
  group_by(company) %>%
  summarize(count = n()) %>%
  filter(count == 8)

growth <- data %>%
  filter(company %in% full_companies$company) %>%
  filter(year != 2010)

growth <- growth %>%
  arrange(company, year) %>%
  mutate(yrly_growth = ((rev - lag(rev)) / lag(rev)) * 100) %>%
  filter(year != 2011)

growth <- growth %>%
  group_by(company) %>%
  summarize(avg_yrly_growth = mean(yrly_growth)) %>%
  arrange(-avg_yrly_growth) %>%
  filter(avg_yrly_growth != Inf) %>%
  filter(avg_yrly_growth < 1000)

growth <- growth %>% mutate(top_grower = ifelse(avg_yrly_growth > 25, 1, 0))
growth2 <- merge(growth, data, by = 'company')
growth2$Sector <- as.character(growth2$Sector)
growth2$Industry <- as.character(growth2$Industry)

growth3 <- growth2 %>%
  group_by(company) %>%
  summarize(avg_yearly_growth = max(avg_yrly_growth),
            top_growth_company = max(top_grower),
            sector = max(Sector),
            industry = max(Industry))

growth3$top_growth_company <- as.factor(growth3$top_growth_company)
growth3$sector <- as.factor(growth3$sector)
growth3$industry <- as.factor(growth3$industry)

## What sector and industries did these organizations come from? ##
top <- growth3 %>% filter(top_growth_company == 1)
rest <- growth3 %>% filter(top_growth_company == 0)

sects <- top %>%
  group_by(sector) %>%
  summarize(count = n(),
            growth = median(avg_yearly_growth)) %>%
  filter(count > 2) %>%
  arrange(-count)

industries <- top %>%
  group_by(industry) %>%
  summarize(count = n(),
            growth = median(avg_yearly_growth)) %>%
  filter(count > 2) %>%
  arrange(-count)

# plot the count of each sector and industry in the top growth companies
industries$industry <- as.character(industries$industry)
industries <- industries %>% mutate(Industry = ifelse(industry == 'Oil & Gas - E&P', 'Oil & Gas',
                                    ifelse(industry == 'Retail - Apparel & Specialty', 'Apparel', industry))) %>%
  select(-industry)

sects$sector <- factor(sects$sector, levels = sects$sector[order(sects$count)])
industries$Industry <- factor(industries$Industry , levels = industries$Industry [order(industries$count)])

# ggplot(sects) + geom_col(aes(x = sector, y = count)) + coord_flip()
# ggplot(industries) + geom_col(aes(x = Industry, y = count)) + coord_flip()

ggplot(sects, aes(x = sector, y = count)) + 
  geom_segment(aes(x = sector, xend = sector, y = 0, yend = count), color = 'orangered') + 
  geom_point(color = "orange2", size = 5, alpha = 0.6) +
  coord_flip() + theme_minimal() + 
  labs(title = element_text('Healthcare & Tech Hold Top 2 Spots', size = 14),
       y = '# of Top Earners', x = 'Sector')

ggplot(industries, aes(x = Industry, y = count)) + 
  geom_segment(aes(x = Industry, xend = Industry, y = 0, yend = count), color = 'orangered') + 
  geom_point(color = "orange2", size = 5, alpha = 0.6) +
  coord_flip() + theme_minimal() + 
  labs(title = element_text('App Software & Biotech Hold Top 2 Spots', size = 14),
       y = '# of Top Earners', x = 'Industry')

biotech_software <- growth3 %>%
  filter(industry %in% c('Application Software', 'Biotechnology') & top_growth_company == 1)

## What is the difference in revenue growth distribution of the top companies vs rest? ##
dists <- growth3 %>%
  mutate(growth = round(avg_yearly_growth, 0), 
         type = ifelse(top_growth_company == 1, 'Top Earners', 'Rest of Market')) %>%
  select(avg_yearly_growth, growth, type, company) %>%
  filter(company != 'REMARK HOLDINGS, INC.')

dists$type <- as.factor(dists$type)

67 / 1006 # top_earners / all companies
ggplot(dists, aes(x = growth, y = type, fill = type, color = type)) +
  geom_density_ridges(alpha = 0.6) +
  theme_ridges() + 
  theme(legend.position = "none") +
  labs(title = element_text('6% of Companies Are Growing Rapidly', size = 14), 
       y = element_blank(),
       x = 'Avg Yearly Revenue Growth') +
  scale_color_manual(values=c("lightblue2", "orange2")) + 
  scale_fill_manual(values=c("lightskyblue", "orangered"))

t.test(top$avg_yearly_growth, rest$avg_yearly_growth)

## What companies made up the top 10% in terms of net income in 2012 and 2018? ##
beg_end <- data %>%
  filter(company %in% full_companies$company)

beg_end$net_income <- round(beg_end$net_income / 1000000, 0) # net income now in millions

beg <- beg_end %>%
  filter(year == 2012) %>%
  arrange(-net_income)

# summary(beg$net_income)
top_beg <- beg[1:100, ]

end <- beg_end %>%
  filter(year == 2018) %>%
  arrange(-net_income)

# summary(end$net_income)
top_end <- end[1:100, ]

top_beg_end <- rbind(top_beg, top_end)
top_beg_end$year <- as.factor(top_beg_end$year)

top_beg_end_grouped <- top_beg_end %>%
  group_by(Sector, year) %>%
  summarize(count = n())

## What are the respective distributions of sectors and industries within these two groups? ##
top_beg_end_grouped <- top_beg_end_grouped %>%
  filter(Sector %in% c('Technology', 'Industrials', 'Healthcare', 
                       'Energy', 'Consumer Defensive', 'Consumer Cyclical',
                       'Business Services'))

top_beg_end_grouped <- top_beg_end_grouped %>%
  mutate(prev = lag(count)) %>%
  mutate(dif = count - prev)

difs <- na.omit(top_beg_end_grouped)
difs <- difs %>% arrange(-dif)

top_beg_end_grouped$Sector <- factor(top_beg_end_grouped$Sector, 
    levels = c('Energy', 'Consumer Cyclical', 'Healthcare',
              'Consumer Defensive','Business Services', 'Industrials', 'Technology'))

ggplot(top_beg_end_grouped, aes(x = count, y = Sector)) + 
  geom_line(aes(group = Sector), color = 'orangered') +
  geom_point(aes(shape = year), size = 3, color = 'orange') + theme_minimal() + 
  labs(x = '# of Top Earners', shape = 'Year',
       title = element_text('Tech Companies are Becoming More Efficient', size = 14))
