library(tidyverse)
library(naniar)
library(lubridate)

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

# filter cols
unique(data$Currency)
summary(data$year)
table(data$year)

data <- data %>%
  select(-Currency) %>% # only USD in data
  filter(year >= 2010 & year < 2019) # between 1000 & 2000 companies between 2010 to 2018

# assess missing data
miss_var_summary(data)
# drop rd, cost_of_rev cols since 60%, 10%, & 5% of rows are missing and are not critical to analysis
# drop nulls in rest of all cols
data <- data %>%
  select(-rd, -cost_of_rev, -sga) 

data <- na.omit(data)

# begin analysis
glimpse(data)
levels(data$Sector) # 12
levels(data$Industry) # 71
levels(data$company) # 2025

data$net_income <- round(data$net_income / 1000000, 0) # net income now in millions

other <- data %>%
  filter(Sector == 'Other') # berkshire is only company in 'Other' Sector

ni_sort <- data %>%
  arrange(-net_income)

bi <- data %>%
  filter(Sector == 'Business Services')


# Which companies have grown at an avg rate of 10% or greater in terms of rev over the past 8 years?
# What sector and industries did these organizations come from?



# What is the difference in revenue growth of the top 10% companies in terms of rev compared to the bottom 50%?


# What companies made up the top 10% in terms of net income in 2010 and 2018?
# What are the respective distributions of sectors and industries within these two groups?







