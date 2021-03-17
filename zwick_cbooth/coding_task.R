#Coding Task for Prof. Zwick @ ChicagoBooth
#Author: Benjamin Dahmen | bendahmen@gmail.com
#Date: 16/03/21

#clean global environment
rm(list=ls())

#set WD in console

# Install and load packages
installation_needed  <- F
loading_needed       <- TRUE
package_list         <- c("tidyverse", "spatstat", "AER")
if(installation_needed){install.packages(package_list, repos='http://cran.us.r-project.org')}
if(loading_needed){lapply(package_list, require, character.only = TRUE)}

#clean global env
rm(list=ls())

#load data 
finance_survey <- read_csv("RA_21_22.csv")
attach(finance_survey)

#turn categorical vars into factors
factor_vars <- c("year", "sex", "education", "race")
finance_survey[,factor_vars] <- lapply(finance_survey[,factor_vars], as.factor)

#calculate wealth
finance_survey <- finance_survey %>%
  mutate(wealth = (asset_total - debt_total), hwealth = (asset_housing - debt_housing))

#function that plots weighted means across education and race for given data
wmedian_calc <- function(data, outcome, heterogeneity) {
  #calculate weighted medians by race and education
  wmedians <- lapply(data[,heterogeneity], function(vars){
    # enquote variables to be used with dplyr-functions
    outcome <- enquo(outcome)
    vars <- enquo(vars)
    
    data %>%
      group_by(!!vars, year) %>%
      summarize(w_median = weighted.median(!!outcome, weight))%>%
      ungroup() %>%
      rename(het = "<fct>")
  })
}

#get wealth means for entire data (Q1)
wealth_medians <- wmedian_calc(finance_survey, wealth, c("race", "education"))

#plot means by race and education
ggplot(wealth_medians[[1]], aes(year, w_median, group=het, color=het)) + geom_line() + labs(x="Year", y="Mean Wealth") + scale_color_discrete(name="Race") + scale_y_continuous(labels = comma)
ggsave("median_wealth_by_race.png")
ggplot(wealth_medians[[2]], aes(year, w_median, group=het, color=het)) + geom_line() + labs(x="Year", y="Mean Wealth") + scale_color_discrete(name="Education") + scale_y_continuous(labels = comma)
ggsave("median_wealth_by_educ.png")

#create subsample for Q2 - black and white individuals only
finance_survey_bw <- finance_survey %>%
  filter(race=="black"|race=="white")

#get housing wealth means for Q2
hwealth_medians <- wmedian_calc(finance_survey_bw, hwealth, c("race"))

#plot hwealth by race
ggplot(hwealth_medians[[1]], aes(year, w_median, group=het, color=het)) + geom_line() + labs(x="Year", y="Mean Housing Wealth") + scale_color_discrete(name="Race") + scale_y_continuous(labels = comma)
ggsave("median_hwealth_by_race_bw.png")

#create subsample for Q3 - black and white individuals aged 25 or older only
finance_survey_bw_25 <- finance_survey_bw %>%
  filter(age>=25)

#get wealth and housing wealth means by race
wealth_25_medians <- wmedian_calc(finance_survey_bw_25, wealth, c("race"))
hwealth_25_medians <- wmedian_calc(finance_survey_bw_25, hwealth, c("race"))

#plot wealth and housing wealth by race
ggplot(wealth_25_medians[[1]], aes(year, w_median, group=het, color=het)) + geom_line() + labs(x="Year", y="Mean Wealth") + scale_color_discrete(name="Race") + scale_y_continuous(labels = comma)
ggsave("median_wealth_by_race_bw_25.png")

ggplot(hwealth_25_medians[[1]], aes(year, w_median, group=het, color=het)) + geom_boxplot() + labs(x="Year", y="Mean Housing Wealth") + scale_color_discrete(name="Race") + scale_y_continuous(labels = comma)
ggsave("median_hwealth_by_race_bw_25.png")

