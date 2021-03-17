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
finance_survey$wealth <- asset_total - debt_total

#function that plots weighted means across education and race for given data
wmedian_plotter <- function(data, outcome) {
  #calculate weighted medians by race and education
  wmedians <- lapply(data[,c("race", "education")], function(vars){
    # enquote variables to be used with dplyr-functions
    outcome <- enquo(outcome)
    vars <- enquo(vars)
    
    median_ts <- data %>%
      group_by(!!vars, year) %>%
      summarize(w_median = weighted.median(!!outcome, weight))%>%
      ungroup()
    colnames(median_ts)[1] <- colnames(vars)
    median_ts
  })
}

#plot means for entire data (Q1)
wealth_medians <- wmedian_plotter(finance_survey, wealth)

#plot means by race
ggplot(wealth_medians[[1]], aes(year, w_median, group=race, color=race)) + geom_line() + labs(x="Year", y="Mean Wealth") + scale_color_discrete(name="Race") + scale_y_continuous(labels = comma)
ggsave("median_wealth_by_race.png")
ggplot(wealth_medians[[2]], aes(year, w_median, group=het, color=het)) + geom_line() + labs(x="Year", y="Mean Wealth") + scale_color_discrete(name="Education") + scale_y_continuous(labels = comma)
ggsave("median_wealth_by_educ.png")

#plot means for Q2 subsample



