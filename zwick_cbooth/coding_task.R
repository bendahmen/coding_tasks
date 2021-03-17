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

#calculate weighted medians by race and education
wmedians <- lapply(finance_survey[,c("race", "education")],function(vars){
  finance_survey %>%
    group_by((vars), year) %>%
    summarize(w_median = weighted.median(wealth, weight))%>%
    rename(het = "(vars)")%>%
    ungroup()
})

#plot means by race
ggplot(wmedians[[1]], aes(year, w_median, group=het, color=het)) + geom_line() + labs(x="Year", y="Mean Wealth") + scale_color_discrete(name="Race") + scale_y_continuous(labels = comma)
ggsave("median_wealth_by_race.png")
