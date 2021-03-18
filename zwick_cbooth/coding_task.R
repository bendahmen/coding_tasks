#Coding Task for Prof. Zwick @ ChicagoBooth
#Author: Benjamin Dahmen | bendahmen@gmail.com
#Date: 16/03/21

#clean global environment
rm(list=ls())

#set WD in console

# Install and load packages
installation_needed  <- F
loading_needed       <- T
package_list         <- c("tidyverse", "spatstat", "AER", "scales")
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
  #deparse variables to use with ggplot
  outcome_text <- deparse(substitute(outcome))
  data_text <- deparse(substitute(data))
  #calculate weighted medians by race and education
  wmedians <- lapply(heterogeneity, function(vars){
    #enquote variables to be used with dplyr-functions
    en_outcome <- enquo(outcome)
    
    stats <- data %>%
      group_by(.data[[vars]], year) %>%
      summarize(w_median = weighted.median(!!en_outcome, weight))%>%
      ungroup()
    
    #plot medians
    ggplot(stats, aes(year, w_median, group=.data[[vars]], color=.data[[vars]])) + geom_line() + labs(x="Year", y=paste("median ", outcome_text)) + scale_y_continuous(labels = comma)
    ggsave(paste("median",outcome_text,data_text,"_by",vars,".png"))
    stats
  })
}

#get wealth means for entire data (Q1)
wealth_medians <- wmedian_calc(finance_survey, wealth, c("race", "education"))

#create subsample for Q2 - black and white individuals only
finance_survey_bw <- finance_survey %>%
  filter(race=="black"|race=="white")

#get housing wealth medians for Q2
hwealth_medians <- wmedian_calc(finance_survey_bw, hwealth, c("race"))

#create subsample for Q3 - black and white individuals aged 25 or older only
finance_survey_bw_25 <- finance_survey_bw %>%
  filter(age>=25)

#get wealth and housing wealth medians by race
wealth_25_medians <- wmedian_calc(finance_survey_bw_25, wealth, c("race"))
hwealth_25_medians <- wmedian_calc(finance_survey_bw_25, hwealth, c("race"))

