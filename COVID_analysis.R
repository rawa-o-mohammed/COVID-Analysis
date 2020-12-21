rm(list = ls(all = T))
library(xlsx)
library(plyr)
library(dplyr)
library(koboquest) # manage kobo questionnaires
library(kobostandards) # check inputs for inconsistencies
library(hypegrammaR)
library(surveyweights)
library(expss)
library(composr) # horziontal operations
library(readr)
library(readxl)

aggregate <- "district"
disaggregate <- "all"

dap_name <- "covid_analysis_2020"
name <- sprintf("covid_analysis_2020_%s_%s", aggregate, disaggregate)
source("COVID_factsheet_2020.R")

dap_name <- "covid_analysis_2019"
name <- sprintf("covid_analysis_2019_%s_%s", aggregate, disaggregate)
source("COVID_factsheet_2019.R")

dap_name <- "covid_analysis_2018"
name <- sprintf("covid_analysis_2018_%s_%s", aggregate, disaggregate)
source("COVID_factsheet_2018.R")
