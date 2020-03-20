##############################################
# R SCRIPT FOR DATA PLOTLY DATA VISUALIZATIONS
##############################################

library(httr)
library(jsonlite)
library(tidyverse)
library(plotly)

########################################################
# https://data.cms.gov/Medicare-Inpatient/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3
# ^^^ REFER TO THIS WEBSITE FOR VARIABLE DESCRIPTION ###

# Get data from the following source in JSON: 
# Inpatient Prospective Payment System (IPPS) Provider Summary for the 
# Top 100 Diagnosis-Related Groups (DRG) - FY2011
medicareIPPS.data <- fromJSON("https://data.cms.gov/resource/97k6-zzx3.json")
View(medicareIPPS.data)

# look at the data after removing duplicate rows
View(medicareIPPS.data %>%
       distinct())



# Check unique provider names b/c of duplicate DRG codes
unique(medicareIPPS.data$provider_id) # 1000 unique provider

# Look at unique number of providers for each state
provider.state <- medicareIPPS.data %>%
  group_by(provider_state) %>%
  summarize(number_providers_per_state = n_distinct(provider_id)) %>%
  arrange(desc(number_providers_per_state))


# **********************************
# ***** PLAY W/ PLOTLY HERE! ***** #