library(plyr)
library(readr)
library(readxl)

# Constructing path of relevant directories
#root <- getwd()
#path_data <- paste(root, "/", "input", sep="")


#path = getwd() 
#setwd('path')
#install.packages("readxl")

#demographic
demog_dat_1 <- read.csv("./data/demographic/12111-0102.csv",skip=7, sep = ';',check.names = F)
demog_dat_2 <- read.csv("./data/demographic/12211-0001.csv",skip=5, sep = ';', check.names = F)
demog_dat_3 <- read.csv("./data/demographic/12211-0200.csv",skip=10, sep = ';', check.names = F)
demog_dat_4 <- read.csv("./data/demographic/12211-0201.csv",skip=9, sep = ';',check.names = F)
demog_dat_5 <- read.csv("./data/demographic/12211-0206.csv",skip=9, sep = ';',check.names = F)


### generation_capacity #####

gen_cap_dat1 <- read.csv("./data/generation_capacity/national_generation_capacity_stacked.csv")
gen_cap_dat2 <- read_excel("./data/generation_capacity/national_generation_capacity.xlsx")


## inflation

infl_dat_1 <- read.csv("./data/inflation/germany-inflation-rate-cpi.csv",skip=16)
infl_dat_2 <- read.csv("./data/inflationC/61111-0002.csv",skip=5, sep = ";")
infl_dat_3 <- read.csv("./data/inflationC/61111-0011.csv",skip=5)
infl_dat_4 <- read.csv("./data/inflationC/61351-0002.csv",skip=6,sep = ";")

## household
household_dat_1 <- read.csv("./data/household/household_data_60min_singleindex.csv", sep = ",")
household_dat_2 <- read.csv("./data/household/household_data_1min_singleindex.csv",sep = ",")
household_dat_3 <- read.csv("./data/household/household_data_15min_singleindex.csv",sep = ",")
household_dat_4 <-read_excel("./data/household/household_data.xlsx",skip=6)# to check


### weather ###
weather_data_1 <- read.csv("./data/weatherC/3066483.csv") 
weather_data_2 <- read.csv("./data/weather/weather_data.csv")

### energie #### all to check
energie_data_1 <- read_excel("./data/energie_daten/energiedaten-energiegewinnung-verbrauch-1-xls.xlsx")
energie_data_2 <- read_excel("./data/energie_daten/energiedaten-energiegewinnung-verbrauch-5-xls (1).xlsx")
energie_data_3 <- read_excel("./data/energie_daten/energiedaten-energiegewinnung-verbrauch-5-xls.xlsx")
energie_data_4 <- read_excel("./data/energie_daten/energiedaten-energietraeger-06-xls.xlsx")
energie_data_5 <- read_excel("./data/energie_daten/energiedaten-energietraeger-08-xls.xlsx")
energie_data_6 <- read_excel("./data/energie_daten/energiedaten-energietraeger-10-xls.xlsx")
energie_data_7 <- read_excel("./data/energie_daten/energiedaten-rahmendaten-1-2-xls.xlsx")

### natural_hazards #### 
#natural_hazards_data_1 <- read_excel("./data/natural_hazards/data_geo_political_risk_index_export.xlsx") 
natural_hazards_data_2 <- read.csv("./data/natural_hazards/natural_disasters_earthquacke.csv")
natural_hazards_data_3 <- read.csv("./data/natural_hazards/natural_disasters_extreme_temperature.csv")
natural_hazards_data_4 <- read.csv("./data/natural_hazards/natural_disasters_storms.csv")
natural_hazards_data_5 <- read.csv("./data/natural_hazards/natural-disasters_dtorms.csv")
natural_hazards_data_6 <- read.csv("./data/natural_hazards/natural-hazards-in-eea-member-countries-6.csv")
natural_hazards_data_7 <- read.csv("./data/natural_hazards/various-measures-of-child-labour-incidence.csv")

### pv_wind ####
pv_wind_data_1 <- read.csv("./data/pv_wind/ninja_pv_wind_profiles_singleindex.csv")

### time_series ####
time_serie_data_1 <- read_excel("./data/time_series/time_series.xlsx")
time_serie_data_2 <- read.csv("./data/time_series/time_series_15min_singleindex.csv")
time_serie_data_3 <- read.csv("./data/time_series/time_series_30min_singleindex.csv")
time_serie_data_4 <- read.csv("./data/time_series/time_series_60min_singleindex.csv")

### when_2_heat ####
when_2_heat_data_1 <- read_excel("./data/when_2_heat/when2heat.xlsx")
when_2_heat_data_2 <- read.csv("./data/when_2_heat/when2heat.csv")
