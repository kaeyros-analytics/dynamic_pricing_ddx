# 0. Setting and loading packages  ####
#_______________________________

rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
cat("\f");

# setting system local encoding
Sys.setlocale("LC_ALL", "English_United States.932") # this works perfectly 

# setting some options
options(stringsAsFactors = FALSE)
options(Encoding = "latin1")

library(dplyr)
require(chron)
require(lubridate)
library(DBI)
library(RSQLite)
library(lubridate)
library(haven)
library(readxl)
library(readr)
library(zoo)
library(ggplot2)

# check loaded packages
loaded_packages <- as.data.frame(.packages())
View(loaded_packages)


# 1. loading weather data   ####
#__________________________

# weather data from  several countries (DE, AT, BE, CZ, ES, GR, IE, IT)
# temperature taken daily every hour
#
# data records from January 1980 to December 2019
# 
# we will aggregate the data at daily level and lately at month level


# set path to input data
path_input <- file.path(getwd(), "data/weather")
path_sqlite <- file.path(path_input, "weather_data.sqlite")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite)

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

#lDataFrames<- vector("list", length=length(tables))

# load testdata into R-environment
weather_data_init <- dbGetQuery(conn = con, statement=paste("SELECT * 
                                                  FROM weather_data", sep=""))
# keeping only DE-temperature information
weather_data <- weather_data_init %>%
  dplyr::select("utc_timestamp", starts_with("DE"))

# check structure of the data frame
str(weather_data)
  
# convert variable 'utc_timestamp' to datetime
weather_data$utc_timestamp <- lubridate::as_datetime(weather_data$utc_timestamp) 

# extract day and month from the timestamp_variable
weather_data$utc_date <- lubridate::as_date(weather_data$utc_timestamp)
weather_data$utc_date_day <- lubridate::day(weather_data$utc_timestamp)
weather_data$utc_date_month <- lubridate::month(weather_data$utc_timestamp)

# create temp_date variables
weather_data$temp_day <- year(weather_data$utc_date)*10000 + 
                               month(weather_data$utc_date)*100 + 
                               day(weather_data$utc_date)

weather_data$temp_month <- year(weather_data$utc_date)*100 + 
  month(weather_data$utc_date) 

# remove unnecessary variables
weather_data <- weather_data %>%
  dplyr::select(-DE_radiation_direct_horizontal, 
                -DE_radiation_diffuse_horizontal)

# aggregate temperature data at daily level
weather_data_day <- weather_data %>%
  dplyr::group_by(temp_day) %>%
  dplyr::summarize(avg_temperature_day =mean(DE_temperature, na.rm = TRUE))


# aggregate temperature data at monthly level
weather_data_month <- weather_data %>%
  dplyr::group_by(temp_month) %>%
  dplyr::summarize(avg_temperature_month = mean(DE_temperature, na.rm = TRUE))

# disconnect established connection
DBI::dbDisconnect(con)
  
# set path to input database
path_sqlite_db <- file.path(file.path(getwd(),"data"), 
                            "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite_db)

# list all tables
tables <- dbListTables(con)
tables

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "weather_data_ger_day", weather_data_day, overwrite = TRUE, 
             row.names = FALSE)

dbWriteTable(con, "weather_data_ger_month", weather_data_month, 
             overwrite = TRUE, row.names = FALSE)

DBI::dbDisconnect(con)

# 2. loading geo political data (GPR)   ####
#__________________________________________

# - GPR-Index := Geo Political Index
# - GPR-Index spikes around two world wars
# - The search is organized in eight (8) Categories
#   * War Threats (Category 1), Peace Threats (Category 2), M
#   * Military Buildups (Category 3), Nuclear Threats (Category 4), 
#   * Terror Threats (Category 5), Beginning of War (Category 6).
#   * Escalation of War (Category 7), Terror Acts (Category 8)
#
#
# we will aggregate the data at daily level and lately at month level

# # setting path to required data
# path_input <- file.path(getwd(), "data/geo_political_risk")
# path_gpr_data <- paste0(path_input, "/data_geo_political_risk_index_export.dta")
# 
# # reading the required data
# gpr_index_data_init <- haven::read_dta(path_gpr_data) 
# 
# # select only required variables
# gpr_index_data <- gpr_index_data_init %>%  
#   dplyr::select(month, GPR, GPRT, GPRA, GPRH, GPRHT, GPRHA, SHARE_GPR,
#                 N10, SHARE_GPRH, N3H, GPRH_NOEW, GPR_NOEW, GPRH_AND,
#                 GPR_AND, GPRH_BASIC, GPR_BASIC, SHAREH_CAT_1, SHAREH_CAT_2,
#                 SHAREH_CAT_3, SHAREH_CAT_4, SHAREH_CAT_5, SHAREH_CAT_6,
#                 SHAREH_CAT_7, SHAREH_CAT_8, GPRC_DEU)


# setting path to required data
path_input <- file.path(getwd(), "data/geo_political_risk")
path_gpr_data <- paste0(path_input, "/data_geo_political_risk_index_short.xls")

gpr_index_data_init <- readxl::read_excel(path_gpr_data)
View(gpr_index_data_init)
 
# select only required variables
gpr_index_data <- gpr_index_data_init %>%  
  dplyr::select(-var_name, -var_label) %>%
  dplyr::mutate(date_month = lubridate::year(month)*100 + month(month)) %>%
  dplyr::filter(date_month >= 200101 & date_month <= 202112)

# disconnect established connection
DBI::dbDisconnect(con)

# set path to input database
path_sqlite_db <- file.path(file.path(getwd(),"data"), 
                            "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite_db)

# list all tables
tables <- dbListTables(con)
tables

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "gpr_index_data_ger_month", gpr_index_data, overwrite = TRUE, 
             row.names = FALSE)


# 3. loading inflation rate data   ####
#________________________________

# - inflation rate at yearly level
# - interpolation could bring something here

# setting path to required data
path_input <- file.path(getwd(), "data/inflation")
path_gpr_data <- paste0(path_input, "/germany_inflation_rate_cpi_clean.csv")

inflation_rate_init <- readr::read_csv(path_gpr_data)

# create additional month_date variable
inflation_rate_init2 <- inflation_rate_init %>%
  dplyr::mutate(date_month = lubridate::year(date)*100 + 
                  lubridate::month(date)) %>%
  dplyr::filter(date_month >= 200101)

# interpolating the data to have information at monthly level
months_seq <- seq(as.Date("2001-02-01"), length = 252, by="1 month") - 1
months_seq_df <- data.frame(months_seq)

months_seq_df <- months_seq_df %>%
  dplyr::mutate(date_month = lubridate::year(months_seq)*100 + 
                  lubridate::month(months_seq))

# joining both data frames
inflation_rate_init3 <- inflation_rate_init2 %>%
  dplyr::right_join(months_seq_df, by = c("date_month" = "date_month")) %>%
  dplyr::arrange(date_month) 

# interpolating missing values
inflation_rate_init4 <- inflation_rate_init3 %>%
  dplyr::mutate(inflation_rate_interp = 
          imputeTS::na_interpolation(Inflation_Rate, option = "spline")) %>%
  dplyr::select(date_month, inflation_rate_interp) %>%
  dplyr::rename(inflation_rate = inflation_rate_interp)

# library(imputeTS)
# x <- inflation_rate_init3$Inflation_Rate
# y <- imputeTS::na.interpolation(x, option = "spline") # linear, stine
# plot(inflation_rate_init4$inflation_rate_interp)

# set path to input database
path_sqlite_db <- file.path(file.path(getwd(),"data"), 
                            "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite_db)

# list all tables
tables <- dbListTables(con)
tables

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "inflation_rate_data_ger_month", inflation_rate_init4, 
             overwrite = TRUE, row.names = FALSE)

# disconnecting
DBI::dbDisconnect(con)


# 4. loading natural_hazards data   ####
#________________________________

# - data on natural disasters earthquake data
# - data on natural disasters extreme temperature
# - data on natural disasters storms
# data records from January 1980 to December 2019
# 
# we will aggregate the data at daily level and lately at month level


# setting path to required data
path_input <- file.path(getwd(), "data/natural_hazards")
earth_quake_data <- paste0(path_input, "/natural_disasters_earthquacke.xlsx")

earth_quake_init <- readxl::read_excel(earth_quake_data)

# keeping only earthquake information
earth_quake_data_init2 <- earth_quake_init %>%
  dplyr::select("Entity", "Year", contains("earthquakes")) #%>%
  #dplyr::filter(Entity == "Germany")

# renaming columns of the data frame
col.from <- colnames(earth_quake_data_init2)
col.to <- c("Entity", "Year", "Nbr_deaths_earthquakes", 
            "Nbr_people_injured_earthquakes", "Nbr_people_affected_earthquakes",
            "Nbr_people_left_homeless_earthquakes", 
            "Nb_total_people_affected_earthquakes", 
            "Reconstruction_costs_earthquakes", 
            "Insured_damages_against_earthquakes", 
            "Total_economic_damages_earthquakes", "Death_rates_earthquakes",
            "Injury_rates_earthquakes", 
            "Nbr_people_affected_earthquakes_per_100k", 
            "Homelessness_rate_earthquakes",
            "Total_nber_people_affected_earthquakes_per_100k",
            "Total_economic_damages_earthquakes_share_of_GDP")

earth_quake_data <- earth_quake_data_init2 %>%
  rename_at(vars(col.from), function(x) col.to)


# set path to input database
path_sqlite_db <- file.path(file.path(getwd(),"data"), 
                            "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite_db)

# list all tables
tables <- dbListTables(con)
tables

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "earth_quake_data_ger_year", earth_quake_data, 
             overwrite = TRUE, row.names = FALSE)

# disconnecting
DBI::dbDisconnect(con)


# 5. loading wind data data   ####
#________________________________

# - data on wind data (current production, from wind data)
# - data records from January 1980 to December 2019

# setting path to required data
path_input <- file.path(getwd(), "data/pv_wind")
pv_wind_data_path <- paste0(path_input, "/ninja_pv_wind_profiles_singleindex.xlsx")

# reading the required data 
pv_wind_data_init <- readxl::read_excel(pv_wind_data_path)

# extract date from timestamp
pv_wind_data_init2 <- pv_wind_data_init %>%
  dplyr::mutate(date_pw = lubridate::date(pv_wind_data_init$time)) %>%
  dplyr::mutate(date_month_pw = year(date_pw)*100 + month(date_pw))

# renaming variables of the data frame
col.from <- colnames(pv_wind_data_init2)
col.to <- c("time", "DE_pv_national_current",          
            "DE_wind_national_current", "DE_wind_offshore_current",        
            "DE_wind_onshore_current", "DE_wind_national_long_termfuture",
            "DE_wind_national_near_termfuture",
            "DE_wind_offshore_near_termfuture",
            "DE_wind_onshore_near_termfuture", "date_pw",                         
            "date_month_pw")

pv_wind_data_init3 <- pv_wind_data_init2 %>%
  rename_at(vars(col.from), function(x) col.to)


# Aggregating data at month level
pv_wind_data_init4 <- pv_wind_data_init3 %>%
  dplyr::group_by(date_month_pw) %>%
  dplyr::summarize(
    DE_pv_national_current = sum(DE_pv_national_current, na.rm = TRUE),
    DE_wind_national_current = sum(DE_wind_national_current, na.rm = TRUE),
    DE_wind_offshore_current = sum(DE_wind_offshore_current, na.rm = TRUE),
    DE_wind_national_long_termfuture = sum(DE_wind_national_long_termfuture, na.rm = TRUE),
    DE_wind_national_near_termfutur = sum(DE_wind_national_near_termfuture, na.rm = TRUE),
    DE_wind_offshore_near_termfuture = sum(DE_wind_offshore_near_termfuture, na.rm = TRUE),
    DE_wind_onshore_near_termfuture = sum(DE_wind_onshore_near_termfuture, na.rm = TRUE)
    ) 

# create final data frame
pv_wind_data <- pv_wind_data_init4 %>%
  filter(date_month_pw >= 200101 & date_month_pw <= 201912)

# set path to input database
path_sqlite_db <- file.path(file.path(getwd(),"data"), 
                            "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite_db)

# list all tables
tables <- dbListTables(con)
tables

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "pv_wind_data_ger_month", pv_wind_data, 
             overwrite = TRUE, row.names = FALSE)

# disconnecting
DBI::dbDisconnect(con)


# 6. loading customer relation data   ####
#__________________________________

# - company size (small, medium, large)
# - number of years of relationship between customer and insurance company

# set path to input data
path_input <- file.path(getwd(), "data")
path_sqlite_db <- file.path(path_input, "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite_db)

# list all tables
tables <- dbListTables(con)
tables

# read required data
test_data <- dbGetQuery(conn = con, statement=paste("SELECT * 
                                                  FROM testdata", sep=""))


# extract client information
clients <- unique(test_data$client_id) 
length(clients) # 100

# create a data frame containing the clients_id
clients_df <- data.frame(client_id = clients)

# Adding required information to the client_id-data frame
set.seed(202306)

clients_df$company_size <- sample(c("small", "middle", "large"), 
                                    prob = c (0.3, 0.6, 0.1), 
                                  size = nrow(clients_df), replace = TRUE)

clients_df$duration_customer_relation <- sample(c(2, 3, 5, 7, 10, 25), 
                                       prob = c(0.05, 0.25, 0.30, 0.10 , 0.18,
                                                0.12),
                                  size = nrow(clients_df), replace = TRUE)


# Adding the created variables to the initial data frame
customers_data <- test_data %>%
  dplyr::left_join(clients_df, by = c("client_id" = "client_id"))

# renaming variables of the data frame
col.from <- colnames(customers_data)
col.to <- c("id", "client_id", "machine_id", "date",
            "lot_health_index", "fixed_premium_rate",               
             "dynamic_premium_rate", "avg_market_premium_rate",   
             "localization_lat", "localization_lon", "company_size",
             "duration_customer_relation")

customers_data2 <- customers_data %>%
  rename_at(vars(col.from), function(x) col.to)

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "customers_data_ger_month", customers_data2, 
             overwrite = TRUE, row.names = FALSE)

# disconnecting
DBI::dbDisconnect(con)



