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
# View(loaded_packages)


# 1. loading internal and external data   ####
#__________________________

# The customer data contains following information:
# 
# - client_id, machine_id, iot_health_index
# - iot_health_index, fixed_premium_rate, average_premium_rate,
# ....

# data records from January 2001 to December 2020


# set path to input data
path_input <- file.path(getwd(), "data")
path_sqlite <- file.path(path_input, "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite)

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]
tables

# load customer data into R-environment
customers_data_init <- dbGetQuery(conn = con, 
          statement=paste("SELECT * FROM customers_data_ger_month", sep=""))
          # 202112

# load gpr_index data
gpr_index_data_init <- DBI::dbGetQuery(conn = con, 
      statement=paste("SELECT * FROM gpr_index_data_ger_month", sep=""))
         # 202112

# load inflation rate data
inflation_rate_data_init <- DBI::dbGetQuery(conn = con, 
    statement=paste("SELECT * FROM inflation_rate_data_ger_month", sep="")) 
        # 202112

# load weather data
weather_data_init <- DBI::dbGetQuery(conn = con, 
    statement=paste("SELECT * FROM weather_data_ger_month", sep="")) 
       # 201912

# load energy production data
pv_wind_data_init <- DBI::dbGetQuery(conn = con, 
    statement=paste("SELECT * FROM pv_wind_data_ger_month", sep="")) 
     # 201912

# load demand forecast data
industry_demand_forecast_init <- DBI::dbGetQuery(conn = con, 
   statement=paste("SELECT * FROM industry_demand_forecast", sep="")) 
    # 201912

# load production index data
production_index_data_init <- DBI::dbGetQuery(conn = con, 
     statement=paste("SELECT * FROM production_index_data", sep="")) 
  # 201912



# restrict the time frame from 2001-01 to 2019-12
customers_data <- customers_data_init %>%
 dplyr::mutate(date_month = 
                 lubridate::year(date)*100 + lubridate::month(date)) %>%
  dplyr::filter(date_month >= 200101 & date_month <= 201912)

gpr_index_data <- gpr_index_data_init %>%
  dplyr::filter(date_month >= 200101 & date_month <= 201912)

inflation_rate_data <- inflation_rate_data_init %>%
  dplyr::filter(date_month >= 200101 & date_month <= 201912)

pv_wind_data <- pv_wind_data_init %>%
  dplyr::filter(date_month_pw >= 200101 & date_month_pw <= 201912) %>%
  dplyr::rename("date_month" = "date_month_pw")

industry_demand_forecast <- industry_demand_forecast_init %>%
  dplyr::filter(Ind_Month_Code_dmd >= 200101 & 
                  Ind_Month_Code_dmd <= 201912) %>%
  dplyr::rename("date_month" = "Ind_Month_Code_dmd")

production_index_data <- production_index_data_init %>%
  dplyr::filter(Ind_Month_Code_prd_index >= 200101 & 
                  Ind_Month_Code_prd_index <= 201912) %>%
  dplyr::rename("date_month" = "Ind_Month_Code_prd_index")



# 2. combine internal and external data sources  ####
#______________________________________________

#  internal factors (customers_data)
# 
# - client_id, machine_id, iot_health_index
# - iot_health_index, fixed_premium_rate, average_premium_rate,
# ....
# data records from January 2001 to December 2019 (228 records available)


# joining all internal and external data sources
dataset_intern_xtern <- customers_data %>%
  dplyr::left_join(gpr_index_data, by = "date_month") %>%
  dplyr::left_join(inflation_rate_data, by = "date_month") %>%
  dplyr::left_join(pv_wind_data, by = "date_month") %>%
  dplyr::left_join(industry_demand_forecast, by = "date_month") %>%
  dplyr::left_join(production_index_data, by = "date_month") #%>%

# 68400  observations of 56 variables

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "dataset_intern_xtern", dataset_intern_xtern, 
             overwrite = TRUE, row.names = FALSE)

# disconnecting
DBI::dbDisconnect(con)

