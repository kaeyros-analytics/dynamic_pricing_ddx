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


# 1. load dataset (xtern and intern) ####
#___________________________________

# The customer data contains following information:
# 
# - internal and external data
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

# load required data set
dataset_intern_xtern <- DBI::dbGetQuery(conn = con, 
     statement=paste("SELECT * FROM dataset_intern_xtern", sep=""))



# 2. preparing data for modelization ####
#___________________________________





# 3. modeling dynamic pricing  (client-machine-view) ####
#___________________________________________________





# 4. modeling dynamic pricing  (machine-view) ####
#_____________________________________________




