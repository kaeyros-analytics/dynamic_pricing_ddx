# 0. Setting and loading packages  ####
#_______________________________

rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
cat("\f");

# setting system local encoding
Sys.setlocale("LC_ALL", "English_United States.932") # this works perfectly 

# setting some options
options(stringsAsFactors = FALSE)
options(Encoding = "latin1")
options(digits=5)

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
library(tidyverse) # for easy data manipulation and visualization
library(caret) # for easy machine learning workflow
library(xgboost) # for computing boosting algorithm


# check loaded packages
loaded_packages <- as.data.frame(.packages())
#View(loaded_packages)


# 1. load dataset (external/ internal) ####
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

# disconnecting
DBI::dbDisconnect(con)


# 2. preparing data for modelization ####
#___________________________________

# inspect structure of the database
str(dataset_intern_xtern$month)

# using data starting from 2005 (because of gpr_data)
temp <- dataset_intern_xtern %>%
  dplyr::filter(date_month >= 200501)

# remove unnecessary variables
input_data_init <- temp  %>%
  select(-dynamic_premium_rate, -localization_lat, -localization_lon,
         -month, -Ind_Month_dmd, -Ind_Month_prd_index, -Ind_Year_dmd,
         - Ind_Year_prd_index, -GPR_Threats, -GPR_Acts)

str(input_data_init)
#View(input_data_init)

# simulating a margin variable (according distr of dyn_price for commodity)
temp_vector <- temp$dynamic_premium_rate 
                      
# changing the scale of the initial vector (10 - 40 % margin)
profit_margin <- scales::rescale(temp_vector, to = c(0.1, 0.4))

# adding the profit margin to the data frame
input_data_init$profit_margin <- profit_margin

# converting all character columns to factors for Similarity calculation
temp <- input_data_init
temp2 <- as.data.frame(unclass(temp), stringsAsFactors = TRUE)
str(temp2)

# Encoding all factor/categorical features using ordinal encoding

# to_be_changed <- which(colnames(temp) == "company_size")
# 
# # defining function for ordinal encoding
# encode_ordinal <- function(x, order = unique(x)) {
#   x <- as.numeric(factor(x, levels = order, exclude = NULL))
#   x
# }
# 
# temp3 <- as.data.frame(apply(temp2[, to_be_changed], 2, 
#                                encode_ordinal))

# temp3 <- temp2 %>%
#   dplyr::mutate(
#     company_size_2 = case_when(
#       company_size == "small" ~ 1,
#       company_size == "middle" ~ 2,
#       company_size == "large" ~ 3,
#       TRUE ~ "Unknown"
#     )
#   )

check <- ifelse(temp2$company_size == "small", 1, 
                ifelse(temp2$company_size == "middle", 2, 
                       ifelse(temp2$company_size == "large", 3, 999)))


temp3 <- temp2
temp3$company_size <- NULL
temp3$company_size <- check
str(temp3)

# rescaling numeric variables
temp4 <- temp3
cols <- sapply(temp4, is.numeric)
temp4[cols] <- base::scale(temp4[cols])


# 3.1 Scenario 1: hierarchical modeling (scaled data)  ####
#___________________________________________________

# working with scaled data
input_data_1 <- temp4$ave # scaled

# select a machine-type 
temp5 <- input_data_1 %>%
  dplyr::filter(machine_id == "M_001") %>%
  dplyr::select(-id, -machine_id, -date, -date_month, -fixed_premium_rate,
                -WLD_dmd, -WLD_prd_index) 

# plot --> nor so good
plot(log1p(temp5$lot_health_index), log1p(temp5$profit_margin))


# use firstly only few variables (to see whats happen with the model)
clients <- levels(temp5$client_id) 
nreg <- length(clients) # 100 clients
col_names <- colnames(temp5)[-c(1, 39)] # 40 predictors, 1 target variable
ncols <- length(colnames(temp5)) - 1

beta.ls <- matrix(NA, nrow = nreg, ncol = ncols)
colnames(beta.ls) <-  c("Intercept", col_names)


# creating regression data (y = profit_margin, X = Matrix of Regressors)
regdata <-  NULL

for (i in 1:nreg){
  
  filter <- temp2$client_id == clients[i]
  y <- temp5$profit_margin
  X <- cbind(1, temp5$lot_health_index, temp5$avg_market_premium_rate,
             temp5$duration_customer_relation, temp5$GPR, temp5$inflation_rate,
             temp5$DE_pv_national_current, temp5$DE_wind_national_current,
             temp5$DE_wind_offshore_current, temp5$DE_wind_onshore_current,
             temp5$DEU_dmd, temp5$ITA_dmd, temp5$FRA_dmd, temp5$ESP_dmd,
             temp5$GBR_dmd, temp5$RUS_dmd, temp5$CHN_dmd, temp5$JPN_dmd,
             temp5$IND_dmd, temp5$KOR_dmd, temp5$USA_dmd, temp5$CAN_dmd,
             temp5$MEX_dmd, temp5$BRA_dmd, temp5$DEU_prd_index, 
             temp5$ITA_prd_index, temp5$FRA_prd_index, temp5$ESP_prd_index,
             temp5$GBR_prd_index, temp5$RUS_prd_index, temp5$CHN_prd_index, 
             temp5$JPN_prd_index,temp5$IND_prd_index, temp5$KOR_prd_index, 
             temp5$USA_prd_index, temp5$CAN_prd_index, temp5$MEX_prd_index, 
             temp5$BRA_prd_index, temp5$company_size)
  
  regdata[[i]] <- list(y=y, X=X)
}

# Find Hierarchical models estimators
Data <- list(regdata = regdata)
sim.size <- 2000
burn.in <- 1000
Mcmc <- list(R = sim.size)

set.seed(2008)
system.time(out <- bayesm::rhierLinearModel(Data = Data, Mcmc = Mcmc))

# extracting beta parameters for the price (build dynamic price)
beta_draw <- out$betadraw[, 3, burn.in:sim.size]
b_avg_price <- round(apply(beta_draw, 1, mean), 6)

# calculating the simulated value of the dynamic price (average_price .coefficent)

# beta_draw <- out$betadraw[, 1, burn.in:sim.size]
# int <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 2, burn.in:sim.size]
# b_iot <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 3, burn.in:sim.size]
# b_log_price <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 4, burn.in:sim.size]
# b_log_avg_price <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 5, burn.in:sim.size]
# b_duration_cust <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 6, burn.in:sim.size]
# b_company_size <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 7, burn.in:sim.size]
# b_log_GPRH <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 8, burn.in:sim.size]
# b_log_wind_nat <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 9, burn.in:sim.size]
# b_log_pv_nat <- round(apply(beta_draw, 1, mean), 6)
# 
# beta_draw <- out$betadraw[, 10, burn.in:sim.size]
# b_inflation <- round(apply(beta_draw, 1, mean), 6)


# create beta_matrix for the client
# beta_client <- cbind.data.frame(clients, int, b_iot, b_log_price, 
#     b_log_avg_price, b_duration_cust, b_company_size, b_log_GPRH,
#     b_log_wind_nat, b_log_pv_nat, b_inflation)



# 3.2 Scenario 2: hierarchical modeling (unscaled data)  ####
#______________________________________________________

# working with scaled data
input_data_1 <- temp3 # scaled

# select a machine-type 
temp5 <- input_data_1 %>%
  dplyr::filter(machine_id == "M_001") %>%
  dplyr::select(-id, -machine_id, -date, -date_month, -fixed_premium_rate,
                -WLD_dmd, -WLD_prd_index) 

# plot --> nor so good
plot(log1p(temp5$lot_health_index), log1p(temp5$profit_margin))


# use firstly only few variables (to see whats happen with the model)
clients <- levels(temp5$client_id) 
nreg <- length(clients) # 100 clients
col_names <- colnames(temp5)[-c(1, 39)] # 40 predictors, 1 target variable
ncols <- length(colnames(temp5)) - 1

beta.ls <- matrix(NA, nrow = nreg, ncol = ncols)
colnames(beta.ls) <-  c("Intercept", col_names)


# creating regression data (y = profit_margin, X = Matrix of Regressors)
regdata <-  NULL

for (i in 1:nreg){
  
  filter <- temp2$client_id == clients[i]
  y <- temp5$profit_margin
  X <- cbind(1, temp5$lot_health_index, temp5$avg_market_premium_rate,
             temp5$duration_customer_relation, temp5$GPR, temp5$inflation_rate,
             temp5$DE_pv_national_current, temp5$DE_wind_national_current,
             temp5$DE_wind_offshore_current, temp5$DE_wind_onshore_current,
             temp5$DEU_dmd, temp5$ITA_dmd, temp5$FRA_dmd, temp5$ESP_dmd,
             temp5$GBR_dmd, temp5$RUS_dmd, temp5$CHN_dmd, temp5$JPN_dmd,
             temp5$IND_dmd, temp5$KOR_dmd, temp5$USA_dmd, temp5$CAN_dmd,
             temp5$MEX_dmd, temp5$BRA_dmd, temp5$DEU_prd_index, 
             temp5$ITA_prd_index, temp5$FRA_prd_index, temp5$ESP_prd_index,
             temp5$GBR_prd_index, temp5$RUS_prd_index, temp5$CHN_prd_index, 
             temp5$JPN_prd_index,temp5$IND_prd_index, temp5$KOR_prd_index, 
             temp5$USA_prd_index, temp5$CAN_prd_index, temp5$MEX_prd_index, 
             temp5$BRA_prd_index, temp5$company_size)
  
  regdata[[i]] <- list(y=y, X=X)
}

# Find Hierarchical models estimators
Data <- list(regdata = regdata)
sim.size <- 2000
burn.in <- 1000
Mcmc <- list(R = sim.size)

set.seed(2008)
system.time(out <- bayesm::rhierLinearModel(Data = Data, Mcmc = Mcmc))


# 4. Building the final data frame with simulated dyn price   ####
#__________________________________________________________






# 4. Scenario 2: Simulation dynamic price   ####
#________________________________________


# 6. Exporting 2: Simulation dynamic price   ####
#________________________________________

input_data_final <- temp3 %>%
  dplyr::rename("dynamic_pricing" = "avg_market_premium_rate")

# set path to input data
path_input <- file.path(getwd(), "data")
path_sqlite <- file.path(path_input, "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite)

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "input_data_final", input_data_final, 
             overwrite = TRUE, row.names = FALSE)

# disconnecting
DBI::dbDisconnect(con)


input_data_final_scaled <- temp4 %>%
  dplyr::rename("dynamic_pricing" = "avg_market_premium_rate")

# set path to input data
path_input <- file.path(getwd(), "data")
path_sqlite <- file.path(path_input, "dyn_pricing_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite)

# write the weather data into the dyn_pricing sqlite-db
dbWriteTable(con, "input_data_final_scaled", input_data_final_scaled, 
             overwrite = TRUE, row.names = FALSE)

# disconnecting
DBI::dbDisconnect(con)

