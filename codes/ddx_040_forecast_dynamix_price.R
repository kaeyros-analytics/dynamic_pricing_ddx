
# 0. Setting and loading packages  ####
#_______________________________

rm(list = ls(all.names = TRUE)); # activate the step before execution !!!!!!
cat("\f");

# setting system local encoding
Sys.setlocale("LC_ALL", "English_United States.932") # this works perfectly ---> für japanese characters

# setting some options
options(stringsAsFactors = FALSE)
options(Encoding = "latin1")

# Loading required libraries
# library(rJava)
# library(DBI)
# library(RJDBC)
# library(pool)
# library(shiny)
# library(sf)
# library(shinyWidgets)
# library(stringr)
# library(leaflet)
# library(plotly)
# library(dplyr)
# library(DT)
# library(shinycssloaders)

# library(pool)
# library(lubridate)
# library(viridisLite)
# library(igraph)
# library(visNetwork)
# library(zoo)
# library(rintrojs)
# library(readxl)
# library(shinyjs)
# library(openxlsx)
# library(bcrypt)
# library(glue)
# library(plotly)

# library(ggplot2)
# library(magrittr)
# library(sass)
# library(shinydashboard)
# library(shinythemes)
# library(shiny.react)
# library(shiny.router)
# require(tidyverse)
# require(tidymodels)
# require(data.table)
# require(tidyposterior)
# require(tsibble)       # tsibble for time series based on tidy principles
# require(fable)         # for forecasting based on tidy principles
# require(ggfortify)     # for plotting timeseries
# require(forecast)      # for forecast function
# require(tseries)

# require(directlabels)
# require(zoo)
# require(lmtest)
# require(TTR)            # for smoothing the time series
# require(MTS)
# require(vars)
# require(fUnitRoots)
# require(lattice)
# require(grid)

library(fpp2)
library(dplyr)
require(chron)
require(lubridate)
library(DBI)
library(RSQLite)
library(scales)
require(tseries)
require(directlabels)
require(zoo)
require(lmtest)
require(TTR)            # for smoothing the time series
require(MTS)
require(vars)
require(fUnitRoots)
require(lattice)
require(grid)

# check loaded packages
loaded_packages <- as.data.frame(.packages())
View(loaded_packages)


# 1. loading required data   ####
#__________________________

# set path to input data
path_input <- file.path(getwd(), "data")
path_sqlite <- file.path(path_input, "generated_db.sqlite3")  

# connect to the sqlite database
con <- dbConnect(drv=RSQLite::SQLite(), dbname = path_sqlite)

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

# load testdata into R-environment
df_testdata <- dbGetQuery(conn = con, statement=paste("SELECT * FROM testdata", 
                                                   sep=""))
# check structure of the loaded data 
str(df_testdata)

# use parse_time in order to create standard ambiguous date format.
df_testdata <- df_testdata %>%
  mutate(date = as.Date(date , format = "%Y-%m-%d"))

str(df_testdata)

# subset the data frame 
df_init <- df_testdata %>%
  filter( client_id == "client_0",
          machine_id == "M_001") %>%
  dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price, 
                fixed_price, localization_lat, localization_lon) %>% 
  relocate(dynamic_price, .before = lot_health_index)
  

df_filter <- df_testdata %>%
  filter( client_id == "client_0",
          machine_id == "M_001") %>%
  dplyr::select(lot_health_index, dynamic_price,avg_market_premium_price) %>% 
  relocate(dynamic_price, .before = lot_health_index)

class(df_filter) # [1] "data.frame


df_filter_light <- df_filter %>%
  # filter( client_id == "client_0",
  #         machine_id == "M_001") %>%
  dplyr::select(lot_health_index, dynamic_price)

class(df_filter_light) 

# Converting a data.frame into mts = Multivariate Time Series
mts_df_filter_light <- stats::ts(df_filter_light,
                           frequency = 12,
                           start = c(2001, 1),
                           end = c(2021, 12))

# Scatterplot matrices
autoplot(mts_df_filter_light, facets=TRUE) +
  ylab("") +
  ggtitle("dynamic_price vs. iot_health_index")


# rescale the iot_health_index to avoid NA by log errors (from 20 to 100)
df_init$lot_health_index <- scales::rescale(df_init$lot_health_index, 
                                            to = c(20, 100)) #  rescale 
                              
df_filter$lot_health_index <- scales::rescale(df_filter$lot_health_index, 
                                            to = c(20, 100)) #  rescale 


# 2. exploring time series data   ####
#______________________________

# Converting a data.frame into mts = Multivariate Time Series
mts_df_filter <- stats::ts(df_filter,
                           frequency = 12,
                           start = c(2001, 1),
                           end = c(2021, 12))

class(mts_df_filter) # [1] "mts"    "ts"     "matrix"

# checking the structure of the created multivariate time series
head(mts_df_filter)
str(mts_df_filter)
colnames(mts_df_filter) # [1] "dynamic_price" "lot_health_index" 
                        # "avg_market_premium_price"

# Visualizing all time series at once
theme_set(theme_bw()) 
fig0 <- autoplot(mts_df_filter) +
  ggtitle("All Time Series at once") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text
plotly::ggplotly(fig0)


theme_set(theme_bw()) 
fig0 <- autoplot(mts_df_filter) +
  ggtitle("All Time Series at once") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text
plotly::ggplotly(fig0)


# Visualize the "iot_health_index" time series
fig1 <- autoplot(mts_df_filter[,"lot_health_index"], colour = "#7879FF") +
  ggtitle("Iot_healt_index") +
  xlab("Year") +
  ylab("Iot health index") #+
  #theme_bw()

plotly::ggplotly(fig1)

# Visualize the "dynamic_price" time series
fig2 <- autoplot(mts_df_filter[,"dynamic_price"], colour = "#B03F3C") +
  ggtitle("dynamic_prices") +
  xlab("Year") +
  ylab("Dynamic price")

plotly::ggplotly(fig2)


# Visualize the "avg_market_premium_price" time series
fig3 <- autoplot(mts_df_filter[,"avg_market_premium_price"]) +
  ggtitle("avg_market_premium_price") +
  xlab("Year") +
  ylab("Avg market premium price")

plotly::ggplotly(fig3)


# Seasonal Plot
ggseasonplot(mts_df_filter[,"lot_health_index"], year.labels=TRUE, 
             year.labels.left=TRUE) +
  ylab("Iot health index") +
  ggtitle("Seasonal plot: Iot health index")

# Seasonal Plot
ggseasonplot(mts_df_filter[,"dynamic_price"], year.labels=TRUE, 
             year.labels.left=TRUE) +
  ylab("Dynamic price") +
  ggtitle("Seasonal plot: Dynamic price")

# Seasonal Plot
ggseasonplot(mts_df_filter[,"avg_market_premium_price"], year.labels=TRUE,
             year.labels.left=TRUE) +
  ylab("Avg market premium pric") +
  ggtitle("Seasonal plot: Avg market premium pric")


# Scatterplot matrices
autoplot(mts_df_filter, facets=TRUE) +
  ylab("")

GGally::ggpairs(as.data.frame(mts_df_filter))

# lag plots
gglagplot(mts_df_filter[,"lot_health_index"])

# Autocorrelation
ggAcf(mts_df_filter[,"lot_health_index"])


gglagplot(mts_df_filter[,"dynamic_price"])

# Autocorrelation
ggAcf(mts_df_filter[,"dynamic_price"])


gglagplot(mts_df_filter[,"avg_market_premium_price"])

# Autocorrelation
ggAcf(mts_df_filter[,"avg_market_premium_price"])




# 3. assessing stationarity of the time series   ####
#_____________________________________________

### test stationarity using standard adf.test
apply(mts_df_filter, 2, tseries::adf.test)


# Alternative: lib fUnitRoots, function
apply(mts_df_filter, 2, fUnitRoots::adfTest, 
      lags=0 , 
      type="c", 
      title = "ADF Test for mts_df_filter Data") 


# apply a log-transformation to enable stationarity of the time series
mts_df_filter_log <- log1p(mts_df_filter)
class(mts_df_filter_log) # "mts"  "ts"  "matrix"

# Visualizing all time series at once after transformation
theme_set(theme_bw()) 
fig0 <- autoplot(mts_df_filter_log) +
  ggtitle("All Time Series at once") +
  theme(plot.title = element_text(hjust = 0.5)) #for centering the text
plotly::ggplotly(fig0)

# assessing stationarity of the transformed variable
apply(mts_df_filter_log, 2, tseries::adf.test)

apply(mts_df_filter_log, 2, fUnitRoots::adfTest, 
      lags=0 , 
      type="c", 
      title = "ADF Test for mts_df_filter Data") 


# for a significance-level of 90% all three time series
#
#    1- iot_health_index
#
#    2- dynamic_price
#
#    3- average_market_price
#
#     are stationary !!

ggAcf(mts_df_filter_log)


# 4. VAR and VARMA Modelization  ####
#______________________________

# identifying optimal lag order
vars::VARselect(log1p(mts_df_filter),
                type = "none", 
                lag.max = 10) 

# max lag = 1

# Creating a VAR model using the vars package
var.a.mts_df_filter <- vars::VAR(mts_df_filter_log,
                                 lag.max = 1, #highest lag order for lag length selection according to the choosen ic
                                 ic = "AIC", #information criterion
                                 type = "none") #type of deterministic regressors to include


summary(var.a.mts_df_filter) 

# Estimation results for equation dynamic_price: 
#
#   dynamic_price = dynamic_price.l1 + lot_health_index.l1 + 
#     avg_market_premium_price.l1 
# 
# Estimate Std. Error t value Pr(>|t|)    
# dynamic_price.l1             1.002e+00  2.532e-03 395.798  < 2e-16 ***
#   lot_health_index.l1          1.974e-04  6.582e-05   2.999  0.00299 ** 
#   avg_market_premium_price.l1 -2.244e-03  2.533e-03  -0.886  0.37648    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# 
# Residual standard error: 0.0003183 on 248 degrees of freedom
# Multiple R-Squared:     1,	Adjusted R-squared:     1 
# F-statistic: 2.385e+10 on 3 and 248 DF,  p-value: < 2.2e-16 

# Residuals diagnostics
vars::serial.test(var.a.mts_df_filter)

# Granger test for causality
causality(var.a.mts_df_filter, #VAR model
          cause = c("dynamic_price")) #cause variable. If not specified then first column of x is used. Multiple variables can be used. 


# $Granger
# 
# Granger causality H0: dynamic_price do not Granger-cause lot_health_index 
# avg_market_premium_price
# 
# data:  VAR object var.a.mts_df_filter
# F-Test = 118.07, df1 = 2, df2 = 744, p-value < 2.2e-16
# 
# 
# $Instant
# 
# H0: No instantaneous causality between: dynamic_price and lot_health_index 
# avg_market_premium_price
# 
# data:  VAR object var.a.mts_df_filter
# Chi-squared = 119.67, df = 2, p-value < 2.2e-16



# 5. Forecasting dynamic price using the VAR-Model  ####
#_________________________________________________

# forecasting VAR models
fcast = predict(var.a.mts_df_filter, n.ahead = 12) # we forecast over a short 
#horizon because beyond short horizon prediction becomes unreliable or uniform
#par(mar = c(2.5,2.5,2.5,2.5))
plot(fcast)

# forecasting dynamic price using the VAR Model
dynamic_price <- fcast$fcst[1]; 
dynamic_price # type list

# Extracting the forecast column
x = dynamic_price$dynamic_price[,1] 

# Inverting the forecast value to the original value
x_predict <- exp(x) - 1

# Adding predicted data to the original time series 
dynamic_price_inv <- ts(c(mts_df_filter[,1], x_predict),
                        frequency = 12,
                        start = c(2001, 1),
                        end = c(2022, 12))

# visualizing the time series including all points 
plot(dynamic_price_inv)


# visualizing the time series including all points 
plot.ts(dynamic_price_inv[217:length(dynamic_price_inv)])


# 
dynamic_price_inv_df <- as.data.frame(dynamic_price_inv[217:length(dynamic_price_inv)]) 
colnames(dynamic_price_inv_df) <- c("x")
head(dynamic_price_inv_df)

# ggplot() + 
#   geom_line(data = as.data.frame(dynamic_price_inv_df[1:36,]),
#             aes(y = get("dynamic_price_inv_df[1:36, ]"), x = seq(1, 36)), 
#             color = "blue") +
#   geom_line(data = as.data.frame(dynamic_price_inv_df[37:48,]), 
#             aes(y = get("DAXinv_datframe[37:48, ]"), x = seq(37, 48)), 
#             color = "red") +
#   ggtitle("forecast of the Dynamic Price for 6 Months") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   xlab("Time") + ylab("Value")


# ## Creating an advanced plot with visual separation
# # Converting to object zoo
# x = zoo(dynamic_price_inv_df)
# 
# # Advanced xyplot from lattice
# xyplot(x, grid=TRUE, panel = function(x, y, ...){
#   panel.xyplot(x, y, col="red", ...)
#   grid.clip(x = unit(36, "native"), just=c("right"))
#   panel.xyplot(x, y, col="blue", ...) })


# 6. Calculation Gain/Losses  ####
#___________________________

# we calculate here the gain/losses made based on decision to use 
# dynamic or fixed price price
# for this purpose we model using 70% of the  

# creating train and test data set
mts_df_filter_log_df  <- (as.data.frame(mts_df_filter_log))

mts_df_filter_log_train <- mts_df_filter_log_df[1:240, ] # 70% of the data
mts_df_filter_log_test <- mts_df_filter_log_df[241:252, ] # 30% of the data


# converting to time series
mts_df_filter_log_train <- ts(mts_df_filter_log_train,
                              frequency = 12,
                              start = c(2001, 1),
                              end = c(2020, 12))

# identifying optimal lag order
vars::VARselect(log1p(mts_df_filter_log_train),
                type = "none", 
                lag.max = 10) 

# max lag = 1

# Creating a VAR model using the vars package
var.train <- vars::VAR(mts_df_filter_log_train,
                                 lag.max = 1, #highest lag order for lag length selection according to the choosen ic
                                 ic = "AIC", #information criterion
                                 type = "none") #type of deterministic regressors to include


summary(var.train)

# forecasting VAR models
fcast = predict(var.train, n.ahead = 12) #
plot(fcast)

# forecasting dynamic price using the VAR Model
dynamic_price <- fcast$fcst[1]; 
dynamic_price # type list

# Extracting the forecast column
x = dynamic_price$dynamic_price[,1] 

# Inverting the forecast value to the original value
x_predict <- exp(x) - 1

# Adding predicted data to the original time series 
dynamic_price_inv <- ts(c(mts_df_filter_log_train[,1], x),
                        frequency = 12,
                        start = c(2001, 1),
                        end = c(2022, 12))

# visualizing the time series including all points 
plot(dynamic_price_inv)


## Creating an advanced plot with visual separation
# Converting to object zoo
x = zoo(dynamic_price_inv_df)

# Advanced xyplot from lattice
xyplot(x, grid=TRUE, panel = function(x, y, ...){
  panel.xyplot(x, y, col="red", ...)
  grid.clip(x = unit(36, "native"), just=c("right"))
  panel.xyplot(x, y, col="blue", ...) })

# getting back to the original values
df_init_test <- df_init[241:252, ]

# adding the forecasted values to the the test data frame
df_init_test$dyn_price_forecast <-x_predict

# calculating differences between dynamic prices
diff_dyn_prices_orig <- diff(df_init_test$dynamic_price)
diff_fixed_prices_orig <- diff(df_init_test$fixed_price)
diff_dyn_prices_forecast <- diff(df_init_test$dyn_price_forecast)

# gain losses
gains_dyn_prices <- sum(diff_dyn_prices_orig) # 0.08333333
gains_prices_orig <- sum(diff_fixed_prices_orig) # 0
gains_dyn_prices_forecast <- sum(diff_dyn_prices_forecast) # -0.07575947




