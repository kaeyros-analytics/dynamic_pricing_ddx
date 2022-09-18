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
View(loaded_packages)


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



# 2. preparing data for modelization ####
#___________________________________

# inspect structure of the database
str(dataset_intern_xtern$month)

# remove unnecessary variables
input_data_init <- dataset_intern_xtern  %>%
  select(-dynamic_premium_rate, -localization_lat, -localization_lon,
         -month)

str(input_data_init)

# simulating a margin variable
temp_vector <- dataset_intern_xtern$dynamic_premium_rate

# changing the scale of the initial vector
profit_margin <- scales::rescale(temp_vector, to = c(0.1, 0.4))

# adding the profit margin to the data frame
input_data_init$profit_margin <- profit_margin


# converting all character columns to factors for Similarity calculation
temp <- input_data_init
temp2 <- as.data.frame(unclass(temp), stringsAsFactors = TRUE)
str(temp)

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

temp3 <- temp2 %>%
  dplyr::mutate(
    company_size_2 = case_when(
      company_size == "small" ~ 1,
      company_size == "middle" ~ 2,
      company_size == "large" ~ 3,
      TRUE ~ "Unknown"
    )
  )

check <- ifelse(temp2$company_size == "small", 1, 
                ifelse(temp2$company_size == "middle", 2, 
                       ifelse(temp2$company_size == "large", 3, 999)))


temp3 <- temp2
temp3$company_size <- NULL
temp3$company_size <- check
str(temp3)

# storing the transformed information into a new data frame
input_data_1 <- temp3


# 3. Scenario 1: hierarchical modeling   ####
#_____________________________________

# select a machine-type 
temp <- input_data_1 %>%
  dplyr::filter(machine_id == "M_001") %>%
  dplyr::select(-id, -machine_id, -date, -date_month) 

# plot --> nor so good
plot(log(temp$lot_health_index), log(temp$profit_margin))

# distribution clients
table(temp$client_id) # 228 data_rows for each client

# use firstly only few variables (to see whats happen with the model)
temp2 <- temp %>%
  dplyr::select(profit_margin, client_id, lot_health_index, fixed_premium_rate,
                avg_market_premium_rate, duration_customer_relation, 
                company_size, GPRH, DE_wind_national_current , 
                DE_pv_national_current, inflation_rate,
  )

clients <- levels(temp2$client_id)
nreg <- length(clients)

beta.ls <- matrix(NA, nrow = nreg, ncol = 10)
colnames(beta.ls) <-  c("Intercept", "lot_health_index", 
                        "log(fixed_premium_rate)", "log(avg_market_premium_rate)", 
                        "duration_customer_relation", "company_size", "log(GPRH)",
                        "log(DE_wind_national_current)", "log(DE_pv_national_current)", 
                        "inflation_rate")


# creating regression data (y = profit_margin, X = Matrix of Regressors)
regdata <-  NULL

for (i in 1:nreg){
  
  filter <- temp2$client_id == clients[i]
  y <- temp2$profit_margin
  X <- cbind(1, temp2$lot_health_index, log(temp2$fixed_premium_rate), 
             log(temp2$avg_market_premium_rate), temp2$duration_customer_relation,     
             temp2$company_size, log(temp2$GPRH), log(temp2$DE_wind_national_current), 
             log(temp2$DE_pv_national_current), temp2$inflation_rate)
  
  regdata[[i]] <- list(y=y, X=X)
}

# Find Hierarchical models estimators
Data <- list(regdata = regdata)
sim.size <- 2000
burn.in <- 1000
Mcmc <- list(R = sim.size)

set.seed(2008)
system.time(out <- bayesm::rhierLinearModel(Data = Data, Mcmc = Mcmc))

beta_draw <- out$betadraw[, 1, burn.in:sim.size]
int <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 2, burn.in:sim.size]
b_iot <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 3, burn.in:sim.size]
b_log_price <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 4, burn.in:sim.size]
b_log_avg_price <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 5, burn.in:sim.size]
b_duration_cust <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 6, burn.in:sim.size]
b_company_size <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 7, burn.in:sim.size]
b_log_GPRH <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 8, burn.in:sim.size]
b_log_wind_nat <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 9, burn.in:sim.size]
b_log_pv_nat <- round(apply(beta_draw, 1, mean), 6)

beta_draw <- out$betadraw[, 10, burn.in:sim.size]
b_inflation <- round(apply(beta_draw, 1, mean), 6)


# create beta_matrix for the client
beta_client <- cbind.data.frame(clients, int, b_iot, b_log_price, 
    b_log_avg_price, b_duration_cust, b_company_size, b_log_GPRH,
    b_log_wind_nat, b_log_pv_nat, b_inflation)



# 4. Scenario 2: Simulation dynamic price   ####
#________________________________________

df_preparation <- dataset_intern_xtern %>%
  mutate(date = as.Date(date , format = "%Y-%m-%d"),
         year_ = lubridate::year(date))

df_preparation$id <- as.character(df_preparation$id)

# change the string values from company_size to numeric
index_large <- which(df_preparation$company_size == "large")
index_middle <- which(df_preparation$company_size == "middle")
index_small <- which(df_preparation$company_size == "small")

df_preparation$company_size[index_large] <- 3
df_preparation$company_size[index_middle] <- 2
df_preparation$company_size[index_small] <- 1
df_preparation$company_size <- as.numeric(df_preparation$company_size)

str(df_preparation)

# select one client and one machine
df_client_machine <- df_preparation %>%
  filter(client_id == "client_0",
         machine_id == "M_001")

# remove unnecessary variable

df_subset <- df_client_machine %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon,
            date_month, month, N10 ,year_))


# Gradient Boosting model in R 



# regression

# Split the data into training and test set
#Randomly split the data into training set (80% for building a predictive model) and test set (20% for evaluating the model)
set.seed(123)

training_samples <- df_subset$dynamic_premium_rate %>% 
  caret::createDataPartition(p = 0.8, list = FALSE)

train_data  <- df_subset[training_samples, ]
test_data <- df_subset[-training_samples, ]

test_data_x <-test_data[, -3]
test_data_y <- test_data[,3]

# define the date for the plot
df_date <- df_client_machine %>% 
  select(date)
df_date_y <- df_date[-training_samples, ]


#train_data <- as.data.frame(scale(train_data))


# Boosted regression trees

set.seed(123)
model_classification <- caret::train(
  dynamic_premium_rate ~., data = train_data, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model_classification$bestTune
#    nrounds    max_depth eta  gamma colsample_bytree min_child_weight subsample
#36     150         2     0.3     0              0.8                1         1

# Make predictions on the test data
predicted_classes <- model_classification %>% predict(test_data_x)
head(predicted_classes, 50)
head(df_subset$dynamic_premium_rate, 50)

# Compute model prediction accuracy rate
mean(predicted_classes == test_data$dynamic_premium_rate)

# visualize original and predicted test data in a plot.

df <- data.frame(test_data_y, predicted_classes)
#df$id <- 1:nrow(df)
df$date <-  df_date_y

theme_set(theme_bw())
p <- ggplot() + geom_line(data = df, aes(x = date, y = test_data_y, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = predicted_classes, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with Gradient Boosting model") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p)

# The function varImp() [in caret] displays the importance of variables in percentage:
caret::varImp(model_classification)

# xgbTree variable importance
# 
# only 20 most important variables shown (out of 29)
# 
# Overall
# lot_health_index                 1.000e+02
# SHAREH_CAT_3                     1.263e+00
# DE_wind_onshore_near_termfuture  3.943e-01
# N3H                              2.745e-01
# GPRH_AND                         8.761e-02
# SHAREH_CAT_8                     8.166e-02
# SHAREH_CAT_7                     7.868e-02
# avg_market_premium_rate          6.986e-02
# GPRH_BASIC                       6.833e-02
# GPRHT                            5.757e-02
# GPRH_NOEW                        1.352e-02
# DE_pv_national_current           1.259e-02
# SHAREH_CAT_6                     1.158e-02
# SHAREH_CAT_2                     1.116e-02
# inflation_rate                   8.621e-03
# GPRH                             5.619e-03
# DE_wind_national_long_termfuture 3.782e-03
# DE_wind_offshore_current         2.629e-03
# SHARE_GPRH                       2.119e-03
# SHAREH_CAT_4                     1.834e-03


# Compute the average prediction error RMSE
#RMSE(predicted_classes, test_data$dynamic_premium_rate)
residuals1 = test_data - predicted_classes
RMSE1 = sqrt(mean(residuals1^2))
cat('residual', round(residuals1,3),'\n','The root mean square error of the test data is ', round(RMSE1,3),'\n')

# df_pred <- df_preparation %>% 
#   select(-dynamic_premium_rate)
# 
# # Make predictions on the whole data
# predicted_classes11 <- model_classification %>% predict(df_pred)
# head(predicted_classes11)
# # compare with the original dynamic_premium_rate
# head(df_preparation$dynamic_premium_rate)


# XGBoost model in R:

library(xgboost) #for fitting the xgboost model
library(caret)   #for general data preparation and model fitting

# select one client and one machine
df_client_machine <- df_preparation %>%
  filter(client_id == "client_0",
         machine_id == "M_001")

df_subset <- df_client_machine %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))

#split into training (80%) and testing set (20%)
df_parts <- caret::createDataPartition(df_subset$lot_health_index, p = .8, list = F)
df_train <- df_subset[df_parts, ]
df_test <- df_subset[-df_parts, ]

#define predictor and response variables in training set
df_ttrain_x <- data.matrix(df_train[, -3])
df_ttrain_y <- df_train[,3]

#define predictor and response variables in testing set
df_ttest_x <- base::data.matrix(df_test[, -3])
df_ttest_y <- df_test[, 3]

# define the date for the plot
df_date <- df_client_machine %>% 
  select(date)
df_date_y <- df_date[-df_parts, ]

#define final training and testing sets
xgb_df_train <- xgboost::xgb.DMatrix(data = df_ttrain_x, label = df_ttrain_y)
xgb_df_test <- xgboost::xgb.DMatrix(data = df_ttest_x, label = df_ttest_y)

#define watchlist
watchlist1 <- list(train=xgb_df_train, test=xgb_df_test)

#fit XGBoost model and display training and testing data at each round
model_XGBoost <- xgboost::xgb.train(data = xgb_df_train, max.depth = 3, watchlist=watchlist1, nrounds = 70)

# Thus, we’ll define our final XGBoost model to use 14 rounds:

#define final model
model_final <- xgboost(data = xgb_df_train, max.depth = 3, nrounds = 14, verbose = 0)

#use model to make predictions on test data
predict_y <- predict(model_final, xgb_df_test)

# visualize original and predicted test data in a plot.

df <- data.frame(df_ttest_y, predict_y)
#df$id <- 1:nrow(df)
df$date <-  df_date_y

theme_set(theme_bw())
p <- ggplot() + geom_line(data = df, aes(x = date, y = df_ttest_y, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = predict_y, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with XGBoost model") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p)


# gradient boosting in R for regression

library(gbm)
library(caret)


# select one client and one machine
df_client_machine <- df_preparation %>%
  filter(client_id == "client_0",
         machine_id == "M_001")

# remove unnecessary variable

df_subset <- df_client_machine %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))

# Step 1 - Train and Test data
set.seed(0)           # set seed for generating random data.

# createDataPartition() function from the caret package to split the original dataset into a training and testing set 
# and split data into training (80%) and testing set (20%)
parts2 <- createDataPartition(df_subset$lot_health_index, p = .8, list = F)
train2 <- df_subset[parts2, ]
test2 <- df_subset[-parts2, ]

test_x2 <- test2[, -3] # feature and target array
test_y2 <- test2[, 3]

# define the date for the plot
df_date <- df_client_machine %>% 
  select(date)
df_date_y <- df_date[-parts2, ]

# Step 2 - Create a gbm model
# Now, we will fit and train our model using the gbm() function with gaussiann distribution
model_gbm = gbm::gbm(train2$dynamic_premium_rate ~.,
                     data = train2,
                     distribution = "gaussian",
                     cv.folds = 10,
                     shrinkage = .01,
                     n.minobsinnode = 10,
                     n.trees = 500)

print(model_gbm)

summary(model_gbm)

# var      rel.inf
# lot_health_index                                 lot_health_index 99.957134756
# SHAREH_CAT_4                                         SHAREH_CAT_4  0.018962378
# GPRHT                                                       GPRHT  0.011921573
# N3H                                                           N3H  0.005998975
# GPRH_NOEW                                               GPRH_NOEW  0.005982318
# fixed_premium_rate                             fixed_premium_rate  0.000000000
# avg_market_premium_rate                   avg_market_premium_rate  0.000000000
# company_size                                         company_size  0.000000000
# duration_customer_relation             duration_customer_relation  0.000000000
# GPRH                                                         GPRH  0.000000000
# GPRHA                                                       GPRHA  0.000000000
# SHARE_GPRH                                             SHARE_GPRH  0.000000000
# GPRH_AND                                                 GPRH_AND  0.000000000
# GPRH_BASIC                                             GPRH_BASIC  0.000000000
# SHAREH_CAT_1                                         SHAREH_CAT_1  0.000000000
# SHAREH_CAT_2                                         SHAREH_CAT_2  0.000000000
# SHAREH_CAT_3                                         SHAREH_CAT_3  0.000000000
# SHAREH_CAT_5                                         SHAREH_CAT_5  0.000000000
# SHAREH_CAT_6                                         SHAREH_CAT_6  0.000000000
# SHAREH_CAT_7                                         SHAREH_CAT_7  0.000000000
# SHAREH_CAT_8                                         SHAREH_CAT_8  0.000000000
# inflation_rate                                     inflation_rate  0.000000000
# DE_pv_national_current                     DE_pv_national_current  0.000000000
# DE_wind_national_current                 DE_wind_national_current  0.000000000
# DE_wind_offshore_current                 DE_wind_offshore_current  0.000000000
# DE_wind_national_long_termfuture DE_wind_national_long_termfuture  0.000000000
# DE_wind_national_near_termfutur   DE_wind_national_near_termfutur  0.000000000
# DE_wind_offshore_near_termfuture DE_wind_offshore_near_termfuture  0.000000000
# DE_wind_onshore_near_termfuture   DE_wind_onshore_near_termfuture  0.000000000

#Step 3 - Make predictions on the test dataset
#We use our model_gbm model to make predictions on the testing data (unseen data) and predict the 'dynamic_premium_rate' 
#value and generate performance measures.
pred_yy <- gbm::predict.gbm(model_gbm, test_x2)
head(pred_yy, 50)

# visualize original and predicted test data in a plot.

df <- data.frame(test_y2, pred_yy)
#df1$id <- 1:nrow(df1)
df$date <-  df_date_y

theme_set(theme_bw())
p2 <- ggplot() + geom_line(data = df, aes(x = date, y = test_y2, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = pred_yy, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with gradient boosting for regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p2)

#Step 4 - Check the accuracy of our model
residuals <- test_y2 - pred_yy
RMSE <- sqrt(mean(residuals^2))
#cat('The root mean square error of the test data is ', round(RMSE,3),'\n')

y_test_mean <- mean(test_y2)
# Calculate total sum of squares
tss <-  sum((test_y2 - y_test_mean)^2 )
# Calculate residual sum of squares
rss <-  sum(residuals^2)
# Calculate R-squared
rsq  <-  1 - (rss/tss)
#cat('The R-square of the test data is ', round(rsq,3), '\n')

cat("Mean: ", y_test_mean, "\nsum of squares: ", tss, "\nresidual sum of squares: ", rss, "\nR-squared: ", rsq,
    "\nRMSE: ", RMSE)

# LightGBM Regression
#____________________

# https://www.datatechnotes.com/2022/04/lightgbm-regression-example-in-r.html

install.packages("lightgbm")

library(lightgbm)
library(caret)
library(ggplot2) 

# select one client and one machine
df_client_machine <- df_preparation %>%
  filter(client_id == "client_0",
         machine_id == "M_001")

# remove unnecessary variable
df_subset <- df_client_machine %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))

# step 1 Preparing the data
# createDataPartition() function from the caret package to split the original dataset into a training and testing set 
# and split data into training (85%) and testing set (15%)
set.seed(12)

indexes <- createDataPartition(df_subset$lot_health_index, p = .80, list = F)
train <- df_subset[indexes, ]
test <- df_subset[-indexes, ]
#test1 <- df_subset154[-indexes, ]

train_x <- train[, -3]
train_x <- scale(train_x)[,]
train_y <- train[,3]


test_x <- test[, -3]
test_x <- scale(test[,-3])[,]
test_y <- test[,3]

# define the date for the plot
df_date <- df_client_machine %>% 
  select(date)
df_date_y <- df_date[-indexes, ]

# load the train and test data into the LightGBM dataset object
dtrain <- lightgbm::lgb.Dataset(train_x, label = train_y)

# Construct validation data with lgb.Dataset.create.valid
dtest <- lightgbm ::lgb.Dataset.create.valid(dtrain, test_x, label = test_y)

# define regression parameters model
params = list(
  objective = "regression"
  , metric = "l2"
  , min_data = 1L
  , learning_rate = .3
)

# validataion data with defined parameters
valids = list(test = dtest)

# train model with defined parameters 
model = lightgbm::lgb.train(
  params = params
  , data = dtrain
  , nrounds = 5L
  , valids = valids
)

# Get record evaluation result from booster
# check L2 values for test dataset. It show 5 round outputs of L2.
lightgbm::lgb.get.eval.result(model, "test", "l2")
# [1] 0.020190023 0.010281055 0.005574870 0.003373286 0.002367622

# prediction the x test data with the trained model
pred_y = predict(model, test_x) 
head(pred_y)

# visualize original and predicted test data in a plot.

df <- data.frame(test_y, pred_y)
#df1$id <- 1:nrow(df1)
df$date <-  df_date_y

theme_set(theme_bw())
p3 <- ggplot() + geom_line(data = df, aes(x = date, y = test_y, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = pred_y, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with LightGBM Regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p3)

# check the prediction accuracy with MSE, MAE, and RMSE metrics
mse <- mean((test_y - pred_y)^2)
mae <- caret::MAE(test_y, pred_y)
rmse <- caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "\nMAE: ", mae, "\nRMSE: ", rmse)
# MSE:  0.001902606 
# MAE:  0.03636031 
# RMSE:  0.04361887

# Compute feature importance in a model
# find the top 5 important features of training data and visualize it in a graph
tree_imp <- lightgbm::lgb.importance(model, percentage = TRUE)
lightgbm::lgb.plot.importance(tree_imp, top_n = 5L, measure = "Gain")


