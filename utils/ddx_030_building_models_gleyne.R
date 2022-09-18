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
#View(loaded_packages)


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

dbDisconnect(con)

# 2. preparing data for modelization ####
#___________________________________

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



# 3. modeling dynamic pricing  (client-machine-view) ####
#___________________________________________________

# select one client and one machine
df_client_machine <- df_preparation %>%
  filter(client_id == "client_0",
         machine_id == "M_001")

# remove unnecessary variable

df_subset <- df_client_machine %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))


# Gradient Boosting model in R 
#______________________________

# Loading required R packages

library(tidyverse) # for easy data manipulation and visualization
library(caret) # for easy machine learning workflow
library(xgboost) # for computing boosting algorithm

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
head(predicted_classes)
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
#____________________

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
#_______________________________________

# https://www.projectpro.io/recipes/apply-gradient-boosting-r-for-regression


install.packages('gbm')                    # for fitting the gradient boosting model

#install.packages('caret')       # for general data preparation and model fitting

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




# 4. modeling dynamic pricing  (machine-view) ####
#_____________________________________________

# select one client and one machine
df_machine_003 <- df_preparation %>%
  filter(machine_id == "M_003")

# remove unnecessary variable
df_subset_003 <- df_machine_003 %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))

# Gradient Boosting model in R 
#______________________________

# Loading required R packages
library(tidyverse) # for easy data manipulation and visualization
library(caret) # for easy machine learning workflow
library(xgboost) # for computing boosting algorithm

# regression

# Split the data into training and test set
#Randomly split the data into training set (80% for building a predictive model) and test set (20% for evaluating the model)
set.seed(123)

training_samples <- df_subset_003$dynamic_premium_rate %>% 
  caret::createDataPartition(p = 0.8, list = FALSE)

train_data  <- df_subset_003[training_samples, ]
test_data <- df_subset_003[-training_samples, ]


test_data_x <-test_data[, -3]
test_data_y <- test_data[,3]

# define the date for the plot
df_date <- df_machine_003 %>% 
  select(date)
df_date_y <- df_date[-training_samples, ]


# Boosted regression trees

set.seed(123)
model_classification <- caret::train(
  dynamic_premium_rate ~., data = train_data, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)
# Best tuning parameter
model_classification$bestTune
#     nrounds   max_depth eta  gamma colsample_bytree min_child_weight subsample
#108     150         3    0.4     0              0.8                1         

# Make predictions on the test data
predicted_classes <- model_classification %>% predict(test_data_x)
head(predicted_classes)
#head(df_subset_003$dynamic_premium_rate)

# prediction the x test data with the trained model
# pred_y = predict(model, test_x) 
# head(pred_y)

# visualize original and predicted test data in a plot.

df <- data.frame(test_data_y, predicted_classes)
#df1$id <- 1:nrow(df1)
df$date <-  df_date_y

theme_set(theme_bw())
p4 <- ggplot() + geom_line(data = df, aes(x = date, y = test_data_y, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = predicted_classes, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with gradient Boosting model") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p4)


# Compute model prediction accuracy rate
mean(predicted_classes == test_data$dynamic_premium_rate)

# The function varImp() [in caret] displays the importance of variables in percentage:
caret::varImp(model_classification)
# xgbTree variable importance
# 
# only 20 most important variables shown (out of 29)
# 
# Overall
# fixed_premium_rate              1.000e+02
# avg_market_premium_rate         3.031e+01
# duration_customer_relation      9.215e-03
# lot_health_index                3.317e-03
# company_size                    1.979e-03
# N3H                             3.013e-04
# GPRHA                           4.261e-05
# inflation_rate                  2.938e-05
# DE_wind_national_current        2.887e-05
# SHAREH_CAT_8                    1.650e-05
# GPRH_NOEW                       1.383e-05
# DE_wind_national_near_termfutur 1.015e-05
# SHAREH_CAT_5                    7.110e-06
# SHAREH_CAT_6                    6.576e-06
# GPRH_AND                        1.971e-06
# SHAREH_CAT_7                    1.892e-06
# DE_wind_offshore_current        6.417e-07
# SHARE_GPRH                      5.810e-07
# GPRH_BASIC                      4.890e-07
# SHAREH_CAT_4                    4.548e-07


# Compute the average prediction error RMSE
RMSE(predicted_classes, test_data$dynamic_premium_rate)
# [1] 0.09260527


# XGBoost model in R:
#____________________

library(xgboost) #for fitting the xgboost model
library(caret)   #for general data preparation and model fitting

# select  one machine
df_machine_003 <- df_preparation %>%
  filter(machine_id == "M_003")

# remove unnecessary variable
df_subset_003 <- df_machine_003 %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))

#split into training (80%) and testing set (20%)
df_parts <- caret::createDataPartition(df_subset_003$lot_health_index, p = .8, list = F)
df_train <- df_subset_003[df_parts, ]
df_test <- df_subset_003[-df_parts, ]

#define predictor and response variables in training set
df_ttrain_x <- data.matrix(df_train[, -3])
df_ttrain_y <- df_train[,3]

#define predictor and response variables in testing set
df_ttest_x <- base::data.matrix(df_test[, -3])
df_ttest_y <- df_test[,3]

# define the date for the plot
df_date <- df_machine_003 %>% 
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
head(predict_y)

df <- data.frame(df_ttest_y, predict_y)
#df$id <- 1:nrow(df)
df$date <-  df_date_y

theme_set(theme_bw())
p5 <- ggplot() + geom_line(data = df, aes(x = date, y = df_ttest_y, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = predict_y, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with XGBoost model") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p5)


# gradient boosting in R for regression
#_______________________________________

# https://www.projectpro.io/recipes/apply-gradient-boosting-r-for-regression


install.packages('gbm')                    # for fitting the gradient boosting model

#install.packages('caret')       # for general data preparation and model fitting

library(gbm)
library(caret)


# select  one machine
df_machine_003 <- df_preparation %>%
  filter(machine_id == "M_003")

# remove unnecessary variable
df_subset_003 <- df_machine_003 %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))


# Step 1 - Train and Test data
set.seed(0)           # set seed for generating random data.

# createDataPartition() function from the caret package to split the original dataset into a training and testing set 
# and split data into training (80%) and testing set (20%)
parts2 <- createDataPartition(df_subset_003$lot_health_index, p = .8, list = F)
train2 <- df_subset_003[parts2, ]
test2 <- df_subset_003[-parts2, ]

test_x2 <- test2[, -3] # feature and target array
test_y2 <- test2[, 3]

# define the date for the plot
df_date <- df_machine_003 %>% 
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

# var   rel.inf
# fixed_premium_rate                             fixed_premium_rate 91.120888
# avg_market_premium_rate                   avg_market_premium_rate  8.879112
# lot_health_index                                 lot_health_index  0.000000
# company_size                                         company_size  0.000000
# duration_customer_relation             duration_customer_relation  0.000000
# GPRH                                                         GPRH  0.000000
# GPRHT                                                       GPRHT  0.000000
# GPRHA                                                       GPRHA  0.000000
# SHARE_GPRH                                             SHARE_GPRH  0.000000
# N3H                                                           N3H  0.000000
# GPRH_NOEW                                               GPRH_NOEW  0.000000
# GPRH_AND                                                 GPRH_AND  0.000000
# GPRH_BASIC                                             GPRH_BASIC  0.000000
# SHAREH_CAT_1                                         SHAREH_CAT_1  0.000000
# SHAREH_CAT_2                                         SHAREH_CAT_2  0.000000
# SHAREH_CAT_3                                         SHAREH_CAT_3  0.000000
# SHAREH_CAT_4                                         SHAREH_CAT_4  0.000000
# SHAREH_CAT_5                                         SHAREH_CAT_5  0.000000
# SHAREH_CAT_6                                         SHAREH_CAT_6  0.000000
# SHAREH_CAT_7                                         SHAREH_CAT_7  0.000000
# SHAREH_CAT_8                                         SHAREH_CAT_8  0.000000
# inflation_rate                                     inflation_rate  0.000000
# DE_pv_national_current                     DE_pv_national_current  0.000000
# DE_wind_national_current                 DE_wind_national_current  0.000000
# DE_wind_offshore_current                 DE_wind_offshore_current  0.000000
# DE_wind_national_long_termfuture DE_wind_national_long_termfuture  0.000000
# DE_wind_national_near_termfutur   DE_wind_national_near_termfutur  0.000000
# DE_wind_offshore_near_termfuture DE_wind_offshore_near_termfuture  0.000000
# DE_wind_onshore_near_termfuture   DE_wind_onshore_near_termfuture  0.000000

#Step 3 - Make predictions on the test dataset
#We use our model_gbm model to make predictions on the testing data (unseen data) and predict the 'dynamic_premium_rate' 
#value and generate performance measures.
pred_yy <- gbm::predict.gbm(model_gbm, test_x2)
head(pred_yy)

# visualize original and predicted test data in a plot.

df <- data.frame(test_y2, pred_yy)
#df1$id <- 1:nrow(df1)
df$date <-  df_date_y

theme_set(theme_bw())
p5 <- ggplot() + geom_line(data = df, aes(x = date, y = test_y2, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = pred_yy, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with gradient boosting for regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p5)

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

# gradient boosting in R for regression
#_______________________________________

# https://www.projectpro.io/recipes/apply-gradient-boosting-r-for-regression


install.packages('gbm')                    # for fitting the gradient boosting model

#install.packages('caret')       # for general data preparation and model fitting

library(gbm)
library(caret)

# select  one machine
df_machine_003 <- df_preparation %>%
  filter(machine_id == "M_003")

# remove unnecessary variable
df_subset_003 <- df_machine_003 %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))

# Step 1 - Train and Test data
set.seed(0)           # set seed for generating random data.

# createDataPartition() function from the caret package to split the original dataset into a training and testing set 
# and split data into training (80%) and testing set (20%)
parts2 <- createDataPartition(df_subset_003$lot_health_index, p = .8, list = F)
train2 <- df_subset_003[parts2, ]
test2 <- df_subset_003[-parts2, ]

test_x2 <- test2[, -3] # feature and target array
test_y2 <- test2[, 3]

# define the date for the plot
df_date <- df_machine_003 %>% 
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

# var   rel.inf
# fixed_premium_rate                             fixed_premium_rate 91.120888
# avg_market_premium_rate                   avg_market_premium_rate  8.879112
# lot_health_index                                 lot_health_index  0.000000
# company_size                                         company_size  0.000000
# duration_customer_relation             duration_customer_relation  0.000000
# GPRH                                                         GPRH  0.000000
# GPRHT                                                       GPRHT  0.000000
# GPRHA                                                       GPRHA  0.000000
# SHARE_GPRH                                             SHARE_GPRH  0.000000
# N3H                                                           N3H  0.000000
# GPRH_NOEW                                               GPRH_NOEW  0.000000
# GPRH_AND                                                 GPRH_AND  0.000000
# GPRH_BASIC                                             GPRH_BASIC  0.000000
# SHAREH_CAT_1                                         SHAREH_CAT_1  0.000000
# SHAREH_CAT_2                                         SHAREH_CAT_2  0.000000
# SHAREH_CAT_3                                         SHAREH_CAT_3  0.000000
# SHAREH_CAT_4                                         SHAREH_CAT_4  0.000000
# SHAREH_CAT_5                                         SHAREH_CAT_5  0.000000
# SHAREH_CAT_6                                         SHAREH_CAT_6  0.000000
# SHAREH_CAT_7                                         SHAREH_CAT_7  0.000000
# SHAREH_CAT_8                                         SHAREH_CAT_8  0.000000
# inflation_rate                                     inflation_rate  0.000000
# DE_pv_national_current                     DE_pv_national_current  0.000000
# DE_wind_national_current                 DE_wind_national_current  0.000000
# DE_wind_offshore_current                 DE_wind_offshore_current  0.000000
# DE_wind_national_long_termfuture DE_wind_national_long_termfuture  0.000000
# DE_wind_national_near_termfutur   DE_wind_national_near_termfutur  0.000000
# DE_wind_offshore_near_termfuture DE_wind_offshore_near_termfuture  0.000000
# DE_wind_onshore_near_termfuture   DE_wind_onshore_near_termfuture  0.000000

#Step 3 - Make predictions on the test dataset
#We use our model_gbm model to make predictions on the testing data (unseen data) and predict the 'dynamic_premium_rate' 
#value and generate performance measures.
pred_yy <- gbm::predict.gbm(model_gbm, test_x2)
head(pred_yy)

# visualize original and predicted test data in a plot.

df <- data.frame(test_y2, pred_yy)
#df1$id <- 1:nrow(df1)
df$date <-  df_date_y

theme_set(theme_bw())
p6 <- ggplot() + geom_line(data = df, aes(x = date, y = test_y2, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = pred_yy, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with gradient boosting for regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p6)

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

# Mean:  173.9874 
# sum of squares:  7710999 
# residual sum of squares:  60494.41 
# R-squared:  0.9921548 
# RMSE:  3.642695

# LightGBM Regression
#____________________

# https://www.datatechnotes.com/2022/04/lightgbm-regression-example-in-r.html

install.packages("lightgbm")

library(lightgbm)
library(caret)
library(ggplot2) 

# select  one machine
df_machine_003 <- df_preparation %>%
  filter(machine_id == "M_003")

# remove unnecessary variable
df_subset_003 <- df_machine_003 %>% 
  select(-c(id, client_id, machine_id, date, localization_lat, localization_lon, date_month, month, N10 ,year_))

# step 1 Preparing the data
# createDataPartition() function from the caret package to split the original dataset into a training and testing set 
# and split data into training (85%) and testing set (15%)
set.seed(12)

indexes <- createDataPartition(df_subset_003$lot_health_index, p = .80, list = F)
train <- df_subset_003[indexes, ]
test <- df_subset_003[-indexes, ]
#test1 <- df_subset154[-indexes, ]

train_x <- train[, -3]
train_x <- scale(train_x)[,]
train_y <- train[,3]


test_x <- test[, -3]
test_x <- scale(test[,-3])[,]
test_y <- test[,3]

# define the date for the plot
df_date <- df_machine_003 %>% 
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
# [1] 822.77410 404.03606 198.60115  98.18397  48.59056

# prediction the x test data with the trained model
pred_y = predict(model, test_x) 
head(pred_y)

# visualize original and predicted test data in a plot.

df <- data.frame(test_y, pred_y)
#df1$id <- 1:nrow(df1)
df$date <-  df_date_y

theme_set(theme_bw())
p7 <- ggplot() + geom_line(data = df, aes(x = date, y = test_y, color = 'Premium rate')) + 
  geom_line(data = df, aes(x=date, y = pred_y, color = 'Predict premium rate')) +
  ggtitle("Prediction the dynamic premium rate with LightGBM Regression") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Dynamic premium rate') +
  xlab("Date")

plotly::ggplotly(p7)

# check the prediction accuracy with MSE, MAE, and RMSE metrics
mse <- mean((test_y - pred_y)^2)
mae <- caret::MAE(test_y, pred_y)
rmse <- caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "\nMAE: ", mae, "\nRMSE: ", rmse)
# MSE:  48.59056 
# MAE:  5.83107 
# RMSE:  6.970693

# Compute feature importance in a model
# find the top 5 important features of training data and visualize it in a graph
tree_imp <- lightgbm::lgb.importance(model, percentage = TRUE)
lightgbm::lgb.plot.importance(tree_imp, top_n = 5L, measure = "Gain")


