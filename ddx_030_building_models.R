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
library(e1071)
library(lightgbm)
library(caret)
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



# 2. preparing data for modelization ####
#__________________________________

glimpse(dataset_intern_xtern)
 # 2 int, 35 dbl, 4 chr 
summary(dataset_intern_xtern)

# Dealing with Incorrect Entries

# Missing Value Treatment
sapply(dataset_intern_xtern, function(x) sum(is.na(x)))

# Encoding Categorical Labels
dataset_intern_xtern$company_size <- as.factor(dataset_intern_xtern$company_size)
dataset_intern_xtern$company_size <-unclass(dataset_intern_xtern$company_size)


# Handling Outliers
skewness(dataset_intern_xtern$dynamic_premium_rate)
# not Outliers 

# Logarithmic Transformation of Numerical Variables
#dataset_intern_xtern$Logdynamic_premium_rate = log(dataset_intern_xtern$dynamic_premium_rate)
#skewness(dataset_intern_xtern$Logdynamic_premium_rate)
table(dataset_intern_xtern$duration_customer_relation)
table(dataset_intern_xtern$N10)

## select data 
dataset_intern_xtern_3<- dataset_intern_xtern %>% 
  select(-c(client_id, machine_id, date,N10,id ))
## correlation analyse
df_mod <- mutate_all(dataset_intern_xtern_3, function(x) as.numeric(as.character(x)))
#dataset_intern_xtern_3[is.na(dataset_intern_xtern_3)] <- 0
#mydata.cor <- cor(df2, method = "pearson", use = "complete.obs")





#write.csv(df2,"C:\\Users\\user\\Documents\\use_case\\People.csv", row.names = FALSE)

# Standardization
# Apply scale function
df_mod<- df_mod %>% 
  select(-c(localization_lat, localization_lon, month,date_month))

df2 <- df_mod%>%
  select(-c(DE_wind_offshore_current,DE_wind_national_long_termfuture,
            DE_wind_national_near_termfutur,DE_wind_offshore_near_termfuture,
            DE_wind_onshore_near_termfuture,SHARE_GPRH,SHAREH_CAT_2,SHAREH_CAT_3,
            SHAREH_CAT_4,SHAREH_CAT_5,SHAREH_CAT_6,SHAREH_CAT_7,SHAREH_CAT_8,
            GPRHT,GPRHA,GPRH_NOEW,GPRH_AND,GPRH_BASIC))




# 3. modeling dynamic pricing  (client-machine-view) ####
#___________________________________________________

##Regression linear

df2_scale <- as.data.frame(scale(df2))

head(df2_scale)

# Run the linear model and save it as 'mod'
mod <- lm(dynamic_premium_rate ~. , data = df2_scale)
# let's view the output:
summary(mod)

  
#
#define new dynamic_price
new <- data.frame(lot_health_index=c(20), fixed_price=c(5), avg_market_premium_price=c(2))

#use the fitted model to predict the rating for the new player
predict(mod, newdata=new)

#######

# lightgbm all data


set.seed(12)

#we'll split them into the train and test parts, and extract x-input and
#y-label parts. Here, we'll extract 15 percent of the dataset as test data.
#It is better to scale x part of data to improve the accuracy.
indexes = createDataPartition(df_mod$company_size, p = .85, list = F)
train = df_mod[indexes, ]
test = df_mod[-indexes, ]

train_x = train[, -3]
train_x = scale(train_x)[,]
train_y = train[,3]

test_x = test[, -3]
test_x = scale(test[,-3])[,]
test_y = test[,3]


#load the train and test data into the LightGBM dataset object.
dtrain = lgb.Dataset(train_x, label = train_y)
dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)

# define parameters
params = list(
  objective = "regression"
  , metric = "l2"
  , min_data = 1L
  , learning_rate = .3
)

# validataion data
valids = list(test = dtest)

# train model 
model = lgb.train(
  params = params
  , data = dtrain
  , nrounds = 5L
  , valids = valids
  #,force_col_wise=TRUE
)
#We can check L2 values for test dataset. It show 5 round outputs of L2.
##lgb.get.eval.result(model, "test", "l2")

# prediction
pred_y = predict(model, test_x)


# accuracy check
mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "\nMAE: ", mae, "\nRMSE: ", rmse)

#visualize original and predicted test data in a plot.

# visualize the result in a plot
df = data.frame(test_y, pred_y)
df$id = 1:nrow(df)

ggplot() + geom_line(data = df, aes(x = id, y = test_y, color = 'test_y')) + 
  geom_line(data = df, aes(x=id, y = pred_y, color = 'pred_y')) +
  ggtitle("dynamic Pricing rate") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('prices')



# feature importance
tree_imp = lgb.importance(model, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 5L, measure = "Gain")


# lightgbm client, machine
df_client_machine <- dataset_intern_xtern %>%
  filter(client_id == "client_0",
         machine_id == "M_001")
df_client_machine





## decision tree r






# 4. modeling dynamic pricing  (machine-view) ####
#_____________________________________________




