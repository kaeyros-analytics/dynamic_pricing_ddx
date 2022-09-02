library("RSQLite")
library("tidyverse")
library("dplyr")
library(ggplot2)
library(broom)
library(ggpubr)

source('read_data.R')

## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), dbname="generated_db.sqlite3")

## list all tables
tables <- dbListTables(con)

## exclude sqlite_sequence (contains table information)
tables <- tables[tables != "sqlite_sequence"]

lDataFrames <- vector("list", length=length(tables))

## create a data.frame for each table
for (i in seq(along=tables)) {
  lDataFrames[[i]] <- dbGetQuery(conn=con, statement=paste("SELECT * FROM '", tables[[i]], "'", sep=""))
}
dbDisconnect()

## extract dataframe
ddx_dat = lDataFrames[[1]]
head(ddx_dat)

# data processing
ddx_dat<- ddx_dat %>% 
  mutate(month = strftime(date,"%m"))%>% 
  mutate(year = strftime(date,"%Y"))


ddx_dat_an <-ddx_dat%>%
  dplyr::select(lot_health_index, fixed_price, avg_market_premium_price,dynamic_price)

## Modelisation

ddt <- merge(ddx_dat_an, infl_dat_1, by = "month") 

##Check for linearity
#plot(dynamic_price ~ height_ft + data = ddx_dat_an, pch=16)


# Run the linear model and save it as 'mod'
mod <- lm(dynamic_price ~lot_health_index + fixed_price + avg_market_premium_price , data = ddx_dat_an)
# let's view the output:
summary(mod)


#define new dynamic_price
new <- data.frame(lot_health_index=c(20), fixed_price=c(5), avg_market_premium_price=c(2))

#use the fitted model to predict the rating for the new player
predict(mod, newdata=new)
