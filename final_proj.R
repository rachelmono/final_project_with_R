library(ggplot2)
library(tidyverse)
library(dslabs)
library('dplyr') 
library("varhandle")
library("fastDummies")
library(Hmisc)
library(randomForest)
#1. Access the Data Set
#read csv file in the defult saving path.
housing=read.csv("C:\\Users\\Rachel\\Desktop\\housing.csv", header = TRUE)
summary(housing)
head(housing)
#longitude latitude housing_median_age total_rooms total_bedrooms population households median_income
#1   -122.23    37.88                 41         880            129        322        126        8.3252
#2   -122.22    37.86                 21        7099           1106       2401       1138        8.3014
#median_house_value ocean_proximity
#1             452600        NEAR BAY
#2             358500        NEAR BAY

tail(housing, n=3)
#longitude latitude housing_median_age total_rooms total_bedrooms population households median_income
#20638   -121.22    39.43                 17        2254            485       1007        433        1.7000
#20639   -121.32    39.43                 18        1860            409        741        349        1.8672
#median_house_value ocean_proximity
#20638              92300          INLAND
#20639              84700          INLAND
summary(housing)
# longitude         latitude     housing_median_age  total_rooms    total_bedrooms     population   
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0   Min.   :    3  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0   1st Qu.:  787  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0   Median : 1166  
#Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9   Mean   : 1425  
#3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0   3rd Qu.: 1725  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0   Max.   :35682  
#NA's   :207                     
#   households     median_income     median_house_value ocean_proximity   
#Min.   :   1.0   Min.   : 0.4999   Min.   : 14999     Length:20640      
#1st Qu.: 280.0   1st Qu.: 2.5634   1st Qu.:119600     Class :character  
#Median : 409.0   Median : 3.5348   Median :179700     Mode  :character  
#Mean   : 499.5   Mean   : 3.8707   Mean   :206856                       
#3rd Qu.: 605.0   3rd Qu.: 4.7432   3rd Qu.:264725                       
#Max.   :6082.0   Max.   :15.0001   Max.   :500001 

# ---------------------------------------------------------------
#2.Data Visualization
par(mfrow = c(3, 3))
hist(housing$latitude,breaks=25, main="latitude", border="yellow", col="pink")
#the latitude are concentrated from 32.5 to 40
p2=hist(housing$longitude, breaks=25, main="longiture", border="yellow", col="pink")
#longitudes are concentrated on from 122.5 to 117.5
p3=hist(housing$housing_median_age, breaks=25, main="housing_median_age", border="yellow", col="pink")
#most housing age are from 0 to 50
p4=hist(housing$households, breaks=10, main="households", border="yellow", col="pink")
#most of the households are at the range of (0-1000)
p5=hist(housing$total_rooms,breaks=10, main="total_rooms", border="yellow", col="pink")
#most of the number of total rooms are less than 5000.
p6=hist(housing$total_bedrooms,breaks=10, main="total_bedrooms", border="yellow", col="pink")
#totol bedrooms are less than 1000.
p7=hist(housing$population,breaks=10, main="population", border="yellow", col="pink")
#the population is less than 5000
p8=hist(housing$median_income,breaks=25, main="median_income", border="yellow", col="pink")
#the median income at the range of (0,10)
p9=hist(housing$median_house_value,breaks=25, main="median_house_value", border="yellow", col="pink")
#the median house value are different, and there's a big 
#price differences.
# ---------------------------------------------------------------
#3.data transformation 
#1)impute missing data with median
sum(is.na(housing))
housing$total_bedrooms=impute(housing$total_bedrooms, median)  # median
summary(housing)

#2)make dummy variables into binary categorical variables and saving in the original dataframe
summary(housing_new)
housing_new=dummy_cols(housing,select_columns = "ocean_proximity")
names(housing_new)
housing_new=rename(housing_new,"NEAR BAY"="ocean_proximity_NEAR BAY",
       "<1H OCEAN"="ocean_proximity_<1H OCEAN",
       "INLAND"="ocean_proximity_INLAND", 
"NEAR OCEAN"="ocean_proximity_NEAR OCEAN",
       "ISLAND"="ocean_proximity_ISLAND")
names(housing_new)
#[1] "longitude"          "latitude"           "housing_median_age" "total_rooms"        "total_bedrooms"    
# "population"         "households"         "median_income"      "median_house_value" "ocean_proximity"   
#[11] "<1H OCEAN"          "INLAND"             "ISLAND"             "NEAR BAY"           "NEAR OCEAN"  
#reomve a variable from the data set.
housing=subset(housing_new, select=-ocean_proximity)
names(housing)
#[1] "longitude"          "latitude"           "housing_median_age" "total_rooms"       
#[5] "total_bedrooms"     "population"         "households"         "median_income"     
#[9] "median_house_value" "<1H OCEAN"          "INLAND"             "ISLAND"            
#[13] "NEAR BAY"           "NEAR OCEAN" 

#(3) add two more cols
mean_number_bedrooms=housing$total_bedrooms/housing$households
mean_number_rooms=housing$total_rooms/housing$households
housing$mean_number_bedrooms=mean_number_bedrooms
housing$mean_number_rooms=mean_number_rooms
#remove two cols
housing=subset(housing, select=c(-total_bedrooms,-total_rooms))
names(housing)
#[1] "longitude"            "latitude"             "housing_median_age"   "population"          
#[5] "households"           "median_income"        "median_house_value"   "<1H OCEAN"           
#[9] "INLAND"               "ISLAND"               "NEAR BAY"             "NEAR OCEAN"          
#[13] "mean_number_bedrooms" "mean_number_rooms"  

#scale numerica data expect median_house_value, so that ML will give equal weight.
housing$longitude=scale(housing$longitude, center=FALSE)
housing$latitude=scale(housing$latitude, center= FALSE)
housing$housing_median_age=scale(housing$housing_median_age, center=FALSE)
housing$mean_number_rooms=scale(housing$mean_number_rooms, center= FALSE)
housing$mean_number_bedrooms=scale(housing$mean_number_bedrooms, center=FALSE)
housing$population=scale(housing$population, center= FALSE)
housing$households=scale(housing$households, center=FALSE)
housing$median_income=scale(housing$households, center=FALSE)

#new data frame
cleaned_housing <- housing %>%
  select(c("NEAR BAY","<1H OCEAN","INLAND","NEAR OCEAN","ISLAND" ), everything())
names(cleaned_housing)
#[1] "NEAR BAY"             "<1H OCEAN"           
#[3] "INLAND"               "NEAR OCEAN"          
#[5] "ISLAND"               "longitude"           
#[7] "latitude"             "housing_median_age"  
#[9] "population"           "households"          
#[11] "median_income"        "median_house_value"  
#[13] "mean_number_bedrooms" "mean_number_rooms"  

# ---------------------------------------------------------------
#4.Create Training and Test sets
#random samples index for caleaned_housing
n=nrow(cleaned_housing)
ind=round(n*0.75)
set.seed(30000)

#(1) random sample index
train_ind=sample(n, ind)
#[1]  9700 19595 14760 16985  4145   761 15889 19253  2048  7021  4031 18478 10152  8294  5637 10811 13376  9435
#[19] 14298 10126 13451  3935 15582  6838  2266  1241  1061 18249 17902  3796 15767  6283  8074  4153 13972 20376
#[37] 11843  7042 17957 14522 17038  3656 17301  6455 10198  9978  6002  8947 18486 10349 13187 17954 15904   891
#......

#(2)
train_set=cleaned_housing[train_ind,]
count(train_set)
#n
#<int>
#  1 15480
#(3)
test_set=cleaned_housing[-train_ind,]
count(test_set)
#      n
#<int>
#  1  5160
# ---------------------------------------------------------------

#5.supervised Machine Learning - Regression
#install.packages("randomForest")
#(1)
train_x=subset(train_set, select=-median_house_value)
train_y=subset(train_set, select=median_house_value)
train_set=rename(train_set,'HOCEAN'='<1H OCEAN','NEARBAY'='NEAR BAY','NEAROCEAN'='NEAR OCEAN')
test_set=rename(test_set,'HOCEAN'='<1H OCEAN','NEARBAY'='NEAR BAY','NEAROCEAN'='NEAR OCEAN')
names(test_set)
names(train_set)
#[1] "NEARBAY"              "HOCEAN"               "INLAND"               "NEAROCEAN"           
#[5] "ISLAND"               "longitude"            "latitude"             "housing_median_age"  
#[9] "population"           "households"           "median_income"        "median_house_value"  
#[13] "mean_number_bedrooms" "mean_number_rooms"   

#(2)
rf <- randomForest(median_house_value~., data= train_set,ntree=500, 
                   importance=TRUE)
names(rf)
#[1] "call"            "type"            "predicted"       "mse"            
#[5] "rsq"             "oob.times"       "importance"      "importanceSD"   
#[9] "localImportance" "proximity"       "ntree"           "mtry"           
#[13] "forest"          "coefs"           "y"               "test"
#[17] "inbag"           "terms"  
rf$predicted
rf$type
rf$mse
rf$importance
#%IncMSE IncNodePurity
#NEARBAY              7.820044e+08  1.984155e+12
#HOCEAN               2.162289e+09  5.267285e+12
#INLAND               5.325498e+09  3.323284e+13
#NEAROCEAN            6.771718e+08  2.791034e+12
#ISLAND               4.033084e+04  1.091855e+10
#longitude            1.027350e+10  3.641039e+13
#latitude             8.784568e+09  3.187345e+13
#housing_median_age   1.150783e+09  9.955852e+12
#population           1.949089e+09  9.388019e+12
#households           1.349069e+09  7.232918e+12
#median_income        1.333809e+09  7.233855e+12
#mean_number_bedrooms 6.804147e+08  9.565251e+12
#mean_number_rooms    6.501965e+09  4.340727e+13


#Question 6
# Not specifying a data source forces OOB predictions
oob_prediction = predict(rf)
# Now compute the training set RMSE
a=(oob_prediction - train_y)^2
train_mse=mean(a$median_house_value)
oob_rmse = sqrt(train_mse)
oob_rmse
#[1] 51531.08


#6.(2)
names(test_set)
test_x=subset(test_set, select=-median_house_value)
test_y=subset(test_set, select=median_house_value)
y_pred = predict(rf , test_x)
# Now compute the test set RSME
b=(y_pred - test_y)^2
test_mse=mean(b$median_house_value)
test_rmse = sqrt(test_mse)
test_rmse
#[1] 50943.5

#From the results, we got two similiar numbers which means the model is good.
#the test set RMSE compare with the training set RMSE are close. The model is
#roughly the same on the training and testing data, suggesting that it makes good predictions.

#Thank you 