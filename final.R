#1. Access the Data Set
#read csv file in the defult saving path.
getwd()
housing=read.table("housing.csv",
           header=TRUE,
           sep=",")
housing=read.table("housing.csv")

head(housing)
tail(housing, n=3)
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

#2.Data Visualization
library(ggplot2)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(ggpubr)
p1=housing %>% ggplot(aes(x=latitude))+geom_histogram(fill="pink", col="yellow")
#the latitude are concentrated from 32.5 to 40
p2=housing %>% ggplot(aes(x=longitude))+geom_histogram(fill="pink", col="yellow")
#longitudes are concentrated on from 122.5 to 117.5
p3=housing %>% ggplot(aes(x=housing_median_age))+geom_histogram(fill="pink", col="yellow")
#most housing age are from 0 to 50
p4=hist(housing$longitude)

p5=hist(housing$households)
#
p6=hist(housing$total_rooms)
p7=hist(housing$total_bedrooms)
p8=hist(housing$population)
p9=hist(housing$median_income)
p10=hist(housing$median_house_value)


housing$total_bedrooms
impute(housing, target=character(0), cols=total_bedrooms)