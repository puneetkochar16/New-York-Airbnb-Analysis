
library(ggplot2)
library(tidyverse)
library(dplyr)

#### Reading the File ####
setwd("D:\\Ms In Business Analytics\\Spring 2020\\Data Visualization\\Project\\AB_NYC_2019.csv (1)")
nyAirbnb = read.csv("AB_NYC_2019.csv")


##########size and structure of data######################
dim(nyAirbnb)
names(nyAirbnb)
str(nyAirbnb)
attributes(nyAirbnb)
head(nyAirbnb)
summary(nyAirbnb)
length(unique(nyAirbnb$id))

########### Checking for Missing-Values##############
sapply(nyAirbnb, function(x) sum(is.na(x))) # missing values in each column

# only reviews per month column have the missing values
# I will replace missing values for reveiws per month as 0 on assumption that a property can have 0 reviewers.
nyAirbnb$reviews_per_month[is.na(nyAirbnb$reviews_per_month)] = 0 #replacing missing values
sum(is.na(nyAirbnb))

## Distribution of Data ####
hist(nyAirbnb$price, xlab = "Price", ylab = "Frequency",main = "Histogram of Price Distribution")
hist(nyAirbnb$minimum_nights, xlab = "Minimum Nights", ylab = "Frequency")
plot(density(nyAirbnb$number_of_reviews))
plot(density(nyAirbnb$reviews_per_month))
par(mfrow = c(2,2))
boxplot(nyAirbnb$calculated_host_listings_count, main = "boxplot of total listing counts")
boxplot(nyAirbnb$availability_365, main = "boxplot of availability")

### No of listing by neighbourhood #####
nooflist = ggplot(data=nyAirbnb, aes(x=neighbourhood_group, fill = room_type))+geom_bar()+
  labs(title = "No of listings by Neigbourhood Group", x = "Neigbourhood Group", y = "No of listings")
nooflist

#### Top Neigbourhood ######
nyAirbnb %>% group_by(neighbourhood) %>% summarize(nooflist = n(), 
            nhood_group = unique(neighbourhood_group)) %>%
  top_n(n = 10, wt = nooflist) %>%
  ggplot(aes(x = neighbourhood, nooflist, y = nooflist, fill = nhood_group)) + geom_col() + coord_flip() +
  labs(title = "Top 10 neighborhoods by no. of listings",x = "Neighborhood",y = "No. of listings")

##### Average Cost of Property in each Neighbourhood Group ####

avg = nyAirbnb %>% group_by(neighbourhood_group) %>% summarise(price = round(mean(price),2))

avg

avgcost= ggplot(avg, aes(x = neighbourhood_group, y=price)) + geom_bar(stat = "identity", fill = "red") +
  labs(title = "Average Cost in Neighbourhood Group", x = "Neighbourhood Group", y = "Average Price")
avgcost

### Analysis of Property Types ####

roomType = ggplot(nyAirbnb, aes(room_type)) + geom_bar(stat = "count", fill = "blue")
roomType

### Price Analysis based on Property Types #####

costAnlaysis = ggplot(nyAirbnb, aes(x = room_type, y = price)) +
  geom_boxplot() + scale_y_log10() +ggtitle("Price by Room Types")
costAnlaysis


#### Relationship between Price and Avalibility #####

ggplot(nyAirbnb, aes(availability_365, price))+geom_point(alpha = 0.2, color = "slateblue") +
  geom_density(stat = "identity", alpha = 0.2) + labs(title = "Price Vs Availability", x = "Availability", y= "Price")

