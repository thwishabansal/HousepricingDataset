rm(list=ls()) 
library(readxl)
library(dplyr)
library(rpart)
library(ggplot2)
library(car)
library(Metrics)
library(ISLR)

#Reading the dataset
setwd("/Users/thwishaabansal/Desktop")
getwd()
Housing = read.csv("Housing.csv", stringsAsFactors = TRUE)
str(Housing)
View(Housing)

#I mutated all the categorical variables so that they can be used in the different models for comparison now
Housing = Housing %>% mutate(guestroom_dummy = ifelse(guestroom=="yes", 1,0))
Housing = Housing %>% mutate(basement_dummy = ifelse(basement=="yes", 1,0))
Housing = Housing %>% mutate(hotwaterheating_dummy = ifelse(hotwaterheating=="yes", 1,0))
Housing = Housing %>% mutate(furnishingstatus_dummy = ifelse(furnishingstatus=="yes", 1,0))
Housing = Housing %>% mutate(prefarea_dummy = ifelse(prefarea=="yes", 1,0))
Housing = Housing %>% mutate(mainroad_dummy = ifelse(mainroad=="yes", 1,0))
Housing = Housing %>% mutate( airconditioning_dummy= ifelse(mainroad=="yes", 1,0))


#First we start off by doing Exploratory data analysis to look at our variables and determine how they are placed.
#We create basic bar graphs to look at the numbers in each one first
ggplot(Housing, aes(x=bedrooms,)) + geom_bar()
ggplot(Housing, aes(x=bathrooms,)) + geom_bar()
ggplot(Housing, aes(x=stories,)) + geom_bar()
ggplot(Housing, aes(x=parking,)) + geom_bar()
ggplot(Housing, aes(x=furnishingstatus_dummy,)) + geom_bar()
ggplot(Housing, aes(x=hotwaterheating_dummy,)) + geom_bar()
#The first bar graph shows the number of houses in the dataset with respect to the number of bedrooms. The graph indicates that the majority of the houses have 3 bedrooms, followed by 2 bedrooms and 4 bedrooms.
#The second bar graph shows the number of houses in the dataset with respect to the number of bathrooms. The graph indicates that the majority of the houses have only one bathroom.
#The third bar graph shows the number of houses in the dataset with respect to the number of stories. The graph indicates that the majority of the houses have 2 stories.
#The fourth bar graph shows the number of houses in the dataset with respect to the number of parking spaces available. The graph indicates that the majority of the houses have no parking spaces.

ggplot(Housing, aes(x=bedrooms, y=price)) + geom_point()
ggplot(Housing, aes(x=bathrooms, y=price)) + geom_point()
ggplot(Housing, aes(x=area, y=price)) + geom_point()
ggplot(Housing, aes(x=hotwaterheating_dummy, y=price)) + geom_point()
#The first scatter plot shows the relationship between the number of bedrooms and the price of the house. It appears that the price increases as the number of bedrooms increase. Additionally, there are a few outliers where the price is high even though the number of bedrooms is low.
#The second scatter plot shows the relationship between the number of bathrooms and the price of the house. It appears that the price increases as the number of bathrooms increase. Additionally, there are a few outliers where the price is high even though the number of bathrooms is low.
#The third scatterplot between area and price shows a positive relationship which means that as the area of the house increases, the price tends to increase as well. The points on the plot appear to form a linear pattern, indicating that a linear regression model may be a good choice for modeling this relationship.

#Removing outliers for the top 1 percentile in our numeric dataset since we found out that there are outliers in the dataset
Housing = Housing %>% 
  filter(price < quantile(price, 0.99),
         area < quantile(area, 0.99),
         bedrooms < quantile(bedrooms, 0.99),
         bathrooms < quantile(bathrooms, 0.99),
         stories < quantile(stories, 0.99),
         parking < quantile(parking, 0.99))

#Creating a train and test dataset
set.seed(69)
random_sample <- createDataPartition(Housing$price, p = 0.7, list = FALSE)
training_dataset  <- Housing[random_sample, ]
testing_dataset <- Housing[-random_sample, ]

Model1 = lm(price ~ bathrooms + hotwaterheating_dummy + parking, data = training_dataset )
summary(Model1)
predictions <- Model1 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model1)

Model2 = lm(price ~ bedrooms + bathrooms, data = training_dataset)
summary(Model2)
predictions <- Model2 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model2)

Model3 = lm(price ~ bedrooms + bathrooms + stories, data = training_dataset)
summary(Model3)
predictions <- Model3 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model3)


Model4 = lm(price ~ bedrooms + bathrooms + stories + area + parking, data = training_dataset)
summary(Model4)
predictions <- Model4 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model4)

Model8 = lm(price ~ bathrooms + stories + area + parking + mainroad_dummy + guestroom_dummy, data = training_dataset)
summary(Model8)
predictions <- Model8 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model8)


Model9 = lm(price ~ bathrooms + stories + area + furnishingstatus_dummy + mainroad_dummy , data = training_dataset)
summary(Model9)
predictions <- Model9 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model9)

Model5 = lm(price ~ bathrooms + stories + area + parking + hotwaterheating_dummy + airconditioning_dummy + prefarea_dummy + mainroad_dummy , data = training_dataset)
summary(Model5)
predictions <- Model5 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model9)

Model7 = lm(price ~ bathrooms + stories + area + parking + mainroad_dummy , data = training_dataset)
summary(Model7)
predictions <- Model7 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model7)

Model6 = lm(price ~ bathrooms + stories + area + parking+ prefarea_dummy + mainroad_dummy + hotwaterheating_dummy , data = training_dataset)
summary(Model6)
predictions <- Model6 %>% predict(testing_dataset)
data.frame(RMSE = RMSE(predictions, testing_dataset$price))
mape(predictions, testing_dataset$price)
vif(Model6)

#Based on these models i figured out that Model6 has the optimum variables and its the most efficient model
#To determine the best predictive model, you need to consider both the RMSE and MAPE values.
#Based on the RMSE and MAPE values, Model6 has the lowest RMSE and MAPE values, which means it is the best performing model among the ones evaluated.

par(mfrow=c(2,2))
plot(Model6,1)
plot(Model6,2)
plot(Model6,3)

#Creating a regression tree

library(caret)

# Split data into 80% training and 20% testing
set.seed(69)
index <- createDataPartition(Housing$price, p = 0.7, list = FALSE)
training_data1 <- training_dataset[index, ]
testing_data1 <- training_dataset[-index, ]

#Regression tree model
RG_1 <- rpart(price ~ bathrooms + stories + area + parking + mainroad_dummy , data = training_data1, method = "anova", minsplit = 5, xval = 0)
rpart.plot(RG_1)
summary(RG_1)
pred <- predict(RG_1, newdata = testing_data1)
rmse <- sqrt(mean((pred - testing_data1$price)^2))
mape <- mean(abs((testing_data1$price - pred)/testing_data1$price))*100
rmse
mape

RG_2 <- rpart(price ~ bathrooms + stories + area + parking+ prefarea_dummy + mainroad_dummy, data = training_data1, method = "anova", minsplit = 5, xval = 0)
rpart.plot(RG_2)
summary(RG_2)
pred <- predict(RG_2, newdata = testing_data1)
rmse <- sqrt(mean((pred - testing_data1$price)^2))
mape <- mean(abs((testing_data1$price - pred)/testing_data1$price))*100
rmse
mape

RG_4 <- rpart(price ~ bathrooms +  basement_dummy + stories + area, data = training_data1, method = "anova", minsplit = 5, xval = 0)
rpart.plot(RG_4)
summary(RG_4)
pred <- predict(RG_4, newdata = testing_data1)
rmse <- sqrt(mean((pred - testing_data1$price)^2))
mape <- mean(abs((testing_data1$price - pred)/testing_data1$price))*100
rmse
mape

RG_7 <- rpart(price ~ bathrooms + stories + area + parking+ prefarea_dummy + mainroad_dummy, data = training_data1, method = "anova", minsplit = 5, xval = 0)
rpart.plot(RG_7)
summary(RG_7)
pred <- predict(RG_7, testing_data1)
rmse <- sqrt(mean((pred - testing_data1$price)^2))
mape <- mean(abs((testing_data1$price - pred)/testing_data1$price))*100
rmse
mape

RG_8 <- rpart(price ~ bathrooms + stories + area + parking + prefarea_dummy + mainroad_dummy + hotwaterheating_dummy, data = training_data1, method = "anova", minsplit = 5, xval = 0)
rpart.plot(RG_8)
summary(RG_8)
pred <- predict(RG_8, testing_data1)
rmse <- sqrt(mean((pred - testing_data1$price)^2))
mape <- mean(abs((testing_data1$price - pred)/testing_data1$price))*100
rmse
mape

varImp(RG_1)
varImp(RG_2)
varImp(RG_3)
varImp(RG_4)
varImp(RG_5)
varImp(RG_6)
varImp(RG_7)

