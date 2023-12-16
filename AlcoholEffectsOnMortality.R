library(car)
library(tidyverse)
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #sets folder to current directory

#splitting the data into a training and testing set (if we want to use it)
#set.seed(1)
#train <- sample(1:nrow(data), 1100)
#data.train <- data[train, ]
#data.test <- data[-train, ]

data <- read.csv("LifeExpectancy2015.csv")
data <- read.csv("Life-Expectancy-Data-Updated.csv")
view(data)
unique(data$Region)

data.Asia <- data[data$Region == "Asia", ]
data.Rest_of_Europe <- data[data$Region == "Rest of Europe", ]
data.Africa <- data[data$Region == "Africa", ]
data.SA <- data[data$Region == "South America", ]
data.Cen_America_Caribbean <- data[data$Region == "Central America and Caribbean", ]
data.Oceania <- data[data$Region == "Oceania", ]
data.EU <- data[data$Region == "European Union", ]
data.ME <- data[data$Region == "Middle East", ]
data.NA <- data[data$Region == "North America", ]

#------------------------------------------------------------------------
# Correlation of Alcohol Consumption on Life Expectancy

Africa_results <- lm(Life_expectancy~Alcohol_consumption, data = data.Africa)
Rest_Europe_results <- lm(Life_expectancy~Alcohol_consumption, data = data.Rest_of_Europe)
Asia_results <- lm(Life_expectancy~Alcohol_consumption, data = data.Asia)
SA_results <- lm(Life_expectancy~Alcohol_consumption, data = data.South_America)
CenA_Car_results <- lm(Life_expectancy~Alcohol_consumption, data = data.Cen_America_Caribbean)
Oceania_results <- lm(Life_expectancy~Alcohol_consumption, data = data.Oceania)
EU_results <- lm(Life_expectancy~Alcohol_consumption, data = data.EU)
ME_results <- lm(Life_expectancy~Alcohol_consumption, data = data.ME)
NA_results <- lm(Life_expectancy~Alcohol_consumption, data = data.NA)

summary(Africa_results)
summary(Rest_Europe_results)
summary(Asia_results)
summary(SA_results)
summary(CenA_Car_results)
summary(Oceania_results)
summary(EU_results)
summary(ME_results)
summary(NA_results)

# marginal scatter plots

par(mfrow=c(3,3))
plot(data.Africa$Alcohol_consumption, data.Africa$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "Africa")
abline(Africa_results)
  
plot(data.Asia$Alcohol_consumption, data.Asia$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "Asia")
abline(Asia_results)

plot(data.Cen_America_Caribbean$Alcohol_consumption, data.Cen_America_Caribbean$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "Cen. America & Carribean")
abline(CenA_Car_results)

plot(data.EU$Alcohol_consumption, data.EU$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "European Union")
abline(EU_results)

plot(data.ME$Alcohol_consumption, data.ME$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "Middle East")
abline(ME_results)

plot(data.NA$Alcohol_consumption, data.NA$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "North America")
abline(NA_results)

plot(data.Oceania$Alcohol_consumption, data.Oceania$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "Oceania")
abline(Oceania_results)

plot(data.Rest_of_Europe$Alcohol_consumption, data.Rest_of_Europe$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "Rest of Europe")
abline(Rest_Europe_results)

plot(data.SA$Alcohol_consumption, data.SA$Life_expectancy, xlab = "Alcohol Consumption", ylab = "Life Expectancy", main = "South America")
abline(SA_results)

# plots
par(mfrow=c(2,2))
plot(Africa_results)
plot(Asia_results)
plot(CenA_Car_results)
plot(EU_results)
plot(ME_results)
plot(NA_results)
plot(Oceania_results)
plot(Rest_Europe_results)
plot(SA_results)

#------------------------------------------------------------------------
# Correlation of Alcohol Consumption on Life Expectancy

Africa_results <- lm(Adult_mortality~Alcohol_consumption, data = data.Africa)
Rest_Europe_results <- lm(Adult_mortality~Alcohol_consumption, data = data.Rest_of_Europe)
Asia_results <- lm(Adult_mortality~Alcohol_consumption, data = data.Asia)
SA_results <- lm(Adult_mortality~Alcohol_consumption, data = data.South_America)
CenA_Car_results <- lm(Adult_mortality~Alcohol_consumption, data = data.Cen_America_Caribbean)
Oceania_results <- lm(Adult_mortality~Alcohol_consumption, data = data.Oceania)
EU_results <- lm(Adult_mortality~Alcohol_consumption, data = data.EU)
ME_results <- lm(Adult_mortality~Alcohol_consumption, data = data.ME)
NA_results <- lm(Adult_mortality~Alcohol_consumption, data = data.NA)

summary(Africa_results)
summary(Rest_Europe_results)
summary(Asia_results)
summary(SA_results)
summary(CenA_Car_results)
summary(Oceania_results)
summary(EU_results)
summary(ME_results)
summary(NA_results)

# marginal scatter plots
par(mfrow=c(3,3))
plot(data.Africa$Alcohol_consumption, data.Africa$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "Africa")
abline(Africa_results)

plot(data.Asia$Alcohol_consumption, data.Asia$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "Asia")
abline(Asia_results)

plot(data.Cen_America_Caribbean$Alcohol_consumption, data.Cen_America_Caribbean$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "Cen. America & Carribean")
abline(CenA_Car_results)

plot(data.EU$Alcohol_consumption, data.EU$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "European Union")
abline(EU_results)

plot(data.ME$Alcohol_consumption, data.ME$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "Middle East")
abline(ME_results)

plot(data.NA$Alcohol_consumption, data.NA$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "North America")
abline(NA_results)

plot(data.Oceania$Alcohol_consumption, data.Oceania$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "Oceania")
abline(Oceania_results)

plot(data.Rest_of_Europe$Alcohol_consumption, data.Rest_of_Europe$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "Rest of Europe")
abline(Rest_Europe_results)

plot(data.SA$Alcohol_consumption, data.SA$Adult_mortality, xlab = "Alcohol Consumption", ylab = "Adult Mortality", main = "South America")
abline(SA_results)


