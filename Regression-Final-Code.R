library(car)
library(tidyverse)

data <- read.csv("cleandata.csv")
data.2015 <- data[data$Year == 2015, ]
data.2010 <- data[data$Year == 2010, ]

#just a basic regression
initial_reg <- lm(Life_expectancy~., data=data)
print(summary(initial_reg))

#regression with just 2015
reg_2015 <- lm(Life_expectancy~as.factor(Region)+Infant_deaths+Under_five_deaths+
                 Adult_mortality+Alcohol_consumption+Hepatitis_B+Measles+BMI+Polio+
                 Diphtheria+Incidents_HIV+GDP_per_capita+Population_mln+
                 Thinness_ten_nineteen_years+Thinness_five_nine_years+Schooling+
                 as.factor(Economy_status_Developed), data=data.2015)
#reduced model
reg_2015_red <- lm(Life_expectancy~Adult_mortality
                     +Alcohol_consumption+Measles+BMI+
                     Diphtheria+Incidents_HIV+GDP_per_capita+Population_mln+
                     Thinness_ten_nineteen_years+Schooling+
                     as.factor(Economy_status_Developed), data=data.2015)
anova(reg_2015_red, reg_2015, test = "F") #initial model is better
print(vif(reg_2015_red))

#just region
reg_2015_region <- lm(Life_expectancy~as.factor(Region), data=data.2015)
plot(as.factor(data.2015$Region), data.2015$Life_expectancy)

#single regression models for each variable
#age_reg <- lm(charges~age, data=data) #significant
#sex_reg <- lm(charges~sex, data=data) #significant but barely
#bmi_reg <- lm(charges~bmi, data=data) #significant
#children_reg <- lm(charges~children, data=data) #significant but just a little
#smoker_reg <- lm(charges~smoker, data=data) #VERY significant
#region_reg <- lm(charges~region, data=data) #NOT significant

#making new variables for regions
#data = data %>% mutate(nw = 
#                         case_when(region == "northwest" ~ 1,
#                                   TRUE ~ 0))


#multicollinearity
print(vif(reg_2015)) #high correlation in region, Infant_deaths, Under_five_deaths, hepB, Diphtheria, and thinness
cor(data.2015$Thinness_five_nine_years, data.2015$Thinness_ten_nineteen_years) #EXTREMELY high

correlation_matrix <- cor(data.2015[, sapply(data.2015, is.numeric)])
high_correlation_indices <- which(correlation_matrix > 0.8, arr.ind = TRUE)
high_correlation_values <- correlation_matrix[high_correlation_indices]
row_names <- rownames(correlation_matrix)[high_correlation_indices[, 1]]
col_names <- colnames(correlation_matrix)[high_correlation_indices[, 2]]
result_df <- data.frame(
  Row = row_names,
  Column = col_names,
  Correlation = high_correlation_values
)
print(result_df)

#doesn't look substantially better or worse tbh
reduced_reg <- #add later
print(summary(reduced_reg))





#classification stuff
#splitting the data into a training and testing set (if we want to use it)
set.seed(1)
train <- sample(1:nrow(data.2015), 125)
data.train <- data.2015[train, ]
data.test <- data.2015[-train, ]

library(randomForest)
set.seed(1)
#creating the model
rf2.fit <- randomForest(as.factor(Region) ~ . - Country, data = data.2015, subset = train, mtry = 2, importance = TRUE)
#applying model to test set
rf2.pred <- predict(rf2.fit, data.test)
#creating confusion matrix
print(table(rf2.pred, data.test$Region))
#find test error
rf2.error <- mean(rf2.pred != data.test$Region)
rf2.error

set.seed(1)
library(e1071)
#creating the model
nb.fit <- naiveBayes(as.factor(Region) ~ ., data = data.2015, subset = train)
#applying model to test set
nb.pred <- predict(nb.fit, data.test)
#creating confusion matrix
print(table(nb.pred, data.test$Region))
#find test error
nb.error <- mean(nb.pred != data.test$Region)
nb.error

library(MASS)
set.seed(1)
#creating the model
qda.fit <- lda(as.factor(Region) ~ . - Country, data = data.2015, subset = train)
#applying model to test set
qda.pred <- predict(qda.fit, data.test)
#creating confusion matrix
qda.class <- qda.pred$class
print(table(qda.class, data.test$Region))
#find test error
qda.error <- mean(qda.class != data.test$Region)
qda.error








