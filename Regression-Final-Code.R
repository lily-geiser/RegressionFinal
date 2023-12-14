library(car)

#splitting the data into a training and testing set (if we want to use it)
set.seed(1)
train <- sample(1:nrow(data), 1100)
data.train <- data[train, ]
data.test <- data[-train, ]

data <- read.csv("insurance.csv")
#just a basic regression
initial_reg <- lm(charges~age+as.factor(sex)+bmi+children+as.factor(smoker)+as.factor(region), data=data)
initial_reg <- lm(charges~., data=data)
print(summary(initial_reg))

#multicollinearity
print(vif(initial_reg)) #doesn't look like there's significant collinearity for any of the variables

#doesn't look substantially better or worse tbh
reduced_reg <- lm(charges~age+bmi+children+as.factor(smoker), data=data)
print(summary(reduced_reg))

#no significant difference between reduced and initial model
anova(reduced_reg, initial_reg, test = "F")
