library(car)

data <- read.csv("insurance.csv")
#just a basic regression
initial_reg <- lm(charges~age+as.factor(sex)+bmi+children+as.factor(smoker)+as.factor(region), data=data)
initial_reg <- lm(charges~., data=data)
print(summary(initial_reg))

#splitting the data into a training and testing set (if we want to use it)
set.seed(1)
train <- sample(1:nrow(data), 1100)
data.train <- data[train, ]
data.test <- data[-train, ]

#multicollinearity
print(vif(initial_reg))
