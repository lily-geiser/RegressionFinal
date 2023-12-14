library(car)
library(tidyverse)

#splitting the data into a training and testing set (if we want to use it)
#set.seed(1)
#train <- sample(1:nrow(data), 1100)
#data.train <- data[train, ]
#data.test <- data[-train, ]

data <- read.csv("cleandata.csv")
data.2015 <- data[data$Year == 2015, ]
data.2010 <- data[data$Year == 2010, ]

#just a basic regression
initial_reg <- lm(Life_expectancy~., data=data)
print(summary(initial_reg))

#regression with just 2015
reg_2015 <- lm(Life.expectancy~as.factor(Country)+
                 Adult.Mortality+infant.deaths+
                 Alcohol+Hepatitis.B+Measles+BMI+under.five.deaths+Polio
               +Diphtheria+HIV.AIDS+GDP+Population+thinness..1.19.years+
                 thinness.5.9.years+Income.composition.of.resources+Schooling, data=data.2015)
reg_2010 <- lm(Life_expectancy~. - Year, data=data.2010)
reg_2015_ <- lm(Life.expectancy~. - Status, data=data.2015)


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
#doesn't work
print(vif(initial_reg)) #doesn't look like there's significant collinearity for any of the variables

#doesn't look substantially better or worse tbh
reduced_reg <- #add later
print(summary(reduced_reg))

#no significant difference between reduced and initial model
anova(reduced_reg, initial_reg, test = "F")
