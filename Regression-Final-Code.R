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
#no significant difference between reduced and initial model
anova(reg_2015_red, reg_2015, test = "F")

print(vif(reg_2015_red))

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


