

# Install packages and load libraries
library(stargazer)



# Loading data using read.csv()
A1 <- read.csv(file = "insurance.csv", header = TRUE, sep = ',')

# Let's look at first few rows of our data set
head(A1)

# Let's view the entire data set
View(A1)

#Question 1

#Summary Statistics for Age, BMI, and Charges
summary(A1$age)
summary(A1$bmi)
summary(A1$charges)

#Plotting Histograms and Checking for Normal Distribution

hist(A1$age, main="Distribution of Age", xlab="Age", col="steelblue", border="black", prob=TRUE)
lines(density(A1$age), col="darkblue", lwd=2)

hist(A1$bmi, main="Distribution of BMI", xlab="BMI", col="lightgreen", border="black", prob=TRUE)
lines(density(A1$bmi), col="darkgreen", lwd=2)

hist(A1$charges, main="Distribution of Charges", xlab="Charges", col="lightcoral", border="black", prob=TRUE)
lines(density(A1$charges), col="darkred", lwd=2)



#Tabulating Categorical Variables
table(A1$sex)
table(A1$smoker)
table(A1$region)

#Percentage of Males vs Females
sex_counts <- table(A1$sex)
sex_percentage <- prop.table(sex_counts) * 100
sex_percentage

#Frequently Sampled US Region
region_counts <- table(A1$region)
most_frequent_region <- names(region_counts)[which.max(region_counts)]
most_frequent_region

#Percentage of Smokers vs Non-Smokers
smoker_counts <- table(A1$smoker)
smoker_percentage <- prop.table(smoker_counts) * 100
smoker_percentage

#Question 2

# Creating a new variable log_bmi
A1$log_bmi <- log(A1$bmi)


# Plotting  histogram for log_bmi
hist(A1$log_bmi, main="Distribution of log(BMI)", xlab="log(BMI)", col="skyblue", border="black", prob=TRUE)
lines(density(A1$log_bmi), col="darkblue", lwd=2)


#Question 3

# QQ plot for Age
qqnorm(A1$age, pch = 1, frame = FALSE, main = "QQ Plot for Age", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(A1$age, col = "steelblue", lwd = 2)


# QQ plot for BMI
qqnorm(A1$bmi, pch = 1, frame = FALSE, main = "QQ Plot for BMI", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(A1$bmi, col = "steelblue", lwd = 2)

# QQ plot for log(BMI)
qqnorm(A1$log_bmi, pch = 1, frame = FALSE, main = "QQ Plot for log(BMI)", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")
qqline(A1$log_bmi, col = "steelblue", lwd = 2)


#Question 4

# Constructing  gender variable
A1$gender <- ifelse(A1$sex == "male", 1, 0)

# Plotting a histogram for the gender variable
hist(A1$gender, 
     main = "Histogram of Gender", 
     xlab = "Gender (1 = Male, 0 = Female)", 
     col = c("lightpink", "lightblue"),  
     border = "black", 
     breaks = 2, 
     xaxt = "n",  
     ylim = c(0, 800))  

axis(1, at = c(0, 1), labels = c("Female", "Male"))


# Creating the isSmoker variable
A1$isSmoker <- ifelse(A1$smoker == "yes", 1, 0)

# Percentage of smokers by gender
male_smokers <- sum(A1$isSmoker == 1 & A1$gender == 1) / sum(A1$gender == 1) * 100
female_smokers <- sum(A1$isSmoker == 1 & A1$gender == 0) / sum(A1$gender == 0) * 100

male_smokers  
female_smokers  

# Constructing the healthyBMI variable
A1$healthyBMI <- ifelse(A1$bmi >= 18.5 & A1$bmi <= 24.9, 1, 0)

# Percentage of people with healthy BMI
healthy_bmi_percentage <- sum(A1$healthyBMI == 1) / nrow(A1) * 100
healthy_bmi_percentage  

# Percentage of males with a healthy BMI
healthy_male_bmi <- sum(A1$healthyBMI == 1 & A1$gender == 1) / sum(A1$gender == 1) * 100

# Percentage of females with a healthy BMI
healthy_female_bmi <- sum(A1$healthyBMI == 1 & A1$gender == 0) / sum(A1$gender == 0) * 100

healthy_male_bmi  
healthy_female_bmi  


# Ensure region is a factor
A1$region <- as.factor(A1$region)

#Question 5

# Running simple linear regression models
lm_age <- lm(charges ~ age, data = A1)
lm_gender <- lm(charges ~ gender, data = A1)
lm_isSmoker <- lm(charges ~ isSmoker, data = A1)
lm_healthyBMI <- lm(charges ~ healthyBMI, data = A1)
lm_region <- lm(charges ~ region, data = A1)
lm_children <- lm(charges ~ children, data = A1)




# Export multiple regression results to Word
stargazer(lm_age, lm_gender, 
          type = "html", out = "regression_results1.doc")
stargazer(lm_age, lm_gender, lm_isSmoker, type = "html", out = "test2_results.doc")




# Export multiple regression results to Word
stargazer(lm_multiple, type = "html", out = "multiple_regression_results.doc")

 

# Multiple regression with significant variables
lm_multiple <- lm(charges ~ age + gender + isSmoker + healthyBMI + children, data = A1)

# Summary of multiple regression model
summary(lm_multiple)

# Export multiple regression results to Word
stargazer(lm_multiple, type = "html", out = "multiple_regression_results.doc")


# Predictions for different individuals
# A 35-year-old smoker with no children and a BMI of 18
pred1 <- predict(lm_multiple, data.frame(age = 35, gender = 1, isSmoker = 1, healthyBMI = 0, children = 0))

# A 35-year-old smoker with no children and a BMI of 20
pred2 <- predict(lm_multiple, data.frame(age = 35, gender = 1, isSmoker = 1, healthyBMI = 1, children = 0))

# A 35-year-old smoker with 2 children and a BMI of 20
pred3 <- predict(lm_multiple, data.frame(age = 35, gender = 1, isSmoker = 1, healthyBMI = 1, children = 2))

# A 35-year-old non-smoker with 2 children and a BMI of 20
pred4 <- predict(lm_multiple, data.frame(age = 35, gender = 1, isSmoker = 0, healthyBMI = 1, children = 2))

# A 60-year-old non-smoker with 2 children and a BMI of 20
pred5 <- predict(lm_multiple, data.frame(age = 60, gender = 1, isSmoker = 0, healthyBMI = 1, children = 2))

# Display the predictions
pred1
pred2
pred3
pred4
pred5




