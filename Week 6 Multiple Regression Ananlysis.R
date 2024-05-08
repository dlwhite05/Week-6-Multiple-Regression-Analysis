library(tidyverse)
library(stats)
library(ggplot2)
library(lmtest)
library(car)

# Load Birthweight data
load(url("https://www.dropbox.com/s/cnwtcr096szm8im/omsba_5112_birthweight.rdata?raw=1"))

my_data <- birthweight

# Summary statistics of data
summary(my_data)

# Define regression model
model <- lm(bwght ~ cigtax + cigprice + parity + fatheduc + motheduc + male + white + cigs + lbwght + bwghtlbs + packs + faminc, data = my_data)

# Run a regression with children's birth weight in ounces as the dependent variable (Y) and average daily number of cigarettes consumed by the mother during pregnancy, family income in $1,000s, sex of child, race, and the parity of the child as the explanatory variables (Xs). 
model <- lm(bwght ~ cigs + faminc + male + white + parity, data = my_data)

# Run the regression analysis with the square of family income
model_poly <- lm(bwght ~ cigs + faminc + I(faminc^2) + male + white + parity, data = my_data)

# View the summary of the regression results
summary(model_poly)

# Create plots for analysis

# Obtain the residuals from the model
model_residuals <- residuals(model_poly)

# Create a residual plot
plot(model_poly$model$faminc, model_residuals, xlab = "Family Income", ylab = "Residuals", main = "Residual Plot")

# Create a predicted vs. residuals plot
plot(fitted(model_poly), model_residuals, xlab = "Predicted Values", ylab = "Residuals", main = "Predicted vs. Residuals Plot")

#Log of birth weight as the dependent outcome.
model <- lm(lbwght ~ cigtax + cigprice + parity + fatheduc + motheduc + male + white + cigs + bwght + bwghtlbs + packs + faminc, data = my_data)


# Fit a linear regression model with log birth weight and the 'male' variable
model_log <- lm(lbwght ~ male + white + cigs, data = my_data)
summary(model_log)

# Fit a linear regression model with log birth weight as the outcome and log family income as a predictor
model_log_income <- lm(lbwght ~ lfaminc + male + white + cigs, data = my_data)
summary(model_log_income)

# Fit a model with interaction between smoking and race
model_interaction <- lm(lbwght ~ male + white + cigs + white:cigs, data = my_data)
summary(model_interaction)

# Fit a model with an interaction term between smoking (cigs) and race (white)
model_interaction_race <- lm(lbwght ~ cigs + white + cigs:white, data = my_data)

# View the summary of the model
summary(model_interaction_race)

# Fit a model with an interaction term between income and race
model_interaction_income_race <- lm(lbwght ~ faminc + white + faminc:white, data = my_data)

# View the summary of the model
summary(model_interaction_income_race)

# Fit a multiple regression model using the specified variables
model_all <- lm(lbwght ~ faminc + cigtax + cigprice + fatheduc + motheduc + parity + male + white + cigs + bwghtlbs + packs + faminc, data = my_data)

# View the summary of the model
summary(model_all)

# Extract the coefficients from the model
coefficients_all <- coef(model_all)[-1]  # Exclude the intercept

# Create a bar plot of the coefficients for all variables
barplot(coefficients_all, main = "Coefficients of All Variables", ylab = "Coefficient Value", col = "skyblue")
