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
model <- lm(bwght ~ cigtax + cigprice + parity + fatheduc + motheduc + male + white + cigs + Ibwght + bwghtlbs + packs + Ifaminc, data = your_data)

# Run a regression with children's birth weight in ounces as the dependent variable (Y) and average daily number of cigarettes consumed by the mother during pregnancy, family income in $1,000s, sex of child, race, and the parity of the child as the explanatory variables (Xs). 
model <- lm(bwght ~ cigs + faminc + male + white + parity, data = my_data)
