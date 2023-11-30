#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
lapply(c("stargazer","car"),  pkgTest)
# Load the Prestige dataset
data("Prestige")
#alternatively as instructed in PS i had run the following:
#install.packages(car)
#library(car)
#data(Prestige)
#help(Prestige)
# Recode the 'type' variable to create 'professional'
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
View(Prestige)
#note that there are 4 out of 102 variables that are missing type hence NA in professional
#as the number of missing value is relatively small, I will drop them as they shouldn't have much difference in model result
#Prestige_clean <- na.omit(Prestige[c("income", "professional", "prestige")])

# Run the linear model on the cleaned dataset
#odel <- lm(prestige ~ income + professional + income:professional, data = Prestige_clean)
# Linear model with interaction term
model <- lm(prestige ~ income + professional + income:professional, data = Prestige)
summary(model)
stargazer(model)
#Prestige = Intercept + Coef_income*Income + Coef_professional*Professional + Coef_interaction*Income*Professional
coef_model <- coef(model)

# Calculate effect of $1,000 increase for professionals
effect_increase <- coef_model["income"] + coef_model["income:professional"]
marginal_1000_income <-effect_increase * 1000  # since the increase is $1000

# Calculate the effect of changing to professional at $6,000 income
change_effect <- coef_model["professional"] + coef_model["income:professional"] * 6000
change_effect

#q2
# Assuming a standard significance level of 0.05
coef_value <- 0.042
std_error <- 0.016
t_value <- coef_value / std_error
p_value <- 2 * (1 - pt(abs(t_value), df = 131 - 2))  # two-tailed test

# Repeat the hypothesis test procedure with the coefficient and standard error for adjacent precincts
coef_value_adjacent <- 0.042
std_error_adjacent <- 0.013
t_value_adjacent <- coef_value_adjacent / std_error_adjacent
p_value_adjacent <- 2 * (1 - pt(abs(t_value_adjacent), df = 131 - 2))