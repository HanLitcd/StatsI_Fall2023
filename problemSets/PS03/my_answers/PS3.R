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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
lapply(c("ggplot2","car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")
#1. regression:
model <- lm(voteshare ~ difflog, data = inc.sub)
summary(model)
# Scatterplot with regression line

jpeg(file='voteshare_difflog.jpeg')
plot(inc.sub$difflog, inc.sub$voteshare, main = "Scatterplot with Regression Line", xlab = "difflog", ylab = "voteshare", pch = 19, col = rgb(0.1, 0.1, 0.1, 0.5))
abline(model, col = "red")
dev.off()
residuals_voteshare <- resid(model)
#prediction
intercept <- coef(model)[1]
slope <- coef(model)[2]
#prediciton: voteshare = intercept + slope * difflog
paste("voteshare =", intercept, "+", slope, "* difflog ")
#q2:
lm_model_presvote <- lm(presvote ~ difflog, data = inc.sub)
summary(lm_model_presvote)
jpeg(file='presvote_difflog.jpeg')
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
dev.off()
residuals_presvote <- resid(lm_model_presvote)
#prediciton: presvote = intercept + slope * difflog
intercept2 <- coef(lm_model_presvote)[1]
slope2 <- coef(lm_model_presvote)[2]
paste("presvote =", intercept2, "+", slope2, "* difflog ")
#q3
model_voteshare <- lm(voteshare ~ presvote, data = inc.sub)
summary(model_voteshare)

# Save the plot
png(filename = "scatterplot_voteshare_presvote.png")
plot(inc.sub$presvote, inc.sub$voteshare, main = "Scatterplot with Regression Line", xlab = "presvote", ylab = "voteshare", pch = 19, col = rgb(0.1, 0.1, 0.1, 0.5))
abline(model_voteshare, col = "red")
dev.off()

# Getting coefficients
intercept_voteshare <- coef(model_voteshare)[1]
slope_voteshare <- coef(model_voteshare)[2]

# Prediction equation
paste("voteshare =", intercept_voteshare, "+", slope_voteshare, "* presvote")
#q4
# Fit the linear model with residuals
model_residuals <- lm(residuals_voteshare ~ residuals_presvote, data = inc.sub)
summary(model_residuals)
# Plotting


# Save the plot
png(filename = "scatterplot_residuals_q1_q2.png")
plot(residuals_presvote, residuals_voteshare, main = "Scatterplot of Residuals with Regression Line", xlab = "Residuals from Q2", ylab = "Residuals from Q1", pch = 19, col = rgb(0.1, 0.1, 0.1, 0.5))
abline(model_residuals, col = "red")
dev.off()
# Getting coefficients
intercept_residuals <- coef(model_residuals)[1]
slope_residuals <- coef(model_residuals)[2]

# Prediction equation for residuals
paste("Residuals_Q1(voteshare) =", intercept_residuals, "+", slope_residuals, "* Residuals_Q2 (presvote)")
#q5
# Fit the multiple linear regression model
model_voteshare_multi <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model_voteshare_multi)
# Getting coefficients for multiple regression
intercept_multi <- coef(model_voteshare_multi)[1]
slope_difflog <- coef(model_voteshare_multi)[2]
slope_presvote <- coef(model_voteshare_multi)[3]

# Prediction equation for multiple regression
paste("voteshare =", intercept_multi, "+", slope_difflog, "* difflog +", slope_presvote, "* presvote")

# Saving the plot for 'presvote'
png(filename = "added_variable_plot_presvote.png")
avPlots(model_voteshare_multi, terms=~ presvote)
dev.off()


