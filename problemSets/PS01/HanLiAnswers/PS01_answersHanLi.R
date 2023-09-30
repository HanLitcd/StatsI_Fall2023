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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t.test(y,conf.level = 0.9, alternative = "two.sided")
t_score <- qt(0.95, df=length(y)-1)
lower_90_t <- mean(y)-(t_score)*(sd(y)/sqrt(length(y)))
upper_90_t <- mean(y)+(t_score)*(sd(y)/sqrt(length(y)))

#H0:school iq <= 100 H1:school_iq> 100 (one sided)
t.test(y,mu=100, alternative = "greater")

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
pdf("relationships.pdf")
pairs(expenditure[,c('Y','X1','X2','X3')])
dev.off( )
pdf("ExpenditurebyRegion.pdf")
boxplot(expenditure$Y~ expenditure$Region,names =c("NorthEast", "NorthCentral", "South","West"))
dev.off()
pdf("plotyx1.pdf")
plot(expenditure$X1,expenditure$Y)
dev.off()
pdf("YbyX1Region.pdf")
plot(expenditure$X1,expenditure$Y,col=expenditure$Region,pch=expenditure$Region)
legend(1000,120,
legend=c("NorthEast", "NorthCentral", "South","West"),
col = c('black','red','green','blue'),
pch=c(1,2,3,4))
dev.off()
