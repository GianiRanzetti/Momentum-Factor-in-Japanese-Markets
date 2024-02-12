library(tidyverse)
library(lmtest)
library(plm)
library(stargazer)
library(sandwich)

FMData <- read.csv("C:\\Users\\granz\\Desktop\\Thesis\\FamaMacbeth.csv")

FM_reg1 <- pmg(Excess.Returns ~ Excess.Market.Returns, data=FMData, index=c("ID", "Year" ))

FM_reg2 <- pmg(Excess.Returns ~ Bear.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator , data=FMData, index=c("ID", "Year" ))

FM_reg3 <- pmg(Excess.Returns ~ Panic.Indicator + Six.Month.Variance + Excess.Market.Returns + Excess.Market.Returns:Panic.Indicator , data=FMData, index=c("ID", "Year" ))

summary(FM_reg1)
summary(FM_reg2)
summary(FM_reg3)
stargazer(FM_reg1)
stargazer(FM_reg2)
stargazer(FM_reg3)
