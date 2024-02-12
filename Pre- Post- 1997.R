library(tidyverse)
library(lmtest)
library(plm)
library(stargazer)
library(sandwich)

AppendixData <- read.csv("C:\\Users\\granz\\Desktop\\Thesis\\IDK2.csv")
AppendixData$Date <- as.Date(ThesisData$Date, format = "%m/%d/%Y")


APportfolios <- select(AppendixData,Year, MOM1JP, MOM2JP, MOM3JP, WMLMOM, COMBOWML)
APfactors <- select(AppendixData,Year, Excess.Market.Returns, Bear.Market.Indicator, Up.Market.Indicator, Six.Month.Variance, Panic.Indicatorr)

Preport <- APportfolios[APportfolios$Year == 1, ]
Prefactors <- APfactors[APfactors$Year == 1, ]
Postport <- APportfolios[APportfolios$Year == 2, ]
Postfactors <- APfactors[APfactors$Year == 2, ]


Pre_model1 <- lapply(Preport, function(x) lm(x ~ Excess.Market.Returns, data = Prefactors))
lapply(Pre_model1, summary)

Pre_model2 <- lapply(Preport, function(x) lm(x ~ Bear.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator, data = Prefactors))
lapply(Pre_model2, summary)

Pre_model3 <- lapply(Preport, function(x) lm(x ~ Bear.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Bear.Market.Indicator:Up.Market.Indicator, data = Prefactors))
lapply(Pre_model3, summary)

Pre_model4 <- lapply(Preport, function(x) lm(x ~ Bear.Market.Indicator + Six.Month.Variance + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance, data = Prefactors))
lapply(Pre_model4, summary)

Pre_model5 <- lapply(Preport, function(x) lm(x ~ Panic.Indicatorr + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance + Excess.Market.Returns:Panic.Indicatorr, data = Prefactors))
lapply(Pre_model5, summary)

stargazer(Pre_model1)
stargazer(Pre_model2)
stargazer(Pre_model3)
stargazer(Pre_model4)
stargazer(Pre_model5)

Post_model1 <- lapply(Postport, function(x) lm(x ~ Excess.Market.Returns, data = Postfactors))
lapply(Post_model1, summary)

Post_model2 <- lapply(Postport, function(x) lm(x ~ Bear.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator, data = Postfactors))
lapply(Post_model2, summary)

Post_model3 <- lapply(Postport, function(x) lm(x ~ Bear.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Bear.Market.Indicator:Up.Market.Indicator, data = Postfactors))
lapply(Post_model3, summary)

Post_model4 <- lapply(Postport, function(x) lm(x ~ Bear.Market.Indicator + Six.Month.Variance + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance, data = Postfactors))
lapply(Post_model4, summary)

Post_model5 <- lapply(Postport, function(x) lm(x ~ Panic.Indicatorr + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance + Excess.Market.Returns:Panic.Indicatorr, data = Postfactors))
lapply(Post_model5, summary)

stargazer(Post_model1)
stargazer(Post_model2)
stargazer(Post_model3)
stargazer(Post_model4)
stargazer(Post_model5)

