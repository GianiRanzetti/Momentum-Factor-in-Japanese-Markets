library(tidyverse)
library(lmtest)
library(plm)
library(stargazer)
library(sandwich)

ThesisData <- read.csv("C:\\Users\\granz\\Desktop\\Thesis\\IDK.csv")
ThesisData$Date <- as.Date(ThesisData$Date, format = "%m/%d/%Y")


portfolios <- select(ThesisData,Year, MOM1JP, MOM2JP, MOM3JP, WMLMOM, COMBOWML)
factors <- select(ThesisData, Year, Excess.Market.Returns, Bear.Market.Indicator, Up.Market.Indicator, Six.Month.Variance, Panic.Indicatorr, Bull.Market.Indicator)
summary(factors)
summary(portfolios)

# First regression of POrtfolio returns on market
ff_models1 <- lapply(portfolios, function(x) lm(x ~ Excess.Market.Returns, data = factors))
lapply(ff_models1, summary)

# First regression of POrtfolio returns on market + Value Factor + Momentum Factor + Bear Market Indicator + interaction term
ff_models2 <- lapply(portfolios, function(x) lm(x ~ Bear.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator, data = factors))
lapply(ff_models2, summary)

# First regression of POrtfolio returns on market + Value Factor + Momentum Factor + Bear Market Indicator + interaction terms
ff_models3 <- lapply(portfolios, function(x) lm(x ~ Bear.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Bear.Market.Indicator:Up.Market.Indicator, data = factors))
lapply(ff_models3, summary)

# First regression of POrtfolio returns on market + Value Factor + Momentum Factor + Bear Market Indicator + interaction terms
ff_models4 <- lapply(portfolios, function(x) lm(x ~ Bull.Market.Indicator + Excess.Market.Returns + Excess.Market.Returns:Bull.Market.Indicator + Excess.Market.Returns:Bull.Market.Indicator:Up.Market.Indicator, data = factors))
lapply(ff_models4, summary)

# Fourth regression with interaction terms excluding standalone bear market indicator
ff_models5 <- lapply(portfolios, function(x) lm(x ~ Bear.Market.Indicator + Six.Month.Variance + Excess.Market.Returns + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance, data = factors))
lapply(ff_models5, summary)

ff_models6 <- lapply(portfolios, function(x) lm(x ~ Panic.Indicatorr + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance + Excess.Market.Returns:Panic.Indicatorr, data = factors))
lapply(ff_models6, summary)

stargazer(ff_models1)
stargazer(ff_models2)
stargazer(ff_models3)
stargazer(ff_models4)
stargazer(ff_models5)
stargazer(ff_models6)








