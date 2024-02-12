library(tidyverse)
library(lmtest)
library(plm)
library(stargazer)
library(sandwich)

ThesisData <- read.csv("C:\\Users\\granz\\Desktop\\Thesis\\IDK.csv")
ThesisData$Date <- as.Date(ThesisData$Date, format = "%m/%d/%Y")



library(plm)

summary(pmg(portfolios$WMLMOM ~ Six.Month.Variance + Panic.Indicatorr +
              Excess.Market.Returns:Bear.Market.Indicator +
              Excess.Market.Returns:Six.Month.Variance +
              Excess.Market.Returns:Panic.Indicatorr,
            data = factors, index=c("filter","firmid")))

portfolios <- select(ThesisData, Year, MOM1JP, MOM2JP, MOM3JP, WMLMOM, COMBOWML)
factors <- select(ThesisData, Year, Excess.Market.Returns, Bear.Market.Indicator, Up.Market.Indicator, Six.Month.Variance, Panic.Indicatorr, Bull.Market.Indicator)
variable_names <- c("Intercept", "6M Variance", "Panic",
                    "Mrkt*Bear",
                    "Mrkt*6M Variance",
                    "Mrkt*Panic")

coefs <- list()
ses <- list()

# Iterate over the portfolios and estimate time-specific coefficients and standard errors
for (i in 1:length(portfolios)) {
  portfolio_coefs <- matrix(NA, nrow = length(unique(factors$Year)), ncol = 6)
  portfolio_ses <- matrix(NA, nrow = length(unique(factors$Year)), ncol = 6)
  colnames(portfolio_coefs) <- variable_names
  colnames(portfolio_ses) <- variable_names
  rownames(portfolio_coefs) <- unique(factors$Year)
  rownames(portfolio_ses) <- unique(factors$Year)
  
  # Iterate over each year
  years <- unique(factors$Year)
  for (j in 1:length(years)) {
    if (!(colnames(portfolios)[i] == "COMBOWML" && j == 1)) {
      # Subset the data for the current year
      subset_data <- subset(factors, Year == years[j])
      subset_ret <- subset(portfolios, Year == years[j])
      
      # Perform the Fama-MacBeth regression for the current year
      fm_model <- lm(subset_ret[[i]] ~ Six.Month.Variance + Panic.Indicatorr +
                       Excess.Market.Returns:Bear.Market.Indicator +
                       Excess.Market.Returns:Six.Month.Variance +
                       Excess.Market.Returns:Panic.Indicatorr,
                     data = subset_data)
      
      # Store the coefficients for the current year
      portfolio_coefs[j, ] <- coef(fm_model)
      
      # Compute the standard errors for the current year
      fm_se <- sqrt(diag(vcovHC(fm_model, type = "HC1")))
      
      # Store the standard errors, replacing NA with NA_character_ to preserve NA values
      portfolio_ses[j, !is.na(portfolio_coefs[j, ])] <- fm_se[!is.na(portfolio_coefs[j, ])]
      portfolio_ses[j, is.na(portfolio_coefs[j, ])] <- NA_character_
    }
  }
  
  # Store the coefficient matrix and standard error matrix for the current portfolio
  coefs[[i]] <- portfolio_coefs
  ses[[i]] <- portfolio_ses
}

average_coefs <- matrix(NA, nrow = length(portfolios)-1, ncol = ncol(coefs[[2]]))
colnames(average_coefs) <- variable_names
row.names(average_coefs) <- colnames(portfolios)[-1]
average_ses <- matrix(NA, nrow = length(portfolios)-1, ncol = ncol(coefs[[2]]))
colnames(average_ses) <- variable_names
row.names(average_ses) <- colnames(portfolios)[-1]

# Iterate over the portfolios
for (i in 1:length(coefs)-1) {
  # Average the coefficients across years
  average_coefs[i, ] <- colMeans(coefs[[i+1]], na.rm = TRUE)
  
  # Convert the standard errors from characters to numeric values
  ses_numeric <- apply(ses[[i+1]], 2, function(x) as.numeric(as.character(x)))
  
  # Calculate the standard error across years
  average_ses[i, ] <- apply(ses_numeric, 2, function(x) sqrt(sum(!is.na(x))^-1 * sum(x^2, na.rm = TRUE)))
}


# Calculate the t-statistic
t_stat <- average_coefs / average_ses



