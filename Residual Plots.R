# Fit FF6 models and check for heteroscedasticity (for WMLMOM portfolio only)
Loser_model <- lm(portfolios$MOM1JP ~ Six.Month.Variance + Panic.Indicatorr + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance + Excess.Market.Returns:Panic.Indicatorr, data = factors)

bp_test_Loser <- bptest(Loser_model)
print("Breusch-Pagan test for Loser portfolio")
print(bp_test_Loser)

white_test_Loser <- bptest(Loser_model, studentize = FALSE)
print("White's test for Loser portfolio")
print(white_test_Loser)

# Plot residuals vs predictors for Loser portfolio
plot(Loser_model$fitted.values, residuals(Loser_model), 
     xlab = "Portfolio Returns", ylab = "Residuals",
     main = "Residuals vs Portfolio Returns - Loser portfolio")
abline(h = 0, col = "gray")

Winner_model <- lm(portfolios$MOM3JP ~ Six.Month.Variance + Panic.Indicatorr + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance + Excess.Market.Returns:Panic.Indicatorr, data = factors)

bp_test_Winner <- bptest(Winner_model)
print("Breusch-Pagan test for Winner portfolio")
print(bp_test_Winner)

white_test_Winner <- bptest(Winner_model, studentize = FALSE)
print("White's test for Winner portfolio")
print(white_test_Winner)

# Plot residuals vs predictors for Winner portfolio
plot(Winner_model$fitted.values, residuals(Winner_model), 
     xlab = "Portfolio Returns", ylab = "Residuals",
     main = "Residuals vs Portfolio Returns - Winner portfolio")
abline(h = 0, col = "gray")

WMLMOM_model <- lm(portfolios$WMLMOM ~ Six.Month.Variance + Panic.Indicatorr + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance + Excess.Market.Returns:Panic.Indicatorr, data = factors)

bp_test_WMLMOM <- bptest(WMLMOM_model)
print("Breusch-Pagan test for WMLMOM portfolio")
print(bp_test_WMLMOM)

white_test_WMLMOM <- bptest(WMLMOM_model, studentize = FALSE)
print("White's test for WMLMOM portfolio")
print(white_test_WMLMOM)

# Plot residuals vs predictors for WMLMOM portfolio
plot(WMLMOM_model$fitted.values, residuals(WMLMOM_model), 
     xlab = "Portfolio Returns", ylab = "Residuals",
     main = "Residuals vs Portfolio Returns - WML portfolio")
abline(h = 0, col = "gray")

COMBO_model <- lm(portfolios$COMBOWML ~ Six.Month.Variance + Panic.Indicatorr + Excess.Market.Returns:Bear.Market.Indicator + Excess.Market.Returns:Six.Month.Variance + Excess.Market.Returns:Panic.Indicatorr, data = factors)

bp_test_COMBO <- bptest(COMBO_model)
print("Breusch-Pagan test for COMBO portfolio")
print(bp_test_COMBO)

white_test_COMBO <- bptest(COMBO_model, studentize = FALSE)
print("White's test for COMBO portfolio")
print(white_test_COMBO)

# Plot residuals vs predictors for COMBO portfolio
plot(COMBO_model$fitted.values, residuals(COMBO_model), 
     xlab = "Portfolio Returns", ylab = "Residuals",
     main = "Residuals vs Portfolio Returns - Val- Mom portfolio")
abline(h = 0, col = "gray")
