install.packages("PerformanceAnalytics")
install.packages("GAS")
library(GAS)
library(PerformanceAnalytics)
library(quantmod)
library(segMGarch)
# Installa e carica il pacchetto fExtremes
install.packages("quarks")
library(quarks)
library(rugarch)
alpha=0.99
backtest_AD <-BacktestVaR(Dollar, VaR_Doll_AD, alpha, Lags = 4)
backtest_AS <- BacktestVaR(Dollar, VaR_Doll_AS, alpha, Lags = 4)
backtest_SAV <- BacktestVaR(Dollar, VaR_Doll_SAV, alpha, Lags = 4)
backtest_IG <- BacktestVaR(Dollar, VaR_Doll_IG, alpha, Lags = 4)

print(backtest_AD)


# Generate breaches (True if the return is less than the VaR)
breaches_AD = ifelse(Dollar < VaR_Doll_AD, 1, 0)
# Perform the Kupiec test
kupiec_test_AD = kupiec(Dollar, VaR_Doll_AD, 0.99, verbose = TRUE, test = "PoF")
print(kupiec_test_AD)

kupiec_test_AS = kupiec(Dollar, VaR_Doll_AS, 0.99, verbose = TRUE, test = "PoF")
print(kupiec_test_AS)

kupiec_test_IG = kupiec(Dollar, VaR_Doll_IG, 0.99, verbose = TRUE, test = "PoF")
print(kupiec_test_IG)

kupiec_test_SAV = kupiec(Dollar, VaR_Doll_SAV, 0.99, verbose = TRUE, test = "PoF")
print(kupiec_test_SAV)

# Perform the Christoffersen test for independence of breaches
christoffersen_test_AD = cvgtest(obj = list(loss = Dollar, VaR = VaR_AD_plot, p = 0.01 ), conflvl = 0.95)

  christoffersen_test_result = VaRTest(breaches, method = "cc", conf.level = 0.99)
print(christoffersen_test_result)
###Dinamic Quantile
  # We might need to add lags of breaches and returns if analyzing dynamics
lag_breaches_AD <- c(NA, breaches_AD[-length(breaches_AD)])
print(lag_breaches_AD)
data <- data.frame(breaches = breaches_AD, lag_breaches = lag_breaches_AD)

# Fit a logistic regression model
model <- glm(breaches_AD ~ lag_breaches_AD, family = binomial(link = "logit"), data = data, na.action = na.exclude)
summary(model)
# Install and load the lmtest package for diagnostic tests
if (!require(lmtest)) install.packages("lmtest")
library(lmtest)

# Run a Breusch-Godfrey test for autocorrelation
bgtest(model, order = 1)
