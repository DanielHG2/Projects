setwd("C:\\Users\\Daniel192\\Desktop\\TESINA SERIE STORICHE\\ELABORAZIONI\\Quarto-CAViars")
library(readxl)
library(openxlsx)
library(readr)
library(ggplot2)
library(PerformanceAnalytics)
library(expectreg)
library(rugarch)
library(quantmod)
library(GAS)
ret_crypto_index_comm <- na.omit(read_excel("ret_crypto_index_comm.xlsx"))
ret_crypto_index_comm$Date <- as.Date(ret_crypto_index_comm$Date)
calcolo_VAR <- read_excel("calcoloVAR_definitivo.xlsx")
calcolo_VAR$Date <- as.Date(calcolo_VAR$Date)

Bitcoin <- na.omit(ret_crypto_index_comm$`Bitcoin (BTC)`)
Bitcoin <- c()
Bitcoin.ret <- xts(ret_crypto_index_comm[, 2], order.by = ret_crypto_index_comm$Date)
Bitcoin.ret <- xts(ret_crypto_index_comm[, 2], order.by = ret_crypto_index_comm$Date)


####### CON I NUOVI MODELLI: IGm, ASm, SAVm, GJG e GJGm.
CAViaR.fun = function(beta, ret, tau, model, G) {
  
  N = length(ret)
  VaR = matrix(NA, N, 1)
  N1 = min(300, N/3)
  VaR[1] = quantile(ret[1:N1], probs = tau)
  
  if (model == "AD") {
    for(t in 2:N) {
      VaR[t] = VaR[t-1] + beta * ((1 + exp(G * (ret[t-1] - VaR[t-1])))^(-1) - tau)
    }
  } else if (model == "SAV") {
    for(t in 2:N) {
      VaR[t] = beta[1] + beta[2] * VaR[t-1] + beta[3] * abs(ret[t-1])
    }
  } else if (model == "AS") {
    for(t in 2:N) {
      VaR[t] = beta[1] + beta[2] * VaR[t-1] + (beta[3] * (ret[t-1] > 0) + beta[4] * (ret[t-1] < 0)) * abs(ret[t-1])
    }
  } else if (model == "IG") {
    for(t in 2:N) { 
      VaR[t] = - sqrt(abs(beta[1] + beta[2] * VaR[t-1]^2 + beta[3] * ret[t-1]^2))
    }
  } else if (model == "IGm") {
    for(t in 2:N) {
      VaR[t] = beta[4] - sqrt(abs(beta[1] + beta[2] * (VaR[t-1] - beta[4])^2 + beta[3] * (ret[t-1] - beta[4])^2))
    }
  } else if (model == "GjG") {
    for(t in 2:N) {
      VaR[t] = -sqrt(abs(beta[1] + beta[2] * VaR[t-1]^2 + beta[3] * (ret[t-1]>0)^2 + beta[4] *(ret[t-1]<0)^2))
    }
  } else if (model == "GjGm") {
    for(t in 2:N) {
      VaR[t] = beta[5]-sqrt(abs(beta[1] + beta[2] * (VaR[t-1]-beta[5])^2 + beta[3] * ((ret[t-1]-beta[5])>0)^2 + beta[4] *((ret[t-1]-beta[5])<0)^2))
    }
  } else if (model == "SAVm") {
    for(t in 2:N) {
      VaR[t] = beta[4] * (1 - beta[2]) + beta[1] + beta[2] * VaR[t-1] + beta[3] * abs(ret[t-1] - beta[4])
    }
  } else if (model == "ASm") {
    for(t in 2:N) {
      VaR[t] = beta[5] * (1 - beta[2]) + beta[1] + beta[2] * VaR[t-1] + beta[3] * ((ret[t-1]-beta[5]) > 0) + beta[4] * ((ret[t-1]-beta[5]) < 0)
    }
  }
  return(VaR)
}
#####################################################################
####################################################################
QL = function(beta, ret, tau, model, G){
  
  N = length(ret)
  if(N < 300) stop("Length of ret should be at least 300")
  if (!model %in% c("AD", "SAV", "AS", "IG", "IGm", "GjG", "GjGm", "SAVm", "ASm")) stop("Wrong MODEL selected")
  
  VaR = CAViaR.fun(beta, ret, tau, model, G)
  
  Hit = ifelse(ret <= VaR, 1, 0)
  out = (tau - Hit) * (ret - VaR)
  
  return(mean(out))
}
#######################################################
######################################################
####################################################
est.CAViaR = function(ret, tau, model, n.start, G) {
  
  if(missing(n.start)) n.start = 10
  if(model == "AD") start_value = matrix(runif(n.start, -1, 1), n.start, 1)
  if(model == "SAV") start_value = matrix(runif(n.start * 3, -1, 1), n.start, 3)
  if(model == "AS") start_value = matrix(runif(n.start * 4, -1, 1), n.start, 4)
  if(model == "IG") start_value = matrix(runif(n.start * 3, -1, 1), n.start, 3)
  if(model == "IGm") start_value = matrix(runif(n.start * 4, -1, 1), n.start, 4)
  if(model == "SAVm") start_value = matrix(runif(n.start * 4, -1, 1), n.start, 4)
  if(model == "ASm") start_value = matrix(runif(n.start * 5, -1, 1), n.start, 5)
  if(model == "GjG") start_value = matrix(runif(n.start * 4, -1, 1), n.start, 4)
  if(model == "GjGm") start_value = matrix(runif(n.start * 5, -1, 1), n.start, 5)
  QL_value = matrix(NA, n.start, 1)
  pars_vec = matrix(NA, n.start, ncol(start_value))
  
  if(missing(G)) G = 10
  if(model == "AD") {
    method = "Brent"
    low = -10
    up = 10
  } else {
    method = "Nelder-Mead"
    low = -Inf
    up = Inf
  }
  
  for(i in 1:n.start){
    optim.out = optim(start_value[i,], fn = QL, ret = ret, tau = tau, model = model, G = G, 
                      method = method, lower = low, upper = up, control = list(reltol = 1e-10, abstol = 1e-10, maxit = 1e4)) 
    QL_value[i] = optim.out$value
    pars_vec[i,] = optim.out$par
  }
  QL_max = QL_value[which.min(QL_value)]
  pars_optim = pars_vec[which.min(QL_value),]
  names(pars_optim) = paste("beta", 1:length(pars_optim), sep = "")
  
  fitted_VaR = CAViaR.fun(pars_optim, ret = ret, tau = tau, model = model, G = G)
  Hit = ifelse(ret <= fitted_VaR, 1, 0)
  
  cat("Quantile level:", tau,
      "\nCoefficients:", pars_optim,
      "\nQL:", QL_max,
      "\nProportion of VaR violations:", mean(Hit))
  
  out = list(tau = tau, pars = pars_optim, QL = QL_max, fitted = fitted_VaR, VaR_violations = Hit)
  return(out)
  }
#####################################

VaR_IGm_05 <- est.CAViaR(ret = Bitcoin, tau = 0.05, model = "IGm", n.start = 10)
VaR_IGm05 <- CAViaR.fun(beta = VaR_IGm_05$pars, ret = Bitcoin.ret, tau = 0.05, model = "IGm", G=10)
VaR_IGm_01 <- est.CAViaR(ret = Bitcoin, tau = 0.01, model = "IGm", n.start = 10)
VaR_IGm01 <- CAViaR.fun(beta = VaR_IGm_01$pars, ret = Bitcoin.ret, tau = 0.01, model = "IGm", G=10)
#
VaR_ASm_05 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.05, model = "ASm", n.start = 10)
VaR_ASm05 <- CAViaR.fun(beta = VaR_ASm_05$pars, ret = Bitcoin.ret, tau = 0.05, model = "ASm", G=10)
VaR_ASm_01 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.01, model = "ASm", n.start = 10)
VaR_ASm01 <- CAViaR.fun(beta = VaR_ASm_01$pars, ret = Bitcoin.ret, tau = 0.01, model = "ASm", G=10)
######
VaR_SAVm_05 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.05, model = "SAVm", n.start = 10)
VaR_SAVm05 <- CAViaR.fun(beta = VaR_SAVm_05$pars, ret = Bitcoin.ret, tau = 0.05, model = "SAVm", G=10)
VaR_SAVm_01 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.01, model = "SAVm", n.start = 10)
VaR_SAVm01 <- CAViaR.fun(beta = VaR_SAVm_01$pars, ret = Bitcoin.ret, tau = 0.01, model = "SAVm", G=10)
#
VaR_GjG_05 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.05, model = "GjG", n.start = 10)
VaR_GjG05 <- CAViaR.fun(beta = VaR_GjG_05$pars, ret = Bitcoin.ret, tau = 0.05, model = "SAVm", G=10)
VaR_GjG_01 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.01, model = "GjG", n.start = 10)
VaR_GjG01 <- CAViaR.fun(beta = VaR_GjG_01$pars, ret = Bitcoin.ret, tau = 0.01, model = "SAVm", G=10)
#
VaR_GjGm_05 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.05, model = "GjGm", n.start = 10)
VaR_GjGm05 <- CAViaR.fun(beta = VaR_GjGm_05$pars, ret = Bitcoin.ret, tau = 0.05, model = "GjGm", G=10)
VaR_GjGm_01 <- est.CAViaR(ret = Bitcoin.ret, tau = 0.01, model = "GjGm", n.start = 10)
VaR_GjGm01 <- CAViaR.fun(beta = VaR_GjGm_01$pars, ret = Bitcoin.ret, tau = 0.01, model = "GjGm", G=10)



alpha=0.05
backtest05_AD <-BacktestVaR(Bitcoin.ret, VaR_Bitcoin_AD05, alpha, Lags = 4)
backtest05_AS <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_AS05, alpha, Lags = 4)
backtest05_SAV <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_SAV05, alpha, Lags = 4)
backtest05_IG <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_IG05, alpha, Lags = 4)
backtest05_Qreg <- BacktestVaR(Bitcoin.ret, VaR_Qreg05, alpha, Lags = 4)
backtest05_Qregalt <- BacktestVaR(Bitcoin.ret, VaR_Qregalt05, alpha, Lags = 4)


calcoli <-data.frame(VaR_SAVm05, VaR_SAVm01, VaR_IGm05, VaR_IGm01, VaR_GjGm05, VaR_GjGm01, VaR_ASm05, VaR_ASm01)
write.xlsx(calcoli, "C:\\Users\\Daniel192\\Desktop\\TESINA SERIE STORICHE\\ELABORAZIONI\\Quarto-CAViars\\calcoli.xlsx")

VaR_ASm05<-calcolo_VAR$VaR_Asm05
VaR_ASm01<-calcolo_VAR$VaR_Asm01
VaR_IGm05<-calcolo_VAR$VaR_IGm05
VaR_IGm01<-calcolo_VAR$VaR_IGm01
VaR_SAVm05<-calcolo_VAR$VaR_SAVm05
VaR_SAVm01<-calcolo_VAR$VaR_SAVm01
VaR_GjG05<-calcolo_VAR$VaR_Gjg05
VaR_GjG01<-calcolo_VAR$VaR_Gjg01
VaR_GjGm05<-calcolo_VAR$VaR_Gjgm05
VaR_GjGm01<-calcolo_VAR$VaR_Gjgm01

VaR_IGm05<-na.omit(VaR_IGm05)
VaR_IGm01<-na.omit(VaR_IGm01)
VaR_SAVm05<-na.omit(VaR_SAVm05)
VaR_SAVm01<-na.omit(VaR_SAVm01)

VaR_IGm05<-c(VaR_IGm05, mean(VaR_IGm05))
VaR_IGm01<-c(VaR_IGm01, mean(VaR_IGm01))
VaR_SAVm05<-c(VaR_SAVm05, mean(VaR_SAVm05))
VaR_SAVm01<-c(VaR_SAVm01, mean(VaR_SAVm01))

##############################################################à
alpha=0.05
backtest05_IGm <-BacktestVaR(Bitcoin.ret, VaR_IGm05, alpha, Lags = 4)
backtest05_ASm <- BacktestVaR(Bitcoin.ret, VaR_ASm05, alpha, Lags = 4)
backtest05_SAVm <- BacktestVaR(Bitcoin.ret, VaR_SAVm05, alpha, Lags = 4)
backtest05_Gj <- BacktestVaR(Bitcoin.ret, VaR_GjG05, alpha, Lags = 4)
backtest05_Gjm <- BacktestVaR(Bitcoin.ret, VaR_GjGm05, alpha, Lags = 4)

IGm05<-c(mean(VaR_IGm05), backtest05_IGm$LRuc[2], backtest05_IGm$LRcc[2], backtest05_IGm$AE, backtest05_IGm$DQ[2], sd(VaR_IGm05))
ASm05<-c(mean(VaR_ASm05), backtest05_ASm$LRuc[2], backtest05_ASm$LRcc[2], backtest05_ASm$AE, backtest05_ASm$DQ[2], sd(VaR_ASm05))
SAVm05<-c(mean(VaR_SAVm05), backtest05_SAVm$LRuc[2], backtest05_SAVm$LRcc[2], backtest05_SAVm$AE, backtest05_SAVm$DQ[2], sd(VaR_SAVm05))
GJ05<-c(mean(VaR_GjG05), backtest05_Gj$LRuc[2], backtest05_Gj$LRcc[2], backtest05_Gj$AE, backtest05_Gj$DQ[2], sd(VaR_GjG05))
GJm05<-c(mean(VaR_GjGm05), backtest05_Gjm$LRuc[2], backtest05_Gjm$LRcc[2], backtest05_Gjm$AE, backtest05_Gjm$DQ[2], sd(VaR_GjGm05))

#
#
alphaa=0.01
backtest01_IGm <-BacktestVaR(Bitcoin.ret, VaR_IGm01, alphaa, Lags = 4)
backtest01_ASm <- BacktestVaR(Bitcoin.ret, VaR_ASm01, alphaa, Lags = 4)
backtest01_SAVm <- BacktestVaR(Bitcoin.ret, VaR_SAVm01, alphaa, Lags = 4)
backtest01_Gj <- BacktestVaR(Bitcoin.ret, VaR_GjG01, alphaa, Lags = 4)
backtest01_Gjm <- BacktestVaR(Bitcoin.ret, VaR_GjGm01, alphaa, Lags = 4)

IGm01<-c(mean(VaR_IGm01), backtest01_IGm$LRuc[2], backtest01_IGm$LRcc[2], backtest01_IGm$AE, backtest01_IGm$DQ[2], sd(VaR_IGm01))
ASm01<-c(mean(VaR_ASm01), backtest01_ASm$LRuc[2], backtest01_ASm$LRcc[2], backtest01_ASm$AE, backtest01_ASm$DQ[2], sd(VaR_ASm01))
SAVm01<-c(mean(VaR_SAVm01), backtest01_SAVm$LRuc[2], backtest01_SAVm$LRcc[2], backtest01_SAVm$AE, backtest01_SAVm$DQ[2], sd(VaR_SAVm01))
GJ01<-c(mean(VaR_GjG01), backtest01_Gj$LRuc[2], backtest01_Gj$LRcc[2], backtest01_Gj$AE, backtest01_Gj$DQ[2], sd(VaR_GjG01))
GJm01<-c(mean(VaR_GjGm01), backtest01_Gjm$LRuc[2], backtest01_Gjm$LRcc[2], backtest01_Gjm$AE, backtest01_Gjm$DQ[2], sd(VaR_GjGm01))

Tabella_Backtesting052<-data.frame(rbind(IGm=IGm05, ASm=ASm05, SAVm=SAVm05, GJ=GJ05, GJm=GJm05))
colnames(Tabella_Backtesting052) <- c("Mean", "LRuc", "LRcc", "AE", "DQ", "SD")

Tabella_Backtesting012<-data.frame(rbind(IGm=IGm01, ASm=ASm01, SAVm=SAVm01, GJ=GJ01, GJm=GJm01))
colnames(Tabella_Backtesting012) <- c("Mean", "LRuc", "LRcc", "AE", "DQ", "SD")

print(xtable(Tabella_Backtesting052, type = 'Latex'))
print(xtable(Tabella_Backtesting012, type = 'Latex'))



