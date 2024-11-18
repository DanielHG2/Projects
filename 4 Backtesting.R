setwd("C:\\Users\\Daniel192\\Desktop\\TESINA SERIE STORICHE\\ELABORAZIONI\\Terzo-2")
library(xts)
library(quantreg)
library(readxl)
library(tidyverse)
library(ggplot2)
require(quantmod)
library(GAS)
library(xtable)
library(PerformanceAnalytics)
library(quantmod)
source("Funzioni CAViaR.R")
ret_crypto_index_comm <- read_excel("ret_crypto_index_comm.xlsx")
ret_crypto_index_comm$Date <- as.Date(ret_crypto_index_comm$Date)
calcolo_VAR_definitivo <- read_excel("calcoloVAR_definitivo.xlsx")
calcolo_VAR_definitivo$Date <- as.Date(calcolo_VAR$Date)
Bitcoin.ret <- xts(ret_crypto_index_comm[, 2], order.by = ret_crypto_index_comm$Date)
Time = index(Bitcoin.ret)
range(Time)


tau = 0.05 # quantile level
ret = coredata(Bitcoin.ret)
n.start = 10 # number of restarts
fit_AD_10 = est.CAViaR(ret = Bitcoin.ret, tau = tau, model = "AD", n.start = n.start, G = 10) 
fit_AD_1e10 = est.CAViaR(ret = Bitcoin.ret, tau = tau, model = "AD", n.start = n.start, G = 1e10) 
fit_SAV = est.CAViaR(ret = Bitcoin.ret, tau = tau, model = "SAV", n.start = n.start) 
fit_AS = est.CAViaR(ret = Bitcoin.ret, tau = tau, model = "AS", n.start = n.start) 
fit_IG = est.CAViaR(ret = Bitcoin.ret, tau = tau, model = "IG", n.start = n.start) 
###valori che già posseggo nell'excel perciò posso
Qreg <- (data=calcolo_VAR$VaR_QuantileReg)

VaR_Bitcoin_SAV05<- (data=calcolo_VAR_definitivo$VaR_BItcoin_SAV05)


VaR_Bitcoin_IG05<- (data=calcolo_VAR_definitivo$VaR_BItcoin_IG05)

VaR_Bitcoin_AD05<- (data=calcolo_VAR_definitivo$VaR_BItcoin_AD05)

VaR_Bitcoin_AS05<- (data=calcolo_VAR_definitivo$VaR_BItcoin_AS05)

VaR_Qreg05<- (data=calcolo_VAR_definitivo$VaR_Qreg_05)

VaR_Qregalt05<- (data=calcolo_VAR_definitivo$VaR_Qregalt_05)


##############################################################
VaR_Bitcoin_SAV01<- (data=calcolo_VAR_definitivo$VaR_BItcoin_SAV01)#

VaR_Bitcoin_IG01<- (data=calcolo_VAR_definitivo$VaR_BItcoin_IG01)#

VaR_Bitcoin_AD01<- (data=calcolo_VAR_definitivo$VaR_BItcoin_AD01)#

VaR_Bitcoin_AS01<- (data=calcolo_VAR_definitivo$VaR_BItcoin_AS01)#

VaR_Qreg01<- (data=calcolo_VAR_definitivo$VaR_Qreg_01)#

VaR_Qregalt01<- (data=calcolo_VAR_definitivo$VaR_Qregalt_01)#

VaR_Qreg01<-na.omit(VaR_Qreg01)
VaR_Qreg01<-c(VaR_Qreg01, mean(VaR_Qreg01))

VaR_Qregalt01<-na.omit(VaR_Qregalt01)
VaR_Qregalt01<-c(VaR_Qregalt01, mean(VaR_Qregalt01))




plot.ts(Bitcoin.ret)
lines(VaR_Bitcoin_SAV05, type = "l", col = "blue")
lines(VaR_Bitcoin_AD05, type = "l", col = "orange")
lines(VaR_Bitcoin_AS05, type = "l", col = "green")
lines(VaR_Bitcoin_IG05, type = "l", col = "violet")

legend("bottomleft", legend = c("VaR_SAV", "VaR_AD", "VaR_AS", "VaR_IG"),
       col = c("blue","orange", "green", "yellow","violet"), 
       lty = 1, 
       cex = 0.8)


plot.ts(Bitcoin.ret)
lines(VaR_Qreg05, type = "l", col = "red")

legend("topright", 
       legend = c("VaR_AD_plot", "VaR_AS_plot", "VaR_IG_plot", "Qreg"),
       col = c("orange", "green", "yellow","violet"), 
       lty = 1, 
       cex = 0.8)



VaR_Qreg05<-na.omit(VaR_Qreg05)
VaR_Qreg05<- c(VaR_Qreg05, mean(VaR_Qreg05))

VaR_Qregalt05<-na.omit(VaR_Qregalt05)
VaR_Qregalt05<- c(VaR_Qregalt05, mean(VaR_Qregalt05))
VaR_Qregalt01<-na.omit(VaR_Qregalt01)
VaR_Qregalt01<- c(VaR_Qregalt01, mean(VaR_Qregalt01))

mean(VaR_Bitcoin_IG05)
mean(VaR_Bitcoin_AD05)
mean(VaR_Bitcoin_AS05)
mean(VaR_Bitcoin_SAV05)
mean(VaR_Bitcoin_Qreg05)
mean(VaR_Bitcoin_Qregalt05)


mean(VaR_Bitcoin_AD01)
mean(VaR_Bitcoin_AS01)
mean(VaR_Bitcoin_SAV01)
mean(VaR_Bitcoin_IG01)
mean(VaR_Qreg01)
mean(VaR_Qregalt01)


alpha=0.05
backtest05_AD <-BacktestVaR(Bitcoin.ret, VaR_Bitcoin_AD05, alpha, Lags = 4)
backtest05_AS <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_AS05, alpha, Lags = 4)
backtest05_SAV <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_SAV05, alpha, Lags = 4)
backtest05_IG <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_IG05, alpha, Lags = 4)
backtest05_Qreg <- BacktestVaR(Bitcoin.ret, VaR_Qreg05, alpha, Lags = 4)
backtest05_Qregalt <- BacktestVaR(Bitcoin.ret, VaR_Qregalt05, alpha, Lags = 4)

names(backtest05_AD)

backtest05_AD$LRuc[2]
##chiedo solo i pvalue delle statistiche
##TAU=0.05
ad05<-c(mean(VaR_Bitcoin_AD05), backtest05_AD$LRuc[2], backtest05_AD$LRcc[2], backtest05_AD$AE, backtest05_AD$DQ[2], sd(VaR_Bitcoin_AD05))
as05<-c(mean(VaR_Bitcoin_AS05), backtest05_AS$LRuc[2], backtest05_AS$LRcc[2], backtest05_AS$AE, backtest05_AS$DQ[2], sd(VaR_Bitcoin_AS05))
sav05<-c(mean(VaR_Bitcoin_SAV05), backtest05_SAV$LRuc[2], backtest05_SAV$LRcc[2], backtest05_SAV$AE, backtest05_SAV$DQ[2], sd(VaR_Bitcoin_SAV05))
ig05<-c(mean(VaR_Bitcoin_IG05), backtest05_IG$LRuc[2], backtest05_IG$LRcc[2], backtest05_IG$AE, backtest05_IG$DQ[2], sd(VaR_Bitcoin_IG05))
qr05<-c(mean(VaR_Qreg05), backtest05_Qreg$LRuc[2], backtest05_Qreg$LRcc[2], backtest05_Qreg$AE, backtest05_Qreg$DQ[2], sd(VaR_Qreg05))
qra05<-c(mean(VaR_Qregalt05), backtest05_Qregalt$LRuc[2], backtest05_Qregalt$LRcc[2], backtest05_Qregalt$AE, backtest05_Qregalt$DQ[2], sd(VaR_Qregalt05))


alpha1=0.01
backtest01_AD <-BacktestVaR(Bitcoin.ret, VaR_Bitcoin_AD01, alpha1, Lags = 4)
backtest01_AS <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_AS01, alpha1, Lags = 4)
backtest01_SAV <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_SAV01, alpha1, Lags = 4)
backtest01_IG <- BacktestVaR(Bitcoin.ret, VaR_Bitcoin_IG01, alpha1, Lags = 4)
backtest01_Qreg <- BacktestVaR(Bitcoin.ret, VaR_Qreg01, alpha1, Lags = 4)
backtest01_Qregalt <- BacktestVaR(Bitcoin.ret, VaR_Qregalt01, alpha1, Lags = 4)

#TAU=0.01
ad01<-c(mean(VaR_Bitcoin_AD01), backtest01_AD$LRuc[2], backtest01_AD$LRcc[2], backtest01_AD$AE, backtest01_AD$DQ[2], sd(VaR_Bitcoin_AD01))
as01<-c(mean(VaR_Bitcoin_AS01), backtest01_AS$LRuc[2], backtest01_AS$LRcc[2], backtest01_AS$AE, backtest01_AS$DQ[2], sd(VaR_Bitcoin_AS01))
sav01<-c(mean(VaR_Bitcoin_SAV01), backtest01_SAV$LRuc[2], backtest01_SAV$LRcc[2], backtest01_SAV$AE, backtest01_SAV$DQ[2], sd(VaR_Bitcoin_SAV01))
ig01<-c(mean(VaR_Bitcoin_IG01), backtest01_IG$LRuc[2], backtest01_IG$LRcc[2], backtest01_IG$AE, backtest01_IG$DQ[2], sd(VaR_Bitcoin_IG01))
qr01<-c(mean(VaR_Qreg01), backtest01_Qreg$LRuc[2], backtest01_Qreg$LRcc[2], backtest01_Qreg$AE, backtest01_Qreg$DQ[2], sd(VaR_Qreg01))
qra01<-c(mean(VaR_Qregalt01), backtest01_Qregalt$LRuc[2], backtest01_Qregalt$LRcc[2], backtest01_Qregalt$AE, backtest01_Qregalt$DQ[2], sd(VaR_Qregalt01))


Tabella_Backtesting05<-data.frame(rbind(AD=ad05, AS=as05, SAV=sav05, IG=ig05, QR=qr05, QR2=qra05 ))
colnames(Tabella_Backtesting05) <- c("Mean", "LRuc", "LRcc", "AE", "DQ", "SD")

Tabella_Backtesting01<-data.frame(rbind(AD=ad01, AS=as01, SAV=sav01, IG=ig01, QR=qr01, QR2=qra01 ))
colnames(Tabella_Backtesting01) <- c("Mean", "LRuc", "LRcc", "AE", "DQ", "SD")

print(xtable(Tabella_Backtesting05, type = 'Latex'))
print(xtable(Tabella_Backtesting01, type = 'Latex'))
