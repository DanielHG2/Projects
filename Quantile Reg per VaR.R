setwd("C:\\Users\\Daniel192\\Desktop\\TESINA SERIE STORICHE\\ELABORAZIONI\\Secondo -2")
library(readxl)
library(openxlsx)
library(quantreg)
library(tseries)
library(lmtest)
library(xtable)
library(quantmod)
##importo da excel
ret_crypto_index_comm <- read_xlsx("C:\\Users\\Daniel192\\Desktop\\TESINA SERIE STORICHE\\ELABORAZIONI\\Secondo -2\\ret_crypto_index_comm.xlsx")
ret_crypto_index_comm <- na.omit(ret_crypto_index_comm)
##definisco le variabili
Bitcoin <- ret_crypto_index_comm$`Bitcoin (BTC)`[1:1347]
Eth <- ret_crypto_index_comm$`Ethereum (ETH)`[1:1347]
Bitcash <- ret_crypto_index_comm$`Bitcoin cash (BCH)`[1:1347]
SEP500 <- ret_crypto_index_comm$`S&P500 (GSPC)`[1:1347]
Litecoin <- ret_crypto_index_comm$`Litecoin (LTC)`[1:1347]
SPUSBT <- ret_crypto_index_comm$`S&P US Treasury Bond (SPUSBT)`[1:1347]
VIX <- ret_crypto_index_comm$`VIX`[1:1347]
#genero delle variabili lag
lBit7 <- na.omit(Lag(Bitcoin, 7))
lBit8 <- na.omit(Lag(Bitcoin, 8))
lBit7 <- c(mean(lBit7), mean(lBit7), mean(lBit7), mean(lBit7), mean(lBit7), mean(lBit7), mean(lBit7), lBit7 )
lBit8 <- c(mean(lBit8), mean(lBit8), mean(lBit8), mean(lBit8), mean(lBit8), mean(lBit8), mean(lBit8), mean(lBit8), lBit8 )

##visivamente
plot.ts(SPUSBT)
plot.ts(VIX)

##provo ad osservare le correlazioni, pearson con le covariate
cor3<-cor(Bitcoin, Eth, method = 'pearson') ##0.764, bene
cor5<-cor(Bitcoin, Bitcash, method = 'pearson') ####0.625041, inserisco anche questo
cor6<-cor(Bitcoin, SEP500, method = 'pearson')  ##0.276 non altissimo
cor4<-cor(Bitcoin, Litecoin, method = 'pearson') ##0.7417
##chiedo il round per la tabella
rcor3<-round(cor3, 3)
rcor4<-round(cor4, 3)
rcor5<-round(cor5, 3)
rcor6<-round(cor6, 3)

####correlazioni con i lag
cor(Bitcoin, lBit7, method = 'pearson') ### 0.06
cor(Bitcoin, lBit8, method = 'pearson') ## altre variabili stesso pearson
cor(Bitcoin, lBit9, method = 'pearson')
cor(Bitcoin, lBit1, method = 'pearson')
###Regressione quantile.
df_Bitcoin <- data.frame(Bitcoin, lBit7, lBit8)
df_Bitcoin1 <- data.frame(Bitcoin, Eth, Litecoin, Bitcash, SEP500)
#modello dai data frame
quantile_rg <- rq(Bitcoin ~ lBit7+lBit8, tau = seq(0.01, 0.99, by = 0.01), data = df_Bitcoin)
summary(quantile_rg)
plot(summary(quantile_rg))
plot(summary(quantile_rg), parm="lBit9")
##alternativo
QuantileReg <- rq(Bitcoin ~ Eth+Litecoin+Bitcash+SEP500 , tau = seq(0.01, 0.99, by = 0.01), data = df_Bitcoin1 )
summary(QuantileReg)
plot(summary(QuantileReg))
sum <- summary(QuantileReg)
##solo i coefficienti
plot(summary(QuantileReg), parm="Eth")
plot(summary(QuantileReg), parm="Litecoin")
plot(summary(QuantileReg), parm="Bitcash")
plot(summary(QuantileReg), parm="SEP500")

plot(summary(QuantileReg), parm="SPUSBT")
plot(summary(QuantileReg), parm="dlVIX")
##predict del VaR nei due quantili 0.05 e 0.01
Qreg05<-rq(Bitcoin ~ Eth+Litecoin+Bitcash+SEP500, tau = 0.05, data = df_Bitcoin1 )
Qreg01<-rq(Bitcoin ~ Eth+Litecoin+Bitcash+SEP500, tau = 0.01, data = df_Bitcoin1 )
summary(Qreg05)
#
Qregalt05<- rq(Bitcoin ~ lBit7+lBit8, tau = 0.05, data = df_Bitcoin)
Qregalt01<- rq(Bitcoin ~ lBit7+lBit8, tau = 0.01, data = df_Bitcoin)
summary(Qregalt05)
#
VaR_Qreg_05<-predict(Qreg05)
VaR_Qreg_01<-predict(Qreg01)
#
VaR_Qregalt_05<-predict(Qregalt05)
VaR_Qregalt_01<-predict(Qregalt01)
#
VaR_Qreg<-data.frame(VaR_Qreg_05,VaR_Qreg_01,VaR_Qregalt_05,VaR_Qregalt_01)
#
write.xlsx(VaR_Qreg, file = "C:\\Users\\Daniel192\\Desktop\\TESINA SERIE STORICHE\\ELABORAZIONI\\Secondo -2\\VarQREG.xlsx", sheetName = "Sheet1", colNames = TRUE, rowNames = FALSE)
#
names(quantile_rg)
names(QuantileReg)



plot(Bitcoin ~ Litecoin, data = df_Bitcoin1, pch = 16, main = "Bitcoin ~ Litecoin ", xlab = "Litecoin", ylab = "Bitcoin")
abline(rq(Bitcoin ~ Litecoin, tau = 0.01, data = df_Bitcoin1), col = "red", lty = 1)
abline(rq(Bitcoin ~ Litecoin, tau = 0.99, data = df_Bitcoin1), col = "red", lty = 1)
abline(lm(Bitcoin ~ Litecoin,data = df_Bitcoin1), col = "blue", lty = 5, lwd = 4)


