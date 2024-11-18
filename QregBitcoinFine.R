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

#
## stazionarietà, adf H0: The time series is non-stationary
adf.test(Eth)
adf.test(Litecoin)
adf.test(SEP500)
adf.test(Bitcash)
adf.test(VIX) ##criticità, graficamente non stazionario, adf non robusto al break
adf.test(SPUSBT)
adf.test(Ripple)
##rimedio con le differenze
dSPUSBT <- diff(SPUSBT)
dSPUSBT <- c(dSPUSBT, mean(dSPUSBT))
dSPUSBT <- ts(dSPUSBT, frequency = 1)
lVIX <- log(VIX)
dlVIX <- diff(lVIX)
dlVIX <- c(dlVIX, mean(dlVIX))
#alternativa
dVIX <- diff(VIX)
dVIX <- c(dVIX, mean(dVIX))
## Granger test: Null Hypothesis (H0): Time series X does not cause
##time series Y to Granger-cause itself. grangertest(X, Y, order = 1)
grangertest(lBit7,Bitcoin)## riufuta----> causa
grangertest(lBit8,Bitcoin) ## accetta
grangertest(lBit9,Bitcoin) ## accetta
#
grangertest(lLit7,Bitcoin)
grangertest(lLit8,Bitcoin)
cor(Bitcoin, lLit7, method = 'pearson')
##visivamente
plot.ts(SPUSBT)
plot.ts(VIX)
##kpss H0: ts is stationary e preparo delle tabelle
kp1<-kpss.test(VIX)# ancora non stazionario
kp2<-kpss.test(dlVIX) ## ora si
kp3<-kpss.test(SPUSBT) ## quasi non stazionario all'1% con kpss
kp4<-kpss.test(dSPUSBT) ##ora si
kp5<-kpss.test(Eth)
kp6<-kpss.test(Litecoin)
kp7<-kpss.test(Bitcash)
kp8<-kpss.test(SEP500)
#estraggo i pvalue
pkp1<-kp1$p.value
pkp2<-kp2$p.value
pkp3<-kp3$p.value
pkp4<-kp4$p.value
pkp5<-kp5$p.value
pkp6<-kp6$p.value
pkp7<-kp7$p.value
pkp8<-kp8$p.value
##provo ad osservare le correlazioni, pearson con le covariate
cor3<-cor(Bitcoin, Eth, method = 'pearson') ##0.764, bene
cor5<-cor(Bitcoin, Bitcash, method = 'pearson') ####0.625041, inserisco anche questo
cor6<-cor(Bitcoin, SEP500, method = 'pearson')  ##0.276 non altissimo
cor2<-cor(Bitcoin, dSPUSBT, method = 'pearson') #### 0.02 ... 
cor4<-cor(Bitcoin, Litecoin, method = 'pearson') ##0.7417
cor1<-cor(Bitcoin, dlVIX, method = 'pearson') ## -0.2
##chiedo il round per la tabella
rcor1<-round(cor1, 3)
rcor2<-round(cor2, 3)
rcor3<-round(cor3, 3)
rcor4<-round(cor4, 3)
rcor5<-round(cor5, 3)
rcor6<-round(cor6, 3)
##tabella
ris_kpss=c(pkp1, pkp2, pkp3, pkp4, pkp5, pkp6, pkp7, pkp8)
ris_corr=c("...", rcor1, "...", rcor2, rcor3, rcor4, rcor5, rcor6)
tabella_3=data.frame(KPSS=ris_kpss, Pear.=ris_corr)
rownames(tabella_3) <- c("VIX","dlVIX","SPUSBT","dSPUSBT","Eth","Litecoin","Bitcash","SEP500")
print(xtable(tabella_3, type = 'Latex'))
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


