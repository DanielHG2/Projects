setwd("C:\\Users\\Daniel192\\Desktop\\TESINA SERIE STORICHE\\ELABORAZIONI\\Primo")
library(readxl)
library(tseries)
library(e1071)
library(xtable)
library(lmtest)
##importo i dati e definisco la serie storica da analizzare
ret_crypto_index_comm <- read_excel("C:/Users/Daniel192/Desktop/TESINA SERIE STORICHE/Dati/crypto_index_comm/ret_crypto_index_comm.xlsx")
View(ret_crypto_index_comm)
Bitcoin = ts(ret_crypto_index_comm[ ,2], frequency=1,  start= 2017-27-07)
##definisco delle variabili sulle caratteristiche della serie 
summary(Bitcoin)
Quartili <- quantile(Bitcoin, probs=seq(0, 1, 0.25))
Media <- c(mean(Bitcoin))
Deviazione_standard <- c(sd(Bitcoin))
Skewness <- c(skewness(Bitcoin))
Kurtosis <- c(kurtosis(Bitcoin))
##costruisco le tabelle con le variabili generate
Tabella_1 <- data.frame(Media = Media, SD = Deviazione_standard, Skewness = Skewness, Kurtosis = Kurtosis)
rownames(Tabella_1) <- c("Bitcoin") #riferimento ai parametri di una normale
Tabella_quartili <- data.frame (Quartili = Quartili)
Tabella_quartili_t <- t(Tabella_quartili)
## digitabili direttamente in Latex
print(xtable(Tabella_statistiche, type = 'Latex'))
print(xtable(Tabella_quartili_t, type = 'Latex'))
## plot riguardanti la serie storica 
plot.ts(Bitcoin) ##volatility clustering?
par(mfrow = c(1, 2))
acf(Bitcoin) ##segni di stazionarietà
pacf(Bitcoin)
hist(Bitcoin, breaks=150, probability=TRUE) ##esplicita diff dalla normale
curve(dnorm(x, mean=mean(Bitcoin), sd=sd(Bitcoin)), lwd= 2, col="blue", add=TRUE)
par(mfrow = c(1, 2))
qqnorm(Bitcoin)
qqline(Bitcoin, col='red') ##sulla diagonale una normale, differisce agli estremi
boxplot(Bitcoin, main="Boxplot", ylab="Bitcoin") ##presenza di outliers
#effettuo dei test sulla serie storica
ADF_Bitcoin <- adf.test(Bitcoin) ##già stazionari poichè sono rendimenti
test_ADF_Bit <- ADF_Bitcoin$statistic
p_ADF_Bit <- ADF_Bitcoin$p.value
JB_Bitcoin <- jarque.bera.test(Bitcoin)##differisce da una normale per via delle caratteristiche di Bitcoin, leptocurtica ecc
test_JB_Bit <- JB_Bitcoin$statistic
p_JB_Bit <- JB_Bitcoin$p.value
BP_Bitcoin <- bptest(Bitcoin)
LB_Bitcoin <- Box.test(Bitcoin, lag = length(Bitcoin), type = "Ljung-Box") ## possibilità di modellare
test_LB_Bit <- LB_Bitcoin$statistic
p_LB_Bit <- LB_Bitcoin$p.value
##esporto i risultati dei test in una tabella
Tabella_2 <- data.frame(ADF= test_ADF_Bit, pval_ADF=p_ADF_Bit, JB = test_JB_Bit, pval_JB = p_JB_Bit)
rownames(Tabella_2) <- c("Bitcoin")
print(xtable(Tabella_2, type = 'Latex'))
##
##H0 no autocorrelazione a quel lag
Box.test(Bitcoin, lag=1, type="Ljung-Box")
sc1<-Box.test(Bitcoin, lag=1, type="Ljung-Box")
sc2<-Box.test(Bitcoin, lag=2, type="Ljung-Box")
sc3<-Box.test(Bitcoin, lag=7, type="Ljung-Box")
sc4<-Box.test(Bitcoin, lag=8, type="Ljung-Box")
sc5<-Box.test(Bitcoin, lag=9, type="Ljung-Box")
sc6<-Box.test(Bitcoin, lag=15, type="Ljung-Box")
##pvalue
pval_L1 <- sc1$p.value
pval_L2 <- sc2$p.value
pval_L7 <- sc3$p.value
pval_L8 <- sc4$p.value
pval_L9 <- sc5$p.value
pval_L15 <- sc6$p.value
###
Tabella_3 <- data.frame(pval_L1= pval_L1, pval_L2=pval_L2, ...="...", pval_L7=pval_L7, pval_L8=pval_L8, pval_L9=pval_L9, ....= "..." , pval_L15=pval_L15 )
print(xtable(Tabella_3, type = 'Latex'))
