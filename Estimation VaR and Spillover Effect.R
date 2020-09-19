library(rugarch)
library(tseries)
library(forecast)
library(car)
library(MASS)
library(fGarch)
library(ggplot2)
library(MTS)
library(fBasics)

#Read data which were preprocessed already
data=read.csv("data_insert_full.csv",header = T)

datapre=data[7830:8291,]#prediction dataset
data1=data[1:7830,]   #model dataset
data$Date = as.Date(data$Date)
date = data1$Date[2:length(data1$Date)] #for return date

#return of crude oil
WTI=diff(log(data1[,2])) 
ts.plot(WTI)
Brent=diff(log(data1[,3]))
return = as.data.frame(cbind(WTI,Brent))
return$Date = date

basicStats(WTI)
basicStats(Brent)

#plot the price data (not stationary)
ggplot(data, aes(Date, WTI, color = "WTI", group = 1)) +
  geom_line()  + xlab("Date") + ylab("Price") +
  geom_line(aes(y = Brent, color = "Brent"))+theme_bw()

#plot the return data (not stationary)
ggplot(return, aes(Date, WTI, color = "WTI", group = 1)) +
  geom_line()  + xlab("Date") + ylab("Return") +
  geom_line(aes(y = Brent, color = "Brent"))+theme_bw()

######################AMAR model selection############################
#----------------WTI-------------------#
#ARMA model selection
acf(WTI)
adf.test(WTI)
kpss.test(WTI)
pacf(WTI)
auto.arima(WTI)
resultsdiff = matrix(0,nrow=5,ncol=5)
n=length(WTI)
logn=log(n)
# for (i in 1:5)
# {
#  for (j in 1:5)
#  {
#    fit =arima(WTI,order=c(i,0,j))
#    resultsdiff[i,j] = fit$aic+(logn-2)*(i+j)
#    }
# }
# which.min(resultsdiff) 

#choose arma(1,1)

#fitmodel
fit_WTI=arma(WTI,order=c(1,1))
res_WTI=fit_WTI$residuals[-1]
acf(res_WTI)
acf(res_WTI^2)
#----------------Brent-------------------#
#ARMA model selection for Brent
acf(Brent)
adf.test(Brent)
kpss.test(Brent)
pacf(Brent)
auto.arima(Brent)

#model criterial BIC
#resultsdiff = matrix(0,nrow=5,ncol=5)
#n=length(Brent)
#logn=log(n)
#for (i in 1:5)
#{
#  for (j in 1:5)
#  {
#    fit =?arima(WTI,order=c(i,0,j))
#    resultsdiff[i,j] = fit$aic+(logn-2)*(i+j)
#    }
#}
#which.min(resultsdiff) 

#choose arma(1,1)

#fit model
fit_Brent=arma(Brent,order=c(1,1))
res_Brent=fit_Brent$residuals[-1]
acf(res_Brent)
acf(res_Brent^2)

################garch model selection####################
#----------------WTI-------------------#
spec=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(2,1)),
                mean.model = list(armaOrder=c(1,1)),distribution.model = "sged")
fit_G_WTI=ugarchfit(spec,WTI) #fit model

mu=mean(WTI)
h=fit_G_WTI@fit$sigma^2 #conditional variance
#find distribution of return
fitdist(distribution = "sged", WTI, control=list())

#VaR_Down
alpha=0.05
z=qdist(distribution = "sged", alpha, skew = 0.93, shape = 1.34)
VaR_down_WTI=mu+z*sqrt(h) #mu的系数改为正

#VaR_Up
alpha1=0.95
z1=qdist(distribution = "sged", alpha1, skew = 0.93, shape = 1.34)
VaR_up_WTI=mu+z1*sqrt(h)

dat=as.data.frame(cbind(WTI,VaR_down_WTI,VaR_up_WTI))
dat$Date <- as.Date(date)
plot_var_WTI=ggplot(dat, aes(Date, WTI, color = "WTI", group = 1)) +
  geom_line()  + xlab("Date") + ylab("Return") +
  geom_line(aes(y = VaR_down_WTI, color = "VaR_down_WTI"))+
  geom_line(aes(y = VaR_up_WTI, color = "VaR_up_WTI"))
plot_var_WTI+theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#ts.plot(dat[c(1,2,3)],col=c("red","blue","green"))

#check failure time: 5%
failure_time_down=length(VaR_down_WTI[VaR_down_WTI>WTI])
failure_time_down/length(WTI)
failure_time_up=length(VaR_up_WTI[VaR_up_WTI<WTI])
failure_time_up/length(WTI)

#----------------Brent-------------------#
fit_G_Brent=ugarchfit(spec,Brent) #fit model

mu=mean(Brent)
h=fit_G_Brent@fit$sigma^2 #conditional variance

#find distribution of return
fitdist(distribution = "sged", Brent, control=list())

#VaR_Down
alpha=0.05
z=qdist(distribution = "sged", alpha, skew = 0.96, shape = 1.34)
VaR_down_Brent=mu+z*sqrt(h) #mu的系数改为正

#VaR_Up
alpha1=0.95
z1=qdist(distribution = "sged", alpha1, skew = 0.96, shape = 1.34)
VaR_up_Brent=mu+z1*sqrt(h)

dat=as.data.frame(cbind(Brent,VaR_down_Brent,VaR_up_Brent))
dat$Date <- as.Date(date)
ggplot(dat, aes(Date, Brent, color = "Brent", group = 1)) +
  geom_line()  + xlab("Date") + ylab("Return") +
  geom_line(aes(y = VaR_down_Brent, color = "VaR_down_Brent"))+
  geom_line(aes(y = VaR_up_Brent, color = "VaR_up_Brent"))
#ts.plot(dat[c(1,2,3)],col=c("red","blue","green"))

#check failure time: 5%
failure_time_down=length(VaR_down_Brent[VaR_down_Brent>Brent])
failure_time_down/length(Brent)
failure_time_up=length(VaR_up_Brent[VaR_up_Brent<Brent])
failure_time_up/length(Brent)
##################predict#############################
#prediction of WTI 
WTI_pre=diff(log(datapre[,2]))
pre_WTI=ugarchforecast(fit_G_WTI, n.ahead=length(WTI_pre))

mu_pre_WTI=pre_WTI@forecast$seriesFor
sigma_pre_WTI=pre_WTI@forecast$sigmaFor

VaR_down_pre_WTI=mu_pre_WTI+z*sigma_pre_WTI
VaR_up_pre_WTI=mu_pre_WTI+z1*sigma_pre_WTI

data_pre=as.data.frame(cbind(WTI_pre,VaR_down_pre_WTI,VaR_up_pre_WTI))
colnames(data_pre) <- c("WTI_pre","VaR_down_pre_WTI","VaR_up_pre_WTI")
date_pre <-datapre$Date[2:length(datapre$Date)]
data_pre$Date <- as.Date(date_pre)
ggplot(data_pre, aes(Date, WTI_pre, color = "WTI", group = 1)) +
  geom_line()  + xlab("Date") + ylab("Return") +
  geom_line(aes(y = VaR_down_pre_WTI, color = "VaR_down_pre_WTI"))+
  geom_line(aes(y = VaR_up_pre_WTI, color = "VaR_up_pre_WTI"))
#ts.plot(data_pre[c(1,2,3)],col=c("red","blue","green"))

#check for pre
failure_time_downpre=length(VaR_down_pre_WTI[VaR_down_pre_WTI>WTI_pre])
failure_time_downpre/length(WTI_pre)
failure_time_uppre=length(VaR_up_pre_WTI[VaR_up_pre_WTI<WTI_pre])
failure_time_uppre/length(WTI_pre)

#----------------Brent-------------------#
Brent_pre=diff(log(datapre[,3]))
pre_Brent=ugarchforecast(fit_G_Brent, n.ahead=length(Brent_pre))

mu_pre_Brent=pre_Brent@forecast$seriesFor
sigma_pre_Brent=pre_Brent@forecast$sigmaFor

VaR_down_pre_Brent=mu_pre_Brent+z*sigma_pre_Brent
VaR_up_pre_Brent=mu_pre_Brent+z1*sigma_pre_Brent

data_pre=as.data.frame(cbind(Brent_pre,VaR_down_pre_Brent,VaR_up_pre_Brent))
colnames(data_pre) <- c("Brent_pre","VaR_down_pre_Brent","VaR_up_pre_Brent")
data_pre$Date <- as.Date(date_pre)
ggplot(data_pre, aes(Date, WTI_pre, color = "Brent", group = 1)) +
  geom_line()  + xlab("Date") + ylab("Return") +
  geom_line(aes(y = VaR_down_pre_WTI, color = "VaR_down_pre_Brent"))+
  geom_line(aes(y = VaR_up_pre_WTI, color = "VaR_up_pre_Brent"))
#ts.plot(data_pre[c(1,2,3)],col=c("red","blue","green"))

#check for pre
failure_time_downpre=length(VaR_down_pre_Brent[VaR_down_pre_Brent>Brent_pre])
failure_time_downpre/length(Brent_pre)
failure_time_uppre=length(VaR_up_pre_Brent[VaR_up_pre_Brent<Brent_pre])
failure_time_uppre/length(Brent_pre)


#####################spillover#########################
#return for WTI and Brent
d <- cbind(WTI,Brent)
MTSplot(d)

ccm(d)

m0=VARorder(d)
names(m0)
m0$Mstat

m1=VAR(d,5) 
VARpred(m1, 8)
m2=refVAR(m1,thres=1.96)
VARirf(m1$Phi, m1$Sig, orth=FALSE)

gt_rt <- GrangerTest(d,p=5)

#####################################
z_down_WTI <- ifelse(WTI<VaR_down_WTI, 1, 0) #WTI down dummy variable
z_up_WTI <- ifelse(WTI>VaR_up_WTI, 1, 0) #WTI up dummy variable
z_down_Brent <- ifelse(Brent<VaR_down_Brent, 1, 0) #Brent down dummy variable
z_up_Brent <- ifelse(Brent>VaR_up_Brent, 1, 0) #Brent up dummy variable

############# VaR down ##############
down <- cbind(z_down_WTI,z_down_Brent)
#ccm
ccm(down)
#VAR 
m_down <- VARorder(down)
m_down$Mstat
d_var=VAR(down,1) 
#grangertest
gt_d_1 <- GrangerTest(down,locInput = 1) #y=WTI
gt_d_2 <- GrangerTest(down,locInput=2) #y=Brent

############ VaR up ################
up <- cbind(z_up_WTI,z_up_Brent)
#ccm
ccm(up)
#VAR
m_up <- VARorder(up)
m_up$Mstat
u_var=VAR(up,1) 
#grangertest
gt_u_1 <- GrangerTest(up,locInput = 1) #y=WTI
gt_u_1 <- GrangerTest(up,locInput = 2) #y=Brent







