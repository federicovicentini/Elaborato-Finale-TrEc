#Libraries necessary for the code to work

library(foreign)
library(sandwich)
library(lmtest)
library(AER)
library(stargazer)
library(tseries)  
library(dynlm)
library(forecast)
library(quantmod)
library(tsapp)
tilde <- rawToChar(as.raw(126)) #Command needed to use the Tilde character in the coding



#Creating a data frame for Economic Uncertainty data

EU=read.csv("Categorical_EPU_Data-ritaglio.csv",sep=";")
EU[,1]=as.Date.character(EU[,1], "%d/%m/%Y")
names(EU)[1]="DATE"
names(EU)[2]="EPU"
names(EU)[3]="MP"
names(EU)[4]="FP"
names(EU)[5]="TAX"
names(EU)[6]="GOV"
names(EU)[7]="HEALTH"
names(EU)[8]="DEFENSE"
names(EU)[9]="ENTITLEMENT"
names(EU)[10]="REG"
names(EU)[11]="FINREG"
names(EU)[12]="TRADE"
names(EU)[13]="DEBTCURRENCY"

#Adding unemployment data

unemployment=read.csv("UNRATE-tagliato.csv")
EU[14]=unemployment$UNRATE
names(EU)[14]="UNEMPLOYMENT"

#Adding the Monetary Policy shocks proxy

proxy=read.csv("instruments_web-ritaglio.csv",sep=";")
names(proxy)[1]="DATE"
names(proxy)[2]="IS-FOMC"
names(proxy)[3]="IV4"
names(proxy)[4]="IV5"

EU[15]=proxy$IV4
names(EU)[15]="PROXY"


#First Regression
regressionbase=dynlm(EPU~Lag(EPU,1)+UNEMPLOYMENT+PROXY,data=EU)
HAC=coeftest(regressionbase, vcov. = vcovHAC(regressionbase)) #This is the code for HAC robust standard errors
ses=list(HAC[,2])
ps=list(HAC[,4])
stargazer(regressionbase,type="latex", se = ses, p = ps, p.auto = F)
stargazer(regressionbase,type="text", se = ses, p = ps, p.auto = F)
plot(Lag(EU$EPU)+EU$UNEMPLOYMENT+EU$PROXY,EU$EPU)
abline(regressionbase)
plot(regressionbase)




#Cutting the sample in 2008
regression2008=dynlm(EPU~Lag(EPU,1)+UNEMPLOYMENT+PROXY,data=EU[1:204,])
HAC08=coeftest(regression2008, vcov. = vcovHAC(regression2008))
ses=list(HAC08[,2])
ps=list(HAC08[,4])
stargazer(regression2008,type="latex", se = ses, p = ps, p.auto = F)
stargazer(regression2008,type="text", se = ses, p = ps, p.auto = F)
plot(Lag(EU$EPU[1:204])+EU$UNEMPLOYMENT[1:204]+EU$PROXY[1:204],EU$EPU[1:204])
abline(regression2008)
plot(regression2008)




#Using VXO as the dependent variable
newregression=dynlm(VXO~Lag(VXO,1)+UNEMPLOYMENT+PROXY,data=EU)
newHAC=coeftest(newregression, vcov. = vcovHAC(newregression))
ses=list(newHAC[,2])
ps=list(newHAC[,4])
stargazer(newregression, type = "text", se = ses, p = ps, p.auto = F)

stargazer(newregression, type = "latex", se = ses, p = ps, p.auto = F)

plot(EU$DATE,EU$VXO)
lines(EU$DATE,EU$VXO)
times=EU$DATE
times=times[-c(228)]
lines(times,newregression[["fitted.values"]], col="blue")




#Cutting the sample in 2008
newregression2008=dynlm(VXO~Lag(VXO,1)+UNEMPLOYMENT+PROXY,data=EU[1:204,])
newHAC2008=coeftest(newregression2008, vcov. = vcovHAC(newregression2008))
ses=list(newHAC2008[,2])
ps=list(newHAC2008[,4])
stargazer(newregression2008, type = "text", se = ses, p = ps, p.auto = F)
stargazer(newregression2008, type = "latex", se = ses, p = ps, p.auto = F)



#Adding DGS10 time series data
DGS10=read.csv("DGS10.csv",sep=",")
names(DGS10)[1]="DATE"
names(DGS10)[2]="DGS10"

EU[18]=DGS10[2]



#New regression with DGS10 as explanatory variable and EPU as dependent variable
newregressionbond=dynlm(EPU~Lag(EPU,1)+UNEMPLOYMENT+DGS10,data=EU)

HACbond=coeftest(newregressionbond, vcov. = vcovHAC(newregressionbond))
ses=list(HACbond[,2])
ps=list(HACbond[,4])
stargazer(newregressionbond, type = "text", se = ses, p = ps, p.auto = F)

plot(EU$DATE,EU$EPU)
lines(EU$DATE,EU$EPU)
times=EU$DATE
times=times[-c(228)]
lines(times,newregressionbond[["fitted.values"]], col="blue")

stargazer(newregressionbond, type = "latex", se = ses, p = ps, p.auto = F)


#New regression with DGS10 as explanatory variable and VXO as dependent variable
newregressionbondVXO=dynlm(VXO~Lag(VXO,1)+UNEMPLOYMENT+DGS10,data=EU)

HACbondVXO=coeftest(newregressionbondVXO, vcov. = vcovHAC(newregressionbondVXO))
ses=list(HACbondVXO[,2])
ps=list(HACbondVXO[,4])
stargazer(newregressionbondVXO, type = "text", se = ses, p = ps, p.auto = F)

plot(EU$DATE,EU$VXO)
lines(EU$DATE,EU$VXO)
times=EU$DATE
times=times[-c(228)]
lines(times,newregressionbondVXO[["fitted.values"]], col="blue")

stargazer(newregressionbondVXO, type = "latex", se = ses, p = ps, p.auto = F)



#Cutting the sample in 2008/06
newregressionbondVXO2008=dynlm(VXO~Lag(VXO,1)+UNEMPLOYMENT+DGS10,data=EU[1:210,])
HACbondVXO2008=coeftest(newregressionbondVXO2008, vcov. = vcovHAC(newregressionbondVXO2008))
ses=list(HACbondVXO2008[,2])
ps=list(HACbondVXO2008[,4])
stargazer(newregressionbondVXO2008, type = "text", se = ses, p = ps, p.auto = F)
stargazer(newregressionbondVXO2008, type = "latex", se = ses, p = ps, p.auto = F)





#Cutting the sample at the end of 2002
newregressionbondVXO2002=dynlm(VXO~Lag(VXO,1)+UNEMPLOYMENT+DGS10,data=EU[1:144,])
HACbondVXO2002=coeftest(newregressionbondVXO2002, vcov. = vcovHAC(newregressionbondVXO2002))
ses=list(HACbondVXO2002[,2])
ps=list(HACbondVXO2002[,4])
stargazer(newregressionbondVXO2002, type = "text", se = ses, p = ps, p.auto = F)

stargazer(newregressionbondVXO2002, type = "latex", se = ses, p = ps, p.auto = F)

plot(EU$DATE[1:144],EU$VXO[1:144])
lines(EU$DATE,EU$VXO)
times=EU$DATE
times=times[-c(144:228)]
lines(times,newregressionbondVXO2002[["fitted.values"]], col="blue")




#Adding the Federal Funds Rate data
FEDFUNDS=read.csv("FEDFUNDS.csv")
EU[19]=FEDFUNDS[2]
names(EU)[19]="FEDFUNDS"




#New regression with FEDFUNDS as explanatory variable and VXO as dependent variable
regressionfed=dynlm(VXO~Lag(VXO,1)+UNEMPLOYMENT+FEDFUNDS,data=EU[1:144,])
HACfed=coeftest(regressionfed, vcov.=vcovHAC(regressionfed))
ses=list(HACfed[,2])
ps=list(HACfed[,4])
stargazer(regressionfed, type = "text", se = ses, p = ps, p.auto = F)

stargazer(regressionfed, type = "latex", se = ses, p = ps, p.auto = F)

plot(EU$DATE[1:144],EU$VXO[1:144])
lines(EU$DATE,EU$VXO)
times=EU$DATE
times=times[-c(144:228)]
lines(times,regressionfed[["fitted.values"]], col="blue")




















