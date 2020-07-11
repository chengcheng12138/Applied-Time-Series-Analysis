# explain the data 
my_data <- read.table("~/Desktop/GD.dat.txt", quote="\"", comment.char="")
dim(my_data)
summary(my_data)
boxplot(my_data)
# understand the nature of the variations

par(mfrow=c(1,2))
gun<-my_data$V1
deaths<-my_data$V2
ts.plot(gun,gpars=list(xlab="time",
  ylab="gun", main="Figure 1.1 Time Series of GUN SALES", lty=c(1:3)))
ts.plot(deaths,gpars=list(xlab="time",
   ylab="firearm deaths", main="Figure 1.2 Time Series of DEATHS", lty=c(1:3)))


#stationary #transformation(s) 
# A stationary series is one in which the properties – 
#mean, variance and covariance, do not vary with time.

##transfor data
##log
# I use log transform, but it is still non
gun_log<-log(gun)
plot.ts(gun_log,xlab="time",
        ylab="gun", main="Figure 2.1 LOG transformed GUN SALES")

deaths_log<-log(deaths)
plot.ts(deaths_log,xlab="time",
        ylab="firearm deaths", main="Figure 2.2 LOG transformed firearm deaths")

#take diff
#We can take differences of a time series as well. 
#This is equivalent to taking the difference between 
#each value and its previous value:
diff1<-diff(gun,lag = 1)
tm1<-cbind(gun,diff1)
head(tm1)
plot.ts(tm1,
        main="Figure 3.1 take differences transformed GUN")


diff2<-diff(deaths,lag = 1)
tm2<-cbind(deaths,diff2)
head(tm2)
plot.ts(tm2,main="Figure 3.2 take differences transformed deaths")
#Often in time series analysis and modeling, we will want to transform data. There are a number of different functions that can be used to 
#transform time series data 
#such as the difference, log, moving average, percent change, lag, or cumulative sum. 
#These type of function are useful for both visualizing time series data and for modeling time series. 
#For example, the moving average function can be used to more easily visualize a high-variance time series and is also a critical part the ARIMA family of models. 
#Functions such as the difference, percent change, and log difference are helpful for 
#making non-stationary data stationary.

#As you can see, taking a difference is an effective way to 
#remove a trend and make a time series stationary.


#Fit an ARIMA model for each one of the series. 
#Use a model selection criterion to select the best models.

##acf and pacf of gun
par(mfrow=c(2,1))
acf(diff(gun_log),48)
pacf(diff(gun_log),48)

##acf and pacf of death
par(mfrow=c(2,1))
acf(diff(deaths_log),48)
pacf(diff(deaths_log),48)

###ccf
par(mfrow=c(1,1))
ccf(diff(gun_log), diff(deaths_log),laf.max=20, ylab="ccf")

## modle of gun seal
par(mfrow = c(1,2))
m1_gun<-sarima(diff(gun_log), p=0, d=0, q=2)
m2_gun<-sarima(diff(gun_log), p=0, d=0, q=2,P=1,D=0,Q=0,S=12)

##modle of death
m1_deaths<-sarima(diff(deaths_log), p=0, d=1, q=3)
m2_deaths<-sarima(diff(deaths_log), p=0, d=1, q=2,P=2,D=0,Q=2,S=12)


# modle selection
c(m1_gun$AIC,m1_gun$AICc,m1_gun$BIC)
c(m2_gun$AIC,m2_gun$AICc,m2_gun$BIC)


c(m1_deaths$AIC,m1_deaths$AICc,m1_deaths$BIC)
c(m2_deaths$AIC,m2_deaths$AICc,m2_deaths$BIC)
##？questions : if we do the log transformation the data is still non stationary
## after we de the diff of log data ot orangional data??

##for the gun and death data , do we have the sensonal or not
# if we use arima modle, we neef to use data or diff data



sarima(diff(deaths_log),2,1,2,1,0,0,12,xreg=rbind(diff(gun_log)))

trend=time(deaths_log)
fit = lm(deaths_log~gun_log)
summary(fit)

par(mfrow=c(1,2))
acf(resid(fit))
pacf(resid(fit))

