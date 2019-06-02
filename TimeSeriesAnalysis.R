#set the working directory
getwd()
setwd("C:\\Users\\Tejaswini\\Desktop\\Advanced Business Analytics\\Assignment")

# Load the data set
library(data.table)
data <- fread("ATT_Twitter.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))

#install packages required to do time series analysis
#install.packages("ts", repos = "https://cran.r-project.org")
#install.packages("forecast", repos = "https://cran.r-project.org")

head(data)
tail(data)
colnames(data)

library(forecast)
#convert the data into time series object
x = ts(data[,2])
print(x)
plot(x)

#z = log10(x)
#plot(head(z,30))

#y = diff(z)
#plot(head(y,30))

# Phillips-Perron Unit Root Test to check if the data is random walk
PP.test(x) 

#determin MA,AR process
par(mfrow = c(1,2))
acf(x,main='Twitter Customer Sentiment')
pacf(x,main='Twitter Customer Sentiment')


#to determine the best model using auto.arima function in package "forecast"
library(forecast)
ARIMAfit = auto.arima(x, approximation=FALSE,trace=TRUE)

summary(ARIMAfit)

###To fit GARCH model install the required packages
install.packages("quantmod", repos = "https://cran.r-project.org")
install.packages("lattice", repos = "https://cran.r-project.org")
install.packages("timeSeries", repos = "https://cran.r-project.org")
install.packages("rugarch", repos = "https://cran.r-project.org")
install.packages("truncnorm", repos = "https://cran.r-project.org")
install.packages("DistributionUtils", repos = "https://cran.r-project.org")
library(quantmod)
library(lattice)
library(timeSeries)
library(rugarch)

getSymbols("^GSPC", verbose=TRUE)
plot(Cl(GSPC))

spReturns = diff(log(Cl(GSPC)))
spReturns[as.character(head(index(Cl(GSPC)),1))] = 0
plot(spReturns)

##sGARCH with armaOrder(0,0)
spec1=ugarchspec(variance.model=list(model="sGARCH"),
                 mean.model=list(armaOrder=c(0,0)))
fit1=ugarchfit(data=spReturns,spec=spec1)
show(fit1)

##sGARCH with armaOrder(0,1)
spec2=ugarchspec(variance.model=list(model="sGARCH"),
                 mean.model=list(armaOrder=c(0,1)))
fit2=ugarchfit(data=spReturns,spec=spec2)
show(fit2)

##apARCH with armaOrder(0,0)
spec3=ugarchspec(variance.model=list(model="apARCH"),
                 mean.model=list(armaOrder=c(0,0)))
fit3=ugarchfit(data=spReturns,spec=spec3)
show(fit3)

##apARCH with armaOrder(0,1)
spec4=ugarchspec(variance.model=list(model="apARCH"),
                 mean.model=list(armaOrder=c(0,1)))
fit4=ugarchfit(data=spReturns,spec=spec4)
show(fit4)
