#Example 1
#Resample 
temp = c(202.2,203.4,200.5,202.5,206.3,198.0,203.7,200.8,201.3,199.0)
B = 1000 #number of resamples
M = NA #a vector of the means
for(i in 1:B) {
  x = sample(temp,length(temp),replace=T)
  M[i] = mean(x)
}
x #last sample in the for loop
mean(x) #mean of the last sample
mean(M) #mean of the 1000 resampled means
sd(M) #The estimated standard error of the mean

#Histogram
hist(M,main = "",xlab = "Means of the Resampled Temperatures")

#Bootstrap function
library(boot) #Uses the boot library
mean.fun <- function(dat, idx) mean(dat[idx], na.rm = TRUE) 
boot.out = boot(data = temp,statistic = mean.fun,R = 1000)
boot.out
mean(boot.out$t)  #mean of the means
sd(boot.out$t)  #estimated standard error of the means

hist(boot.out$t,main = "",xlab = "")

#Determining the median
median.fun = function(dat, idx) median(dat[idx],na.rm = TRUE)
boot.out.median = boot(data = temp, statistic = median.fun, R = 1000)
boot.out.median
hist(boot.out.median$t,main = "",xlab = "")

#Example 2
library(ISLR)
boot.fn = function(data,index) 
  return(coef(lm(mpg~horsepower,data = data,subset = index)))
boot.fn(Auto,1:392)
#Repeat twice
boot.fn(Auto,sample(392,392,replace = TRUE))
boot.fn(Auto,sample(392,392,replace = TRUE))

#Using the boot function
boot.out = boot(Auto,boot.fn,1000)
boot.out

#Comparing to the linear model
auto.lm = lm(mpg~horsepower,data = Auto)
summary(auto.lm)

#Diagnostic Plots
par(mfrow = c(2,2))
plot(auto.lm)
par(mfrow = c(1,1))

#Creating a Quadratic Model
boot.fn <- function(data , index) 
  coef(lm(mpg ~ horsepower + I(horsepower ^2), 
          data = data , subset = index))
boot(Auto , boot.fn, 1000)
summary(lm(mpg~ horsepower + I(horsepower^2), data = Auto))$coef
