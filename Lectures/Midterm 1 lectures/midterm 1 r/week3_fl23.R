##########################
#Lecture 5 R script#
##########################

library(MASS)
# Different Plotting Characters
plot(Boston$lstat,Boston$medv,xlab = "lstat",ylab = "medv")
lm.fit = lm(medv ~ lstat,data = Boston)
predict(lm.fit,newdata = data.frame(lstat = 10))
abline(lm.fit,lwd = 3, col = "red")
plot(Boston$lstat,Boston$medv,pch = 20)

plot(Boston$lstat,Boston$medv,pch = "+",col = "blue")

plot(1:20,1:20,pch = 1:20)
#Input stock_price dataset
stock_price <- read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/stock_price.csv")

#Simple linear regression
stock.lm = lm(Stock_Index_Price ~ Interest_Rate,data = stock_price)
summary(stock.lm)


#Diagnostic Plots
par(mfrow = c(2,2)) #puts four plots in one window with by 2 rows and 2 columns
plot(interest.lm) #diagnostic plots
par(mfrow = c(1,1)) #puts back to one plot in window

#Only Using unemployement rate
unemployment.lm = lm(Stock_Index_Price ~ Unemployment_Rate, data = stock_price)
summary(unemployment.lm)

#Only using Year
year.lm = lm(Stock_Index_Price ~ Year, data = stock_price)
summary(year.lm)

#Multiple linear regression
stock3.lm = lm(Stock_Index_Price ~ Interest_Rate+Unemployment_Rate+Year,
               data = stock_price)
summary(stock3.lm)
#Interpret B_3:  For each additional year the stock index price increases
#on average by $28.89 , given a fixed interest rate and unemployment rate.

#Multicollinarity
cor(stock_price[,-2])

#Anova
anova(stock3.lm)
(TSS_stock = 894463+22394+980+103579)
(RSS_stock = 103579)
#Test Statistic: 
(F_stock = ((TSS_stock - RSS_stock)/3)/(RSS_stock/20))
#P-value
1 - pf(F_stock,3,20)

#Test for Interest Rate in the model, given unemployment rate and year
#are in the model
(t.b1 = 324.59/123.37) #Test Statistic
(2*(1-pt(t.b1,20))) #P-value

#Without Year
stock2.lm = lm(Stock_Index_Price ~ Interest_Rate + Unemployment_Rate,
               data = stock_price)
summary(stock2.lm)
par(mfrow = c(2,2))
plot(stock2.lm)
par(mfrow = c(1,1))

#stepwise Regression
step(stock3.lm) #stepwise "backwards"
step(stock3.lm,direction = "forward")
step(stock3.lm,direction = "both")

# RSE and R^2 for each model
sum.intrest = summary(interest.lm)
sum.stock2 = summary(stock2.lm)
sum.stock3 = summary(stock3.lm)

#R^2
sum.intrest$r.squared
sum.stock2$r.squared
sum.stock3$adj.r.squared

#RSE
sd(sum.intrest$residuals)*sqrt(23/22)
sd(sum.stock2$residuals)*sqrt(23/21)
sd(sum.stock3$residuals)*sqrt(23/20)


anova(stock2.lm)
(104559/5179) + 2*3 -24 #Cp for Interest rate + Unemployment rate
(103579/5179) +2*4 -24 #Cp with all 3 variables
anova(interest.lm)
(126953/5179) + 2*2 -24 #Cp with Interest rate only

2*3 + 24 *log(104559/24) #AIC for Interest rate + unemployment rate
2*2 + 24*log(126953/24) #AIC for Interest rate
2*4 + 24*log(103579/24) #AIC for all 3 variables

BIC(interest.lm) #Interest Rate
BIC(stock2.lm) #Interest Rate + Unemployment Rate
BIC(stock3.lm) #Interest Rate + Unemployment Rate + Year

1 - (104559/(24-2-1))/(1021416/23) #Adjusted R^2 for Interest Rate + Unemployment

library(leaps)
stock.fit = regsubsets(Stock_Index_Price~Unemployment_Rate +
                         Interest_Rate + Year,
                       data = stock_price)
stock.res = summary(stock.fit)
stock.res
stock.res$adjr2
stock.res$cp
stock.res$bic

#Put all in a table
stock.stat = cbind(stock.res$rsq,
                   stock.res$adjr2,
                   stock.res$cp,
                   stock.res$bic)
colnames(stock.stat) = c("rsq","Adjr2","Cp","BIC")
rownames(stock.stat) = c("Interest","Interest+ Unemployement",
                         "Interest+Unemployment+Year")
stock.stat

par(mfrow = c(2,2))
plot(stock2.lm)
par(mfrow = c(1,1))

#Getting a Prediction Interval
predict(stock2.lm,
       newdata = data.frame(Interest_Rate = 2.25,
                            Unemployment_Rate = 6.0),
       interval = "prediction")

#Getting a Confidence Interval
predict(stock2.lm,
        newdata = data.frame(Interest_Rate = 2.25,
                             Unemployment_Rate = 6.0),
        interval = "confidence")
