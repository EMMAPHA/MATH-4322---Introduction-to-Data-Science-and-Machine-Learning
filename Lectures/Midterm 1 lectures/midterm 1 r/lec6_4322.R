##########################
#Lecture 6 R script#
##########################

#Input stock_price dataset
stock_price <- read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/stock_price.csv")

#Linear regression
stock.lm = lm(Stock_Index_Price ~ Interest_Rate,data = stock_price)
summary(stock.lm)

stock2.lm = lm(Stock_Index_Price ~ Interest_Rate+Unemployment_Rate,
               data = stock_price)
summary(stock2.lm)

stock3.lm = lm(Stock_Index_Price~Interest_Rate+Unemployment_Rate+
                 Year, data = stock_price)
summary(stock3.lm)

#Analysis of Variance
(stock3.aov = anova(stock3.lm))
(stock2.aov = anova(stock2.lm))
(stock.aov = anova(stock.lm))

#R^2 and RSE
#Interest_Rate + Unemployment_Rate + Year
sd(resid(stock3.lm))*sqrt(23/20) #RSE
summary(stock3.lm)$r.squared #R^2
#Interest_Rate + Unemployment_Rate
sd(resid(stock2.lm))*sqrt(23/20) #RSE
summary(stock2.lm)$r.squared #R^2
#Interest_Rate Only
sd(resid(stock.lm))*sqrt(23/20) #RSE
summary(stock.lm)$r.squared #R^2

#Adjusted R^2 
#Interest_Rate + Unemployment_Rate + Year
summary(stock3.lm)$adj.r.squared
#Interest_Rate + Unemployment_Rate
summary(stock2.lm)$adj.r.squared
#Interest_Rate Only
summary(stock.lm)$adj.r.squared


##Cp
stock3.aov$`Mean Sq`[4] #MSE_all

stock3.aov$`Sum Sq`[4]  #RSS_3
(103579/5179) +2*4 -24 #Cp with all 3 variables
stock.aov$`Sum Sq`[2]  #RSS_1
(126953/5179) + 2*2 -24 #Cp with Interest rate only
stock2.aov$`Sum Sq`[3]  #RSS_2
(104559/5179) + 2*3 -24 #Cp for Interest rate + Unemployment rate

#AIC
2*3 + 24 *log(104559/24) #AIC for Interest rate + unemployment rate
2*2 + 24*log(126953/24) #AIC for Interest rate
2*4 + 24*log(103579/24) #AIC for all 3 variables

#BIC
BIC(interest.lm) #Interest Rate
BIC(stock2.lm) #Interest Rate + Unemployment Rate
BIC(stock3.lm) #Interest Rate + Unemployment Rate + Year


#install.packages("leaps")
library(leaps)
stock.fit = regsubsets(Stock_Index_Price~Unemployment_Rate +
                         Interest_Rate + Year,
                       data = stock_price)
(stock.res = summary(stock.fit))
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

#Getting a Confidence Interval
predict(stock2.lm,
        newdata = data.frame(Interest_Rate = 2.25,
                             Unemployment_Rate = 6.0),
        interval = "confidence")

#Getting a Prediction Interval
predict(stock2.lm,
       newdata = data.frame(Interest_Rate = 2.25,
                            Unemployment_Rate = 6.0),
       interval = "prediction")



