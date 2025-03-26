#Using ggplot2 
rollercoaster <- read.delim("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/rollercoaster.txt")
rollercoaster$Type = as.factor(rollercoaster$Type)
library(ggplot2)
ggplot(rollercoaster, aes(x=Height, y=Speed, color=Type)) +
        geom_point() 

#Import index data
index = read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/index.csv")
View(index)
plot(index$age,index$length) #scatterplot, linear?
head(index)

#Linear Regression
fish.lm = lm(length~age,data = index)
abline(fish.lm)
summary(fish.lm)
par(mfrow = c(2,2)) #put the 4 plots in one frame
plot(fish.lm)

#Quadradic Polynomial
fish.lm2 = lm(length~poly(age,2),data = index)
summary(fish.lm2)
plot(fish.lm2)
dev.off() #Removes the plots

#Cubic Polynomial
fish.lm3 = lm(length~poly(age,3),data = index)
summary(fish.lm3)

#Polynomial to the 5th power
fish.lm5 = lm(length~poly(age,5), data = index)
summary(fish.lm5)

#Plotting the data and the models
x = seq(1,6,0.01)
y = predict(fish.lm2,data.frame(age = x)) #Predicted values for quadradic model
y5 = predict(fish.lm5,data.frame(age = x)) #Predicted values for model of 5th degree


plot(index$age,index$length,ylab = "length",xlab = "age") #Scatterplot
abline(fish.lm,col="orange") #Linear model
lines(x,y, col = "blue") #Quadraic model
lines(x,y5,col = "green") #Model to 5th degree
#add legend
legend("topleft",legend = c("Linear","Degree 2","Degree 5"), 
       col = c("orange","blue","green"),lty = 1)

#Lab questions
library(MASS)
fit.lm = lm(medv ~ lstat, data = Boston)
summary(fit.lm)
plot(Boston$lstat,Boston$medv)
abline(fit.lm,col = "red")
par(mfrow = c(2,2))
plot(fit.lm)

fit.lm2 = lm(medv ~ poly(lstat,2),data = Boston)
summary(fit.lm2)
plot(fit.lm2)

fit.lm3 = lm(medv ~ poly(lstat,3),data = Boston)
summary(fit.lm3)
plot(fit.lm3)
fit.lm5 = lm(medv ~ poly(lstat,5),data = Boston)
summary(fit.lm5)
plot(fit.lm5)
par(mfrow = c(1,1))


#Example for Linear Regression
Era <- read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 3339 Lectures/Notes for Summer 2020/Era.csv")
plot(Era$ERA,Era$Wins,xlab = "ERA", ylab = "Wins",
     main = "Pitcher's Wins With ERA")
era.lm = lm(ERA ~ Wins,data = Era)
summary(era.lm)


#Detecting OUtliers Example
#Using Era data
plot(Era$ERA,Era$Wins,xlab = "ERA", ylab = "Wins",
     main = "Pitcher's Wins With ERA")
era.lm = lm(ERA ~ Wins, data = Era)
par(mfrow = c(2,2))
plot(era.lm)
Era[11,]

#Removing the 11th value
era2 = Era[-11,]
era.lm2 = lm(ERA ~ Wins,data = era2)
summary(era.lm2)
plot(era.lm2)
par(mfrow = c(1,1))
plot(era2$Wins,era2$ERA,xlab = "ERA", ylab = "Wins",
     main = "Pitcher's Wins With ERA")
abline(era.lm2)

#Detecting High Leverage Observations
#Using the Rollercoaster Data
plot(rollercoaster$Height,rollercoaster$Speed,xlab = "Height",
     ylab = "Speed")
par(mfrow = c(2,2))
roller.lm = lm(Speed ~ Height, data = rollercoaster)
summary(roller.lm)
plot(roller.lm)
rollercoaster[71,]

#Remove leverage points
roller2.lm = lm(Speed ~ Height, data = rollercoaster[-71,])
summary(roller2.lm)
plot(roller2.lm)

#Detecting Multicollinarity
#Using the Stock Index Price Data
library(car)
stock3.lm = lm(Stock_Index_Price ~ Interest_Rate+Unemployment_Rate+Year,
               data = stock_price)
vif(stock3.lm)

summary(lm(Interest_Rate ~ Unemployment_Rate + Year, data = stock_price))$r.squared


