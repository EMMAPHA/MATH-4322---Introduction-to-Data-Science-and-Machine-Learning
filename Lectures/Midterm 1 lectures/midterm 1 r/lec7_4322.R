#########################
# Lecture 7 - MATH 4322 #
#########################

#Use mtcars
summary(mtcars)
head(mtcars)
?mtcars

#Type of transmission as a predictor
summary(lm(mpg~am,data = mtcars))
boxplot(mpg~am,data = mtcars)
am = as.factor(mtcars$am)
plot(am,mtcars$mpg)
summary(lm(mtcars$mpg~am))

#Number of cylinders as predictor
summary(lm(mpg~cyl,data = mtcars))

#Need to define it as categorical
cyl.fact = as.factor(mtcars$cyl) 

summary(lm(mtcars$mpg~cyl.fact))
tapply(mtcars$mpg,cyl.fact,mean)
boxplot(mtcars$mpg~cyl.fact)

#Credit data
#install.packages("ISLR")
library(ISLR)
head(Credit)
summary(Credit)
levels(Credit$Ethnicity)
summary(lm(Balance~Ethnicity,data = Credit))
anova(lm(Balance~Ethnicity,data = Credit))
boxplot(Balance~Ethnicity,data = Credit)

#Stock Price data
#Model with Interaction term
stock.int = lm(Stock_Index_Price~Interest_Rate*Unemployment_Rate)
summary(stock.int)
step(stock.int)


#Rollercoaster Data
#Import the Data See This Lecture Folder
#rollercoaster <- read.delim("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/rollercoaster.txt")
View(rollercoaster)
head(rollercoaster)
rollercoaster$Type=as.factor(rollercoaster$Type)
roller.lm = lm(Speed~Height+Type,data = rollercoaster)
summary(roller.lm)

#Plotting the Two Lines No interaction
plot(rollercoaster$Height,roller.lm$coefficients[1]+
       roller.lm$coefficients[2]*rollercoaster$Height,type = "l",
     col = "red",ylab = "Speed",xlab = "Height",main = "No Interaction Term")
lines(rollercoaster$Height,roller.lm$coefficients[1] + 
        roller.lm$coefficients[2]*rollercoaster$Height + 
        roller.lm$coefficients[3],col = "blue")
legend("topleft",legend = c("Wood","Steel"),col = c("red","blue"), lty = 1)

#Checking Assumptions
par(mfrow=c(2,2))
plot(roller.lm)
par(mfrow = c(1,1))

#Using the Interaction Term
roller.int = lm(Speed~Height*Type,data = rollercoaster)
summary(roller.int)

#Plotting the two lines with interaction
plot(rollercoaster$Height,roller.int$coefficients[1]+
       roller.int$coefficients[2]*rollercoaster$Height,type = "l",
     col = "red",ylab = "Speed",xlab = "Height",main = "With Interaction Term")
lines(rollercoaster$Height,roller.int$coefficients[1] + 
        (roller.int$coefficients[2] + roller.int$coefficients[4])*
        rollercoaster$Height + 
        roller.int$coefficients[3],col = "blue")
legend("topleft",legend = c("Wood","Steel"),col = c("red","blue"), lty = 1)

