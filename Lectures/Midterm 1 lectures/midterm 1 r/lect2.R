#Starting with R
2+2
exp(-2)
sqrt(224)

#Assignments
x = 2
x
x+x

#Vectors in R
weight = c(60,72,57,90,95,72)
weight[3]
rnorm(15) #creates a random vector of 15 numbers based on the normal distribution.
mean(weight)
sd(weight)

#Using a data frame that is in R.
?mtcars
?mean
dim(mtcars)
head(mtcars)
plot(wt,mpg)
rm(wt)
plot(mtcars$wt,mtcars$mpg)
attach(mtcars)
detach(mtcars)
plot(mtcars$cyl,mtcars$mpg)
cyl = as.factor(mtcars$cyl)
plot(cyl,mtcars$mpg)
pairs(~mpg+disp+hp+wt,mtcars)
pairs(mtcars)
summary(mtcars$mpg)
summary(mtcars)
summary(cyl)

#Import ontime data
ontime <- read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/ontime.csv")
summary(ontime)
ontime$CARRIER = as.factor(ontime$CARRIER)
table(ontime$CARRIER)
library(ggplot2)
ggplot(ontime, aes(x = DEP_DELAY_NEW, y = DISTANCE,color = CARRIER))+
geom_point()

#Import stock_price
stock_price <- read.csv("E:/Lectures uh/stock_price.csv")
