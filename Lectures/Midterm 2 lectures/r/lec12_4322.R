## Lecture 12 - MATH 4322

# Introduce the libraries you will use
library(ISLR)
library(leaps)

summary(Auto)
dim(Auto)
plot(Auto$horsepower,Auto$mpg)

#Separating into Training/Testing
set.seed(10)
sample1 = sample(1:nrow(Auto),floor(nrow(Auto)*.9))
train1.auto = Auto[sample1,]
test1.auto = Auto[-sample1,]

auto.reg = regsubsets(mpg ~ poly(horsepower,3),data = train1.auto)
auto.res = summary(auto.reg)
auto.stat = cbind(auto.res$adjr2,auto.res$cp,auto.res$bic)
colnames(auto.stat) = c("Adj.R2","Cp","BIC")
rownames(auto.stat) = c("Degree 1","Degree 2","Degree 3")
auto.stat

#Creating the models
auto1.lm = lm(mpg ~ horsepower, data = train1.auto)
auto1.lm2 = lm(mpg ~ poly(horsepower,2),data = train1.auto)
auto1.lm3 = lm(mpg ~ poly(horsepower,3),data = train1.auto)

#MSE on Training data
mse.lm1 = mean(auto1.lm$residuals^2)
mse.lm2 = mean(auto1.lm2$residuals^2)
mse.lm3 = mean(auto1.lm3$residuals^2)


#MSE on Test data
mse.test1 = sum((test1.auto$mpg - predict(auto1.lm,test1.auto))^2)/nrow(test1.auto)
mse.test2 = sum((test1.auto$mpg - predict(auto1.lm2,test1.auto))^2)/nrow(test1.auto)
mse.test3 = sum((test1.auto$mpg - predict(auto1.lm3,test1.auto))^2)/nrow(test1.auto)

mse1 = cbind(c(mse.lm1,mse.lm2,mse.lm3),c(mse.test1,mse.test2,mse.test3))
colnames(mse1) = c("Taining","Test")
rownames(mse1) = c("Degree 1","Degree 2","Degree 3")
mse1

#Validation Set
#Setting the test/training data
set.seed(10)
sample = sample(1:nrow(Auto),nrow(Auto)/2)
train = Auto[sample,]
test = Auto[-sample,]

#Creating the models
auto.lm = lm(mpg ~ horsepower, data = train)
auto.lm2 = lm(mpg ~ poly(horsepower,2),data = train)
auto.lm3 = lm(mpg ~ poly(horsepower,3),data = train)

#Getting the predicted values hat(mpg) from test data
auto.pred = predict(auto.lm,newdata = test,se.fit = TRUE)
auto.pred2 = predict(auto.lm2,newdata = test,se.fit = TRUE)
auto.pred3 = predict(auto.lm3,newdata = test,se.fit = TRUE)

#Getting the MSE for each model
mean((Auto$mpg - predict(auto.lm,Auto))[-sample]^2)
mean((Auto$mpg - predict(auto.lm2,Auto))[-sample]^2)
mean((Auto$mpg - predict(auto.lm3,Auto))[-sample]^2)

#or
mean((test$mpg - auto.pred$fit)^2)
mean((test$mpg - auto.pred2$fit)^2)
mean((test$mpg - auto.pred3$fit)^2)

#Getting this result 10 times
mse = matrix(nrow = 10, ncol = 3)
colnames(mse) = c("Linear","Quadratic","Cubic")
for (i in 1:10) {
  sample = sample(1:nrow(Auto),nrow(Auto)/2)
  train = Auto[sample,]
  test = Auto[-sample,]
  
  #Creating the models
  auto.lm = lm(mpg ~ horsepower, data = train)
  auto.lm2 = lm(mpg ~ poly(horsepower,2),data = train)
  auto.lm3 = lm(mpg ~ poly(horsepower,3),data = train)
  
  #Getting the MSE for each model
  mse[i,1] = mean((Auto$mpg - predict(auto.lm,Auto))[-sample]^2)
  mse[i,2] = mean((Auto$mpg - predict(auto.lm2,Auto))[-sample]^2)
  mse[i,3] = mean((Auto$mpg - predict(auto.lm3,Auto))[-sample]^2)
}
mse

#Leave-One-Out Cross Validation
mse.loocv = matrix(nrow = nrow(Auto), ncol = 3)
for (i in 1:nrow(Auto)) {
  sample = i
  #Creating the models
  auto.lm = lm(mpg ~ horsepower, data = Auto[-sample,])
  auto.lm2 = lm(mpg ~ poly(horsepower,2),data = Auto[-sample,])
  auto.lm3 = lm(mpg ~ poly(horsepower,3),data = Auto[-sample,])
  
  #Getting the MSE for each model
  mse.loocv[i,1] = (Auto$mpg[sample] - predict(auto.lm,Auto[sample,]))^2
  mse.loocv[i,2] = (Auto$mpg[sample] - predict(auto.lm2,Auto[sample,]))^2
  mse.loocv[i,3] = (Auto$mpg[sample] - predict(auto.lm3,Auto[sample,]))^2
}
head(mse.loocv)
colMeans(mse.loocv)

#Function to create the loocv
#install.packages("boot")
library(boot)
auto.glm = glm(mpg ~ horsepower,data = Auto)
cv.glm (Auto, auto.glm)$delta

#Repeat Up to fifth degree
cv.error = rep(0,5)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower,i),data = Auto)
  cv.error[i] = cv.glm(Auto,glm.fit)$delta[1]
}
cv.error
plot(1:5,cv.error,type = "b",xlab = "Degree",ylab = "CV Error")

#K-fold CV 
sample = 1:39
auto.lm2 = lm(mpg ~ poly(horsepower,2),data = Auto[-sample,])
sum(Auto$mpg[sample] - predict(auto.lm2,Auto[sample,]))^2/39

#K-fold Cross Validataion
#use K = 10
cv.error.10 = rep(0,5)
set.seed(3)
for (i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower,i),data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit,K = 10)$delta[1]
}
cv.error.10

#Show with classification
#predicting origin

#Leave One Out Cross Validation
library(MASS)
loocv.err = NA
for (i in 1:nrow(Auto)){
  sample = i
  fit.lda = lda(origin ~ horsepower,data = Auto[-sample,])
  loocv.err[i] = (Auto$origin[sample] == predict(fit.lda,Auto[sample,])$class)
}
mean(loocv.err)
