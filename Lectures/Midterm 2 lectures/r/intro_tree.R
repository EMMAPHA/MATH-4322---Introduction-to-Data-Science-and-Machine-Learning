#install.packages('tree')
library(tree)
library(ISLR)
?tree
?Hitters
head(Hitters[,c("Salary","Years","Hits")])
summary(Hitters$Salary)
Hitters2 = na.omit(Hitters)
summary(Hitters2)
plot(Hitters2$Hits,Hitters2$Salary)
hist(Hitters2$Salary)
hist(log(Hitters2$Salary))
plot(Hitters2$Hits,log(Hitters2$Salary))

#Split the data into training/testing
set.seed(100)
sample = sample(1:nrow(Hitters2),nrow(Hitters2)/2)
train.hitters = Hitters2[sample,]
test.hitters = Hitters2[-sample,]

#Create a linear regression model
lm.hitters = lm(log(Salary) ~ Hits + HmRun + Runs + RBI + Walks + 
                    Years + PutOuts + Assists + Errors,
                  data = train.hitters)
summary(lm.hitters)

#Determine Significant Variables
step(lm.hitters)
lm.hitters = lm(log(Salary) ~ Hits + Walks + Years, data = train.hitters)
summary(lm.hitters)

#Assumptions met?
par(mfrow = c(2,2))
plot(lm.hitters)
par(mfrow = c(1,1))


#MSE Train
pred.train = predict(lm.hitters)
(mse.hitter.train = mean((log(train.hitters$Salary) - pred.train)^2))
sqrt(exp(.3262))


#MSE Test
pred.test = predict(lm.hitters,newdata = test.hitters)
(mse.hitter.test = mean((log(test.hitters$Salary) - pred.test)^2))
sqrt(exp(0.4697))


#K-fold CV
library(boot)
glm.hitters = glm(log(Salary) ~ Hits + Walks + Years, data = Hitters2)
set.seed(2)
(cv.hitters = cv.glm(Hitters2,glmfit = glm.hitters, K =10)$delta)
sqrt(exp(cv.hitters))

#Regression Tree
tree.hitters = tree(log(Salary) ~ Hits + HmRun + Runs + RBI + Walks + 
                      Years + PutOuts + Assists + Errors,data = train.hitters)
sum.hit = summary(tree.hitters)
sum.hit$used
plot(tree.hitters)
text(tree.hitters,pretty = 0)
pred.tree.train = predict(tree.hitters)

#Residual Mean Deviance
sum.hit
dim(train.hitters)
(tot.resid.dev = sum((log(train.hitters$Salary) - predict(tree.hitters))^2))
tot.resid.dev/(nrow(train.hitters)-10)


#MSE Train
(mse.hitter.tree.train = mean((log(train.hitters$Salary) - pred.tree.train)^2))
sqrt(exp(mse.hitter.train))


#MSE Test
pred.tree.test = predict(tree.hitters,newdata = test.hitters)
(mse.hitter.tree.test = mean((log(test.hitters$Salary) - pred.tree.test)^2))
sqrt(exp(mse.hitter.test))


#Cross Validation
cv.hitters = cv.tree(tree.hitters)
cv.hitters
plot(cv.hitters$size,cv.hitters$dev,type = "b")
prune.hitters = prune.tree(tree.hitters,best = 3)
plot(prune.hitters)
text(prune.hitters)

#Determining MSE
yhat = predict(prune.hitters)
plot(yhat,log(train.hitters$Salary))
abline(0,1)
(mse.prune.train = mean((yhat-log(train.hitters$Salary))^2))
sqrt(exp(mse.prune.train))

yhat.prune = predict(prune.hitters,newdata = test.hitters)
plot(yhat.prune,log(test.hitters$Salary))
abline(0,1)
(mse.prune.test = mean((yhat.prune - log(test.hitters$Salary))^2))
sqrt(exp(mse.prune.test))

#Repeat 10 times
mse.train = NA
mse.test = NA
for (i in 1:10) {
  train = sample(1:nrow(Hitters2),nrow(Hitters2)/2)
  tree.hitters = tree(log(Salary) ~ Hits + HmRun + Runs + RBI + Walks + 
                        Years + PutOuts + Assists + Errors,
                      Hitters2,subset = train)
  prune.hitters = prune.tree(tree.hitters,best = 3)
  mse.train[i] = sqrt(exp(mean((predict(prune.hitters)-
                                  log(Hitters2[train,"Salary"]))^2)))
  mse.test[i] = sqrt(exp(mean((predict(prune.hitters,newdata = Hitters2[-train,])-
                                  log(Hitters2[-train,"Salary"]))^2)))
}
mse.train
mse.test
mean(mse.train)
mean(mse.test)

#For Linear Model

mse.lm.train = NA
mse.lm.test = NA
for (i in 1:10) {
  train = sample(1:nrow(Hitters2),nrow(Hitters2)/2)
  lm.hitters = lm(log(Salary) ~ Hits + Walks + Years,
                      Hitters2,subset = train)
  mse.lm.train[i] = sqrt(exp(mean((predict(lm.hitters)-
                                  log(Hitters2[train,"Salary"]))^2)))
  mse.lm.test[i] = sqrt(exp(mean((predict(lm.hitters,newdata = Hitters2[-train,])-
                                 log(Hitters2[-train,"Salary"]))^2)))
}
mse.lm.train
mse.lm.test
mean(mse.lm.train)
mean(mse.lm.test)

