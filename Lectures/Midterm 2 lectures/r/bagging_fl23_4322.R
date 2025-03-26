#Hitters data set is in the ISLR library
library(ISLR) 
library(tree)

#Import Heart.csv
Heart <- read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/Heart.csv")
head(Heart)
summary(Heart)
set.seed(100)
train = sample(1:nrow(Heart),nrow(Heart)/2+0.5)
Heart$AHD = as.factor(Heart$AHD)
Heart$ChestPain = as.factor(Heart$ChestPain)
Heart$Sex = as.factor(Heart$Sex)
Heart$Thal = as.factor(Heart$Thal)
summary(Heart)
tree.heart = tree(AHD ~ ., Heart,subset =  train)
plot(tree.heart)
text(tree.heart,pretty = 0)
summary(tree.heart)

#Checking Test Error Rate
Heart.test = Heart[-train,]
tree.pred = predict(tree.heart,Heart.test,type = "class")
table(tree.pred,Heart.test$AHD)
(18+13)/(60+18+13+60)

#Pruining
set.seed(3)
cv.heart = cv.tree(tree.heart,FUN = prune.misclass)
par(mfrow = c(1,2))
plot(cv.heart$size,cv.heart$dev,type = "b")
plot(cv.heart$k,cv.heart$dev,type = "b")
dev.off()

prune.heart = prune.misclass(tree.heart,best = 6)
plot(prune.heart)
text(prune.heart,pretty = 0)
levels(Heart$ChestPain)
levels(Heart$Thal)
summary(prune.heart)

#Test Error for Pruned Tree
tree.pred = predict(prune.heart,Heart.test,type = "class")
table(tree.pred,Heart.test$AHD)
(13+18)/(120+13+18)

## Recall Hitters Data
mse = NA
yhat.tree = NA
n = nrow(Hitters)
Hitters2 = na.omit(Hitters)
included.vars = names(Hitters[-c(3,8:15,20)])
par(mfrow = c(1,2))
for (i in 1:2) {
  train = sample(1:n, 0.5*n)	  
  tree.model = tree(log(Salary) ~.,
                    data = Hitters2[,included.vars],
                    subset = train)
  prune.model = prune.tree(tree.model,best = 5)
  plot(prune.model); text(prune.model,pretty = T)
  yhat.tree = predict(prune.model,newdata = Hitters2[-train,included.vars])
  mse[i] = mean((yhat.tree - log(Hitters2[-train,"Salary"]))^2)
}
par(mfrow = c(1,1))
print(mse)
sqrt(exp(mse))
par(mfrow = c(1,1))
head(yhat.tree)

yhat = NA
for (i in 1:2) {
  train = sample(1:n, .5*n)
  lm.model = lm(log(Salary) ~., 
                data = Hitters2[,included.vars],
                subset = train)
  print(lm.model$coeff)
  hitters.test = Hitters2[-train,"Salary"]
  yhat = predict(lm.model,newdata = Hitters2[-train,included.vars])
  mse[i] = mean((yhat[i] - log(hitters.test))^2)
}
print(mse)
sqrt(exp(mse))
head(yhat)

#Example of Bagging
#install.packages('randomForest')
library(randomForest)
set.seed(100)
train = sample(1:nrow(Hitters2),nrow(Hitters2)/2+0.5)
p = 9 #No. of predictors
B = 100 #No. of bootstrap trees
bag.model.p = randomForest(log(Salary) ~.,
                           Hitters2[,included.vars], subset = train,
                           mtry = p,ntree = B, keep.forest = TRUE)
head(bag.model.p$predicted)
bag.model.p #To get an outline of bagging results.
hitters.test = Hitters2[-train,"Salary"]
yhat.bag = predict(bag.model.p,
                   newdata=Hitters2[-train,included.vars])
mean((yhat.bag - log(hitters.test))^2) #Test MSE
plot(bag.model.p) #MSE vs Number of Trees


#Test MSE of Pruned Decision Tree
tree.model = tree(log(Salary) ~. ,
                  data = Hitters2[,included.vars],
                  subset = train)
prune.model = prune.tree(tree.model,best = 4)

yhat = predict(prune.model,newdata = Hitters2[-train,included.vars])
mean((yhat - log(hitters.test))^2) #MSD


#Example of Classification Tree
#Using Heart data
#Fix the data set
Heart = na.omit(Heart); Heart$X = NULL
Heart$AHD = as.factor(Heart$AHD) #needs to be categorical for the tree
Heart$ChestPain = as.factor(Heart$ChestPain) #originally imported as "character"
Heart$Thal = as.factor(Heart$Thal) #originally imported as "character"
summary(Heart) #checking

#Split into training and testing
set.seed(100)
train = sample(1:nrow(Heart),nrow(Heart)/2+0.5)
tree.heart = tree(AHD ~ ., Heart,subset =  train) #Model
Heart.test = Heart[-train,]

#Getting the Test Error Rate
tree.pred = predict(tree.heart,Heart.test,type = "class")
(conf.matrix = table(tree.pred,Heart.test$AHD))
(conf.matrix[1,2]+conf.matrix[2,1])/sum(conf.matrix) #Test error rate



#bagging
n = nrow(Heart); p = ncol(Heart)-1
B = 1000
#In Random Forest Need to put response as factor if a classification problem
Heart$AHD = as.factor(Heart$AHD) 
bag.model = randomForest(AHD ~., data = Heart,
                         ntree = B,
                         mtry = p,
                         importance = TRUE)
bag.model
plot(bag.model)
#Most important Variables
bag.model$importance
varImpPlot(bag.model)

#Most important Variables - Hitters Data
bag.model.p$importance
varImpPlot(bag.model.p)


