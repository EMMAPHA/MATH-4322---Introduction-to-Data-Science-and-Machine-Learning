library(tree)
#Example of Regression Tree
y = c(15,9,3,25,7,13)
x1 = c(10,5,7,19,11,18)
x2 = c(2,3,3,6,7,9)
example = cbind(y,x1,x2)
(example = as.data.frame(example))
plot(x1,x2)

#No Split
(y.mean = mean(y))
(rss1 = sum((y - y.mean)^2))

#Split 1
(example1 = example[-c(4,6),])
mean(example1$y)
(example2 = example[c(4,6),])
mean(example2$y)
(rss2 = sum((example1$y - mean(example1$y))^2) + 
    sum((example2$y - mean(example2$y))^2))

#Split 2
(example3 = example1[c(2,3),])
mean(example3$y)
(example4 = example1[c(1,4),])
mean(example4$y)
(rss3 = sum((example3$y - mean(example3$y))^2) +
    sum((example4$y - mean(example4$y))^2) +
    sum((example2$y - mean(example2$y))^2))

tree.ex = tree(y~x1 + x2,control = tree.control(6,mincut =2,minsize = 4))
tree.ex
?tree.control
plot(tree.ex)
text(tree.ex)

#Import mower data
mower <- read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/mower.csv")
library(ggplot2)
ggplot(mower,aes(x = Income, y = Size,color = Own,shape = Own)) +
  geom_point() 

#Split 1
ggplot(mower,aes(x = Income, y = Size,color = Own,shape = Own)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 84.75,col = "blue")

#Split 2
ggplot(mower,aes(x = Income, y = Size,color = Own,shape = Own)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 84.75,col = "blue") +
  geom_segment(aes(x = 84.75,xend = min(Income)),
               y = 19.8, yend = 19.8,col = "blue")

#Split 3
ggplot(mower,aes(x = Income, y = Size,color = Own,shape = Own)) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = 84.75,col = "blue") +
  geom_segment(aes(x = 84.75,xend = min(Income)),
               y = 19.8, yend = 19.8,col = "blue") +
  geom_segment(aes(x = 59.7,xend = 59.7),
               y = 19.8, yend = 13,col = "blue")

#fitting the tree
library(tree)
summary(mower)
mower$Own = as.factor(mower$Own)

# Classification Error
tree.mower = tree(Own ~ Income + Size,data = mower)
summary(tree.mower)
plot(tree.mower)
text(tree.mower)
tree.mower

#Gini Index - Node purity
tree.mower.gini = tree(Own ~ Income + Size, data = mower, split = "gini")
summary(tree.mower.gini)
plot(tree.mower.gini)
text(tree.mower.gini)
tree.mower.gini


#Entropy


#Regions with the Gini Method
ggplot(mower,aes(x = Income, y = Size,color = Own,shape = Own)) +
  geom_point() +
  geom_vline(xintercept = 59.7,col = "blue") +
  geom_segment(aes(x = 59.7,xend = max(Income)),
               y = 19.8, yend = 19.8,col = "blue")


set.seed(3)
cv.mower = cv.tree(tree.mower,FUN = prune.misclass)
cv.mower
plot(cv.mower$size,cv.mower$dev,type = "b")
prune.mower = prune.misclass(tree.mower,best = 3)
summary(prune.mower)
plot(prune.mower)
text(prune.mower)

#Example 2: Heart Data
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
(32)/(63+10+30+48)

#rpart package
#install.packages("rpart")
library(rpart)
rtree.mower = rpart(Own ~ Income + Size,data = mower,
                    parms = list(prior = c(.5,.5), split = "information"))
plot(rtree.mower)
text(rtree.mower)
rtree.mower

table(Heart$AHD)

#Linear Regression
lm.ex = lm(y~x1 + x2)
summary(lm.ex)
sum(lm.ex$residuals^2) #rss
plot(lm.ex)
