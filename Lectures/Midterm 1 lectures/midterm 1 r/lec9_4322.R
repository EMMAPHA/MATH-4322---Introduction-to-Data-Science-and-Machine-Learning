#Cleaning the BreastCancer data
#install.packages("mlbench")
data(BreastCancer,package = "mlbench")
summary(BreastCancer)

#Create a copy with no missing values and remove id column
bc <- BreastCancer[complete.cases(BreastCancer), ] 
bc <- bc[,-1] # remove id column

#Convert the factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}
summary(bc)
#Put malignant = 1 and benign = 0 (not necessary)
bc$Class = ifelse(bc$Class == "malignant", 1, 0) 
bc$Class = as.factor(bc$Class)
summary(bc)

#To get the logistic model
fit.bc = glm(Class ~ Cell.shape, family = "binomial", data = bc)
summary(fit.bc)

#Getting the predictor
predict.glm(fit.bc,newdata = data.frame(Cell.shape = 5))
predict.glm(fit.bc,newdata = data.frame(Cell.shape = 5), 
            type = "response")

#Lab Questions
fit2.bc = glm(Class ~ Cl.thickness,family = "binomial", data = bc)
summary(fit2.bc)
predict.glm(fit2.bc,newdata = data.frame(Cl.thickness = 5),type = "response")

#Setting the frequency table to raw data
Titanic
#install.packages("bbl") #package used to convert to raw data
library(bbl) #call the package
x <- as.data.frame(Titanic) #put as a data frame
head(x)
dim(x)
titanic = freq2raw(data=x[,1:4], freq=x$Freq) #convert to the raw data
head(titanic)
dim(titanic)

#Creating Test/Training data sets
set.seed(101) #set seed
#Select 75% of the data
sample = sample.int(n = nrow(titanic),size = round(.75*nrow(titanic)),
                    replace = FALSE)
head(sample)
train = titanic[sample,]
test = titanic[-sample,]
dim(train)
dim(test)
summary(titanic)

#Create the model
titanic.glm = glm(Survived ~ Sex, family = "binomial",
                  data = train)
summary(titanic.glm)

boxplot(Freq ~ Survived + Sex, data = x)
proportions(xtabs(Freq ~ Survived + Sex,data = x),"Sex")

#Creating a Confusion Matrix
predict.titanic.train = predict(titanic.glm,type = "response")
predict.survive.train = ifelse(predict.titanic.train < 0.5, "No","Yes")
(conf.mat.train = table(predict.survive.train,train$Survived))

(conf.mat.train[1,2]+conf.mat.train[2,1])/sum(conf.mat.train)

#Confusion Matrix on Test Data
predict.titanic = predict(titanic.glm,type = "response", newdata = test)
head(predict.titanic)
predict.survive = ifelse(predict.titanic< 0.5,"No","Yes")
(conf.mat = table(predict.survive,test$Survived))

#Testing Error Rate
(conf.mat[1,2]+conf.mat[2,1])/sum(conf.mat)

#sensitivity
conf.mat[2,2]/(conf.mat[1,2]+conf.mat[2,2])
#Specificity
conf.mat[1,1]/(conf.mat[1,1]+conf.mat[2,1])

