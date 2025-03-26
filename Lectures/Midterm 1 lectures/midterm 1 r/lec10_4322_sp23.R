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
bc$Class = ifelse(bc$Class == "malignant", 1, 0)

bc$Class = as.factor(bc$Class)
summary(bc)

#To get the logistic model
#Only one predictor
fit.bc = glm(Class ~ Cell.size, family = "binomial", data = bc)
summary(fit.bc)

#Using 3 predictors
fit.bc3 = glm(Class ~ Cl.thickness + Cell.shape + Cell.size, 
              family = "binomial", data = bc)
summary(fit.bc3)

#predicting
predict.glm(fit.bc3,newdata = data.frame(Cl.thickness = 5, Cell.shape = 5,
                                         Cell.size = 5),type = "response")
#Getting the confusion matrix
summary(fit.bc)
percent.bc = predict.glm(fit.bc, type = "response")
predict.bc = ifelse(percent.bc < 0.5, "benign","malignant")
(conf.bc = table(predict.bc,bc$Class))
sum(conf.bc)
(conf.bc[1,1]+conf.bc[2,2])/sum(conf.bc) #accuracy rate
(conf.bc[2,2]/(conf.bc[2,1]+conf.bc[2,2])) #sensitivity
(conf.bc[1,1]/(conf.bc[1,1]+conf.bc[1,2])) #specificity

#For the three variable model
percent.bc3 = predict.glm(fit.bc3, type = "response")
predict.bc3 = ifelse(percent.bc3 < 0.5, "benign","malignant")
(conf.bc3 = table(bc$Class,predict.bc3))
sum(conf.bc3)
(conf.bc3[1,1]+conf.bc3[2,2])/sum(conf.bc3) #accuracy rate
(conf.bc3[2,2]/(conf.bc3[2,1]+conf.bc3[2,2])) #sensitivity
(conf.bc3[1,1]/(conf.bc3[1,1]+conf.bc3[1,2])) #specificity

#Separate into Test/Training Sets
set.seed(100)
sample = sample.int(n = nrow(bc),
                    size = floor(.75*nrow(bc)),
                    replace = FALSE) 
train.data.bc = bc[sample,]
test.data.bc = bc[-sample,]
train.bc = glm(Class ~ Cl.thickness + Cell.shape + Cell.size,
               data = train.data.bc,
               family = "binomial")
glm.pred = predict.glm(train.bc,newdata = test.data.bc, 
                       type = "response") #getting predictors using the test set
yHat = ifelse(glm.pred < 0.5, "benign","malignant")

(conf.test = table(yHat,test.data.bc$Class))
summary(train.bc)

summary(fit.bc)
1 - 254.76/884.35 #R^2 for one term

summary(fit.bc3)
1-176.5/884.35 #R^2 for three terms

#Using all predictors which is best?  
fit.bc.all = glm(Class ~ ., family = "binomial", data = bc)
summary(fit.bc.all)
step(fit.bc.all)

#Final Result
fit.bc.final = glm(Class ~ Cl.thickness+Cell.shape+Marg.adhesion+Bare.nuclei+Bl.cromatin+Normal.nucleoli+Mitoses, family = "binomial", data = bc)
summary(fit.bc.final)
glm.pred = predict.glm(fit.bc.final,type = "response") 
yHat = ifelse(glm.pred < 0.5, "benign","malignant")
table(yHat,bc$Class)

