#Packages Used
#install.packages("nnet")
#install.packages("NeuralNetTools")
#install.packages("neuralnet")
library(neuralnet)
library(NeuralNetTools)
library(nnet)

#Example 1 using nnet library
RestaurantTips <- read.csv("C:/Users/cpoliak/OneDrive - University Of Houston/MATH 4322/RestaurantTips.csv")
attach(RestaurantTips)
names(RestaurantTips)
summary(RestaurantTips)
RestaurantTips$CustomerWillTip = as.factor(CustomerWillTip)

#Train the model based on output from input
model = nnet(CustomerWillTip ~ Service + Ambience + Food, 
             data = RestaurantTips, 
             size = 5, 
             rang = 0.1, 
             decay = 5e-2, 
             maxit = 5000)
print(model)

pred_nnet<-predict(model,RestaurantTips,type = "class") 
(mtab<-table(RestaurantTips$CustomerWillTip,pred_nnet))
plotnet(model)
garson(model)

#Example 2 Using neuralnet library
input = c(0,1,2,3,4,5,6,7,8,9,10)
output = input^2
mydata = data.frame(cbind(input,output))
names(mydata) = c("input","output")
mydata

set.seed(1)
model = neuralnet(output ~ input,  data = mydata, hidden = 10, 
                  threshold = 0.01)
model
plot(model)


yhat = as.data.frame(model$net.result)
final_output = cbind(input, output,yhat)
colnames(final_output) = c("Input","Expected output", "Neural Net Output")
print(final_output)

sum((output - as.data.frame(model$net.result))^2/2)
model$result.matrix
