
data <- read.csv("/Users/Memo/Desktop/final4.csv",header=TRUE)

#function on calculate function
cal_ACU<-function(x){
  j=0
  for (i in 1:1636){
    if (x[i]==test$PM2.5[i]){
      j=j+1
    }
  }
  ACU=ACU=j/1636
return(ACU)}


data$PM2.5 = as.factor(data$PM2.5)
# Store every fifth record in a “test” dataset starting with the first record
i<-seq (1,nrow(data),by=5)
test<-data[i,]
# Store the rest in the “training” dataset
train<-data[-i,]
##########################################################################

#C5.0
#install.packages("C50")
library(C50)
#build model
c50_model<-C5.0(train[-2],train$PM2.5)
#check model
summary(c50_model)
pred_c50 <- predict(model,test[-2])
acu_C50 = cal_ACU(pred_c50)


#############################################################################
#knn
# Related to library class in order to use knn() function
library(class)
# Use knn with k=1 and classify the test dataset
predict <- knn(train[,-1][,-1],test[,-1][,-1],training[,2],k=1)
acu_knn1=cal_ACU(predict)

# Repeat the above steps with k=2, k=5, k=10
# k=2
predict <- knn(train[,-1][,-1],test[,-1][,-1],training[,2],k=2)
acu_knn2=cal_ACU(predict)
# k=5
predict <- knn(train[,-1][,-1],test[,-1][,-1],training[,2],k=5)
acu_knn5=cal_ACU(predict)
# k=10
predict <- knn(train[,-1][,-1],test[,-1][,-1],training[,2],k=10)
acu_knn10=cal_ACU(predict)


#################################################################
#naive Bayes
library(e1071)
# Build the model
model <- naiveBayes(PM2.5 ~ ., data = train[, 2:7])
# Predict the test dataset
pred_test = predict(model, test[, 3:7], type = 'class')
#calculate ACU
j=0
for (i in 1:1636){
  if (pred_test[i]==test$PM2.5[i]){
    j=j+1
  }
}
ACU=j/1636
####################################################################
#random forest
library(randomForest)

# Build the model;
rf <- randomForest(PM2.5 ~ ., data=train[, 2:7], importance=TRUE)
importance(rf)
varImpPlot(rf)

# Predict the testing dataset
pred_rf = predict(rf, test[, 3:7])
#calculate ACU
ACU_rf=cal_ACU(pred_rf)