library(openxlsx)

bostonhousing = read.xlsx("Boston_Housing.xlsx" ,1)
nrow(bostonhousing)
ncol(bostonhousing)

#data preparation
#RECODING MEDV VARIABLE AS _ IF PRICE>= 30
highprice<-ifelse(bostonhousing$MEDV >= 30,1,0)
highprice

table(highprice)

bostonhousing<-data.frame(bostonhousing,highprice)
str(bostonhousing)

bostonhousing[13]
#removing the unused variable from data frame
bostonhousing<-bostonhousing[-13]
str(bostonhousing)

#converting variables to categorical
bostonhousing$CHAS<-as.factor(bostonhousing$CHAS)
bostonhousing$highprice<-as.factor(bostonhousing$highprice)
str(bostonhousing)

#partitioning into training and testing
install.packages("caTools")
library(caTools)
set.seed(1)
ind<-sample.split(Y=bostonhousing$highprice,SplitRatio=0.8)
ind


trainbostonhousing<-subset(bostonhousing,ind==TRUE)
nrow(trainbostonhousing)
str(trainbostonhousing)

testbostonhousing<-subset(bostonhousing,ind==FALSE)
nrow(testbostonhousing)

#to run classification tree 
install.packages("tree")
library(tree)
modelbostonhousing<-tree(highprice~.,trainbostonhousing)
modelbostonhousing

plot(modelbostonhousing)
text(modelbostonhousing,pretty=2)

#predicting with training data
predicttrain<-predict(modelbostonhousing,
                      trainbostonhousing,type='class')
head(predicttrain) #predicted outcomes
head(trainbostonhousing$highprice) #actual outcomes

#creating confusion matrix
confusionmatrixtrain<-table(Predicted=predicttrain,
                            Actual=trainbostonhousing$highprice)

confusionmatrixtrain

#error percentage = number of errors / nrow(trainbostonhousing)*100

(4+5)/nrow(trainbostonhousing)*100

#error rate - proportion of incorrect predictions
x<-(4+5)/nrow(trainbostonhousing)

#accuracy =1-error rate #proportion of correct predictions (TP + TN)
accuracy=1-x
accuracy

#SENSITIVITY(recall)=TP/(TP+FN) HIGH RECALL = FEWER FALSE NEGATIVES
62/(62+5)

#PRECISION = tp/(tp+fp) HIGH PRECISION = FEWER FALSE POSITIVES
62/(62+4)

#F1 SCORE 2*(precision*recall)/(precision+recall) F1 =0 when either
#precision or recall is 0
2*((0.9393939*0.9253731)/(0.9393939+0.9253731))

#predicting with testing data
predicttest <- predict(modelbostonhousing,testbostonhousing,type='class')
head(predicttest)
head(testbostonhousing$highprice) #actual outcomes for  testing data

#creating confusion matrix
confusionmatrixtest<-table(Predicted=predicttest,
                           Actual=testbostonhousing$highprice)

confusionmatrixtest

#NUMBER OF ERRORS
(3+3)/nrow(testbostonhousing)*100 #error percentage

#error rate - proportion of incorrect predictions

#scoring new records
recordtoscore = read.xlsx("Boston_Housing.xlsx",2)
recordtoscore$CHAS<-as.factor(recordtoscore$CHAS)

predictedvalue<-predict(modelbostonhousing,
                        recordtoscore,type='class')


predictedvalue

#predict with a different cutoff value

probs<-predict(modelbostonhousing,
               recordtoscore,type='vector')

class(probs)
probs

cutoff <- 0.5
predicted_class <- ifelse(probs[, "1"] > cutoff,
                          "1","0")

confusionmatrixtrain<-table(predicted=predicted_class,
                            Actual=trainbostonhousing$highprice)

confusionmatrixtrain
(11+0)/nrow(trainbostonhousing)*100

#Regression tree

library(openxlsx)
boston = read.xlsx("Boston_Housing.xlsx",1)
str(boston)

#checking data
range(boston$MEDV)

#converting to factor
boston$CHAS=as.factor(boston$CHAS)
str(boston)

#partition the dataset
library(caTools)
set.seed(1234)
ind<-sample.split(boston$MEDV,SplitRatio=0.8)
train_bh<-subset(boston,ind==TRUE)
test_bh<-subset(boston,ind==FALSE)

install.packages('rsample')
library(rsample)
set.seed(123)
split <- initial_split(boston, prop = 0.8, strata = "MEDV")
train_bh <- training(split)
nrow(train_bh)
test_bh <- testing(split)
nrow(test_bh)

#to run regression tree
library(tree)
model_bh<-tree(MEDV~.,train_bh)
model_bh

plot(model_bh)
text(model_bh,pretty=0)

#predicting with train data
predicttrain<-predict(model_bh,train_bh)
head(predicttrain)
train_bh$predicted<-predicttrain

#calculating model fit measures for training data
#training data
#model fit indices Training data
mse <- mean((predicttrain - train_bh$MEDV)^2)
mse
rmse <- sqrt(mean((predicttrain - train_bh$MEDV)^2)) 
rmse
mad<-sum(abs(predicttrain - train_bh$MEDV)) / nrow(train_bh)
mad

#R square -> 1-(RSSE/TSSE) ; 
#1-(residual sum of square error/total sum of square error)
x=sum((predicttrain-train_bh$MEDV)^2) #Residual SSE
y=sum((train_bh$MEDV-mean(train_bh$MEDV))^2) #Total SSE
Rsquare= 1-(x/y)
Rsquare

# Adjusted R square
#1- ((1-Rsquare)(n-1)/(n-p-1)) where n is number of observations and
#p is number of predictors
AdjRsquare= 1-((1-Rsquare)*(nrow(train_bh)-1)/
                 (nrow(train_bh)-(ncol(train_bh)-1)-1))
AdjRsquare

#predicting with testing data
predicttest<-predict(model_bh,test_bh)
head(predicttest)
head(test_bh$MEDV)

#appending the predicted values to the data frame
test_bh$predicted<-predicttest


#calculating model fit measures for testing data
#Testing data
mse <- mean((predicttest - test_bh$MEDV)^2)
mse
rmse <- sqrt(mean((predicttest - test_bh$MEDV)^2)) 
rmse
mad<-sum(abs(predicttest-test_bh$MEDV))/nrow(test_bh)
mad

#mse(preds=predicttest,actuals=test_bh$MEDV)
#rmse(preds=predicttest,actuals=test_bh$MEDV)

xtest=sum((predicttest-test_bh$MEDV)^2) 
ytest=sum((test_bh$MEDV-mean(test_bh$MEDV))^2)
Rsquaretest=1-(xtest/ytest)
Rsquaretest

AdjRsquaretest= 1-( (1-Rsquaretest)*(nrow(test_bh)-1)/
                      (nrow(test_bh)-(ncol(test_bh)-1)-1))
AdjRsquaretest

new = read.xlsx("Boston_Housing.xlsx",2)
new$CHAS<-as.factor(new$CHAS)
predictedoutcome<-predict(model_bh,new)
predictedoutcome

















