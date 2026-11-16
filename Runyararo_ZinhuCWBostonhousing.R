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



















