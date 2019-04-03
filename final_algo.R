stp<-BandData3$Steps
cl<-BandData3$Calories_Burned
tsp<-BandData3$Total_slp
wk<-BandData3$Weekend
sleept<-BandData3$Deep_sleep

sleept<-as.factor(BandData3$Deep_sleep)
sleept
#creating data frame for model
df<-data.frame(stp,cl,tsp,wk,sleept)
dim(df)


# Sleep_type = c() 
# for(i in 1:length(BandData3$Deep_sleep))
# {
#   if(BandData3$Deep_sleep[i]<=90)
#   {
#     Sleep_type = c(Sleep_type,"Not enough sleep")
#   }
#   else
#   {
#     Sleep_type = c(Sleep_type," Good sleep")
#   }
# }
#BandData3=cbind(BandData3,Sleep_type)
BandData3
df$class=Sleep_type
head(df)

Sleep_type<-cut(BandData3$Deep_sleep, breaks=2,labels=c("A","B"),right = FALSE)
Sleep_type
df$class=Sleep_type
head(df)

#creating sample for model
sam<-sample(2,nrow(df),replace=TRUE,prob=c(0.7,0.3))

#taining data set
train<-df[sam==1,]

#testing data set 
test<-df[sam==2,]

#loading the library
library(e1071)
library(caret)

#model construction using naive bayes
model<-naiveBayes(class~stp+cl+tsp+wk,data=train)
model

#predictive model for testing data set
pred<-predict(model,test)
pred

cm=table(test$class,pred)

#confusion matrix for train
#confusionMatrix(pred, train$sleept,dnn=list("Predicted","Actual"))

#confusion matrix for test
#confusionMatrix(pred,test$class,dnn=list("Predicted","Actual"))

TP=cm[1,1]
TN=cm[1,2]
FP=cm[2,1]
FN=cm[2,2]
accuracy=(TP+FN)/(TP+TN+FP+FN)
precision=TP/(TP+FP)
Recall= TP/(TP+FN)
print(accuracy*100)
print(precision*100)
print(Recall*100)