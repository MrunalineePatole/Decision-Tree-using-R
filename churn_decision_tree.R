ch<-read.csv("E:/datasets/churn.csv")
head(ch)
colSums(is.na(ch))
mean(ch$TotalCharges,na.rm = TRUE)
ch$TotalCharges[is.na(ch$TotalCharges)]<-2283.3
head(ch)
View(ch)
ch<-data.frame(ch[,-1])
####ctree or rpart , plot or 
ch$gender<-as.numeric(as.factor(ch$gender))
ch$Partner<-as.numeric(as.factor(ch$Partner))
ch$Dependents<-as.numeric(as.factor(ch$Dependents))
ch$PhoneService<-as.numeric(as.factor(ch$PhoneService))
ch$MultipleLines<-as.numeric(as.factor(ch$MultipleLines))
ch$InternetService<-as.numeric(as.factor(ch$InternetService))
ch$OnlineBackup<-as.numeric(as.factor(ch$OnlineBackup))
ch$OnlineSecurity<-as.numeric(as.factor(ch$OnlineSecurity))
ch$DeviceProtection<-as.numeric(as.factor(ch$DeviceProtection))
ch$TechSupport<-as.numeric(as.factor(ch$TechSupport))
ch$StreamingTV<-as.numeric(as.factor(ch$StreamingTV))
ch$StreamingMovies<-as.numeric(as.factor(ch$StreamingMovies))
ch$Contract<-as.numeric(as.factor(ch$Contract))
ch$PaperlessBilling<-as.numeric(as.factor(ch$PaperlessBilling))
ch$PaymentMethod<-as.numeric(as.factor(ch$PaymentMethod))

library(dplyr)

ch<-mutate(ch,Churn =  ifelse(Churn=="Yes",1,0))
ch$Churn<-factor(ch$Churn)

#Apply sampling
sample_ch<-sample(2,nrow(ch),replace = TRUE,prob = c(0.8,.2))
train_ch<-ch[sample_ch==1,]
test_ch<-ch[sample_ch==2,]


#Apply Prediction
library(rpart)
model_ch<-rpart(Churn~.,data=train_ch)
summary(model_ch)
##Prediction
pred_ch<-predict(model_ch,test_ch , type = "class" )
pred_ch

