###Logistic Regression  on Credit Risk###
##Problem Statement:
#1.Build a predictive model which can predict wether to approve or reject loan
#2.Run a marketing /sale campaign to identify(target) the good customer
cr<-read.csv("E:/datasets/CreditRisk.csv",na.strings = "")
cr<-cr[,c(2:13)]
View(cr)
head(cr)
colSums(is.na(cr))

table(cr$Gender)
cr$Gender[is.na(cr$Gender)]<-"Male"
colSums(is.na(cr))

table(cr$Married)
cr$Married[is.na(cr$Married)]<-"Yes"
colSums(is.na(cr))        

table(cr$Dependents)
cr$Dependents[is.na(cr$Dependents)]<- 0
colSums(is.na(cr))

table(cr$Self_Employed)
cr$Self_Employed[is.na(cr$Self_Employed)]<-"Yes"
colSums(is.na(cr))

mean(cr$LoanAmount,na.rm = TRUE)
cr$LoanAmount[is.na(cr$LoanAmount)]<-142.5115
colSums(is.na(cr))

mean(cr$Loan_Amount_Term,na.rm = TRUE)
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)]<-342.2019
colSums(is.na(cr))

table(cr$Credit_History)
cr$Credit_History[is.na(cr$Credit_History)]<-0
colSums(is.na(cr))

####Converting categorial data into numeric
cr$Gender <-as.numeric(as.factor(cr$Gender))
cr$Married <-as.numeric(as.factor(cr$Married))
cr$Education <-as.numeric(as.factor(cr$Education))
cr$Self_Employed<-as.numeric(as.factor(cr$Self_Employed))
cr$Property_Area <-as.numeric(as.factor(cr$Property_Area))

library(dplyr)
# Converting loan status into 0 & 1
cr<-mutate(cr,Loan_Status = ifelse(Loan_Status=="Y",1,0))
colSums(is.na(cr))
View(cr)

cr$Loan_Status<-factor(cr$Loan_Status)

#Apply Sampling
sample_cr<- sample(2,nrow(cr),replace = TRUE,prob = c(0.8,0.2))
cr_train<-cr[sample_cr==1,]
cr_test<-cr[sample_cr==2,]
dim(cr_train)
dim(cr_test)
dim(cr)
library(rpart)
###Apply Decision Tree Model
model_cr<-rpart(Loan_Status~.,data=cr_train)
summary(model_cr)
pred_cr<-predict(model_cr,cr_test,type = "class")
pred_cr

####Apply Confusion matrix
### To build the confusion matrix prediction has to be  i
tab1=table(pred_cr,cr_test$Loan_Status)
tab1

Acc=sum(diag(tab1))*100/sum(tab1)
Acc
library(rpart.plot)
rpart.plot(model_cr)

tab1[0][1]
class(tab1)
dim(tab1)
tpr1<-tab1[2,2]/(tab1[2,2]+tab1[1, 2])
tpr1
fpr1<-tab1[2,1]/(tab1[2,1]+tab1[1,1])
fpr1

