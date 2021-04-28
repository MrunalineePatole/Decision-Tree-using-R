#####Data Set  CTG of Decision Tree
ctg<-read.csv("E:/datasets/CTG.csv")
head(ctg)
colSums(is.na(ctg))
#Aply Sampling
ctg$NSP<-factor(ctg$NSP)
sample_ctg<-sample(2,nrow(ctg),replace=TRUE,prob=c(0.8,.2))
train_ctg<-ctg[sample_ctg==1,]
test_ctg<-ctg[sample_ctg==2,]

model_ctg<-rpart(NSP~.,data=train_ctg,control = rpart.control(maxdepth = 3,minsplit = 100))
summary(model_ctg)
pred_ctg<-predict(model_ctg,test_ctg,type="class")
pred_ctg

#cofusion Matrix
tab1=table(pred_ctg,test_ctg$NSP)
tab1


  Acc=sum(diag(tab1))*100/sum(tab1)
Acc
library(rpart.plot)
rpart.plot(model_ctg)

