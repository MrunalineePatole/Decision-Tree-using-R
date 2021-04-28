install.packages("randomForest")
(6/10)*log2(6/10)-(4/10)*log2(4/10)

-(0.5)*log2(0.5)-(0.5)*log2(0.5)


-(30/560)*log2(30/560)-(530/560)*log2(530/560)
-(370/440)*log2(370/440)-(70/440)*log2(70/440)

#------------------------decision tree------------------------------------------------------

#problem statement 1. build a predictive moedel which can predict whether to approve or reject the loan

cr <- read.csv("CreditRisk.csv", na.strings = "")

head(cr)
dim(cr)

cr <- cr[, -1]
head(cr)
dim(cr)

# ----------------------handling NA------------------
colSums(is.na(cr))

table(cr$Gender)
cr$Gender[is.na(cr$Gender)] <- 'Male'

colSums(is.na(cr))
table(cr$Married)
cr$Married[is.na(cr$Married)] <- 'Yes'

colSums(is.na(cr))
table(cr$Dependents)
cr$Dependents[is.na(cr$Dependents)] <- 0

colSums(is.na(cr))
table(cr$Self_Employed)
cr$Self_Employed[is.na(cr$Self_Employed)] <- 'Yes'

colSums(is.na(cr))
table(cr$LoanAmount)
median(cr$LoanAmount, na.rm = TRUE)
mean(cr$LoanAmount, na.rm = TRUE)
cr$LoanAmount[is.na(cr$LoanAmount)] <- 126

colSums(is.na(cr))
table(cr$Loan_Amount_Term)
mean(cr$Loan_Amount_Term,na.rm = TRUE)
median(cr$Loan_Amount_Term,na.rm = TRUE)
cr$Loan_Amount_Term[is.na(cr$Loan_Amount_Term)] <- 360

colSums(is.na(cr))
table(cr$Credit_History)
cr$Credit_History[is.na(cr$Credit_History)] <- 0

colSums(is.na(cr))
#------------------convert categorical to numeric--------------

library(dplyr)

cr <- mutate(cr, Loan_Status=ifelse(Loan_Status=="Y",1,0))

cr$Gender <- as.numeric(as.factor(cr$Gender))
cr$Married <- as.numeric(as.factor(cr$Married))
cr$Education <- as.numeric(as.factor(cr$Education))
cr$Self_Employed <- as.numeric(as.factor(cr$Self_Employed))
cr$Property_Area <- as.numeric(as.factor(cr$Property_Area))
head(cr)
View(cr)

colSums(is.na(cr))

is.numeric(cr)

table(cr$Loan_Status)

cr$Loan_Status <- factor(cr$Loan_Status)

#-----------------------Sampling--------------------

cr_sample <- sample(2, nrow(cr), replace = TRUE, prob = c(0.8,0.2))
cr_train <- cr[cr_sample== 1,]
cr_test <- cr[cr_sample == 2, ]
dim(cr_train)
dim(cr_test)

#---------------------------model building-----------------------
library(rpart)
cr_tree <- rpart(Loan_Status~., data = cr_train)
summary(cr_tree)

#----------------------------------------------------------------

pred_cr <- predict(cr_tree, cr_test, type = "class")
pred_cr

#----------------canfusion matrix--------------------------------

cm <- table( pred_cr,cr_test$Loan_Status)
cm

acc <- sum(diag(cm)) / sum(cm)
acc

#----------plot treee----------------------------
library(rpart.plot)
rpart.plot(cr_tree)










#---------------------new dataset-------------------------------

ctg <- read.csv("datasets/CTG.csv")

dim(ctg)
colSums(is.na(ctg))

ctg$NSP <- factor(ctg$NSP)
#--------------------sampling-----------------------
samplectg <- sample(2,nrow(ctg), replace = TRUE, prob = c(0.8,0.2))
ctgtrain <- ctg[samplectg==1,]
ctgtest <- ctg[samplectg==2,]
dim(ctgtrain)

#------------------------model build--------------------------------

ctgmodel <- rpart(NSP~., data = ctgtrain, control=rpart.control(maxdepth = 5, minisplit=50))
summary(ctgmodel)
#--------------------------prediction----------------------------------------

prediction <- predict(ctgmodel,ctgtest, type = "class")
prediction

#-------------------confusion matrix----------------------

cm1 <- table(prediction, ctgtest$NSP)
cm1


#---------------plot---------------------
rpart.plot(ctgmodel)
