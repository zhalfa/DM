rm(list=ls(all=TRUE));
dcredit = read.csv("test.csv" )
str(dcredit)
dim(dcredit)
names(dcredit)
head(dcredit)
tail(dcredit)
dcredit[100:105,]
dcredit[100:105,"ID"]
summary(dcredit)
quantile(dcredit$LIMIT_BAL)
quantile(dcredit$LIMIT_BAL, c(.1, .3, .65))
var(dcredit$LIMIT_BAL)
plot(density(dcredit$LIMIT_BAL))
pie(table(dcredit$LIMIT_BAL))
library(MASS)
parcoord(dcredit[2:25], col=dcredit$default.payment.next.month)

#prepare data
k <- c(1:30000);
trainIndex <- sample(k,20000);

set.seed(1234)
ind <- sample(2, nrow(dcredit), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dcredit[ind==1,]
testData <- dcredit[ind==2,]

#mode

#tree
library(party)

myFormula <- VAL~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6

credit_tree <- ctree(myFormula, data=trainData)
table(predict(credit_tree), trainData$VAL)

treePred <- predict(credit_tree, newdata= testData)
table(treePred,testData$VAL)

#nnet
library(nnet)

nnetFormula <- default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6

credit_nnt<- nnet(myFormula, data = trainData,  size=2, rang =0.1, decay=5e-4,maxit=200);
table(predict(credit_nnt),trainData$VAL);

nntPred <- predict(credit_nnt, testData);
table(nntPred,testData$VAL);

#svm
library(e1071)

svmFormula <- nnetFormula

#svmMode <-svm(svmFormula, data=dcredit)
credit_svm <-svm(myFormula, data=trainData);
table(predict(credit_svm), trainData$VAL);

svmPre <- predict(credit_svm,testData);
table(svmPre, testData$VAL);

