rm(list=ls(all=TRUE));
#dcredit = read.csv("test.csv")
dcredit = read.csv("default of credit card clients.csv");
#check sample count
dim(dcredit)
#head(dcredit)
#tail(dcredit)

dim(dcredit)
str(dcredit)
#names(dcredit)
#summary(dcredit)

#explore
quantile(dcredit$LIMIT_BAL)
quantile(dcredit$LIMIT_BAL, c(.1, .3, .65))
var(dcredit$LIMIT_BAL)
plot(density(dcredit$LIMIT_BAL))
pie(table(dcredit$LIMIT_BAL))
library(MASS)
#parcoord(dcredit, col=dcredit$DEFAULT_NEXT)

#processRaw <- function( dcredit )
{
	dcredit$ID <- NULL;
	dcredit$VAL <- NULL;
	dcredit$DEFAULT_NEXT <- dcredit$default.payment.next.month
	dcredit$default.payment.next.month <- NULL; 

	for( it in c(1: nrow(dcredit))){
		dcredit$DEFAULT_NEXT[it] <- if (dcredit$DEFAULT_NEXT[it]==1)"yes" else "no";
	}
	dcredit$DEFAULT_NEXT <- factor(dcredit$DEFAULT_NEXT);
	dcredit$SEX <- factor(dcredit$SEX);
	dcredit$EDUCATION<- factor(dcredit$EDUCATION);
	dcredit$MARRIAGE<- factor(dcredit$MARRIAGE);
}

str(dcredit);
#processRaw(dcredit);

#prepare data
k <- c(1:30000);
trainIndex <- sample(k,20000);

set.seed(1234)
ind <- sample(2, nrow(dcredit), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dcredit[ind==1,]
testData <- dcredit[ind==2,]

#mode
myFormula <- DEFAULT_NEXT~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6

#tree
library(party)
fun_tree <- function(){
	print("decision tree");
	treeFormula <- DEFAULT_NEXT~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6

	credit_tree <- ctree(treeFormula, data=trainData)
	table(trainData$DEFAULT_NEXT, predict(credit_tree))

	treePred <- predict(credit_tree, newdata= testData)
	table(testData$DEFAULT_NEXT, treePred)
}

#nnet
library(nnet)
fun_nnet <- function(){
	print("nnet");
	nnetFormula <- DEFAULT_NEXT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6

	credit_nnt<- nnet(nnetFormula, data = trainData,  size=5, rang =0.1, decay=5e-4,maxit=500);
	table(trainData$DEFAULT_NEXT, predict(credit_nnt));

	nntPred <- predict(credit_nnt, testData);
	table(testData$DEFAULT_NEXT, nntPred);
}

#svm
library(e1071)

fun_svm <- function(){
	print("svm");
	svmFormula <- DEFAULT_NEXT~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6

	credit_svm <- svm(svmFormula, data=trainData);
	table(trainData$DEFAULT_NEXT, predict(credit_svm));

	svmPre <- predict(credit_svm,testData);
	table(testData$DEFAULT_NEXT, svmPre);
}

#cluster
fun_cluster <- function(){
	print("cluster");
	credit_dup = dcredit;
	credit_dup$DEFAULT_NEXT <- NULL;

	kmeans.result <- kmeans( credit_dup, 2 );
	table(dcredit$DEFAULT_NEXT, kmeans.result$cluster);
}


