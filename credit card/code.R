rm(dcredit)
dcredit = read.csv("default of credit card clients.csv" )
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

