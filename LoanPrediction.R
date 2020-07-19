#Required packages for decision tree and ROC curve
library(ISLR)
library(tree)
library(ROCR)
library(pROC)
library(caTools)

#Importing dataset and structure of dataset
BANK<- read.csv("~/Desktop/Universalbank.csv",header = TRUE,sep = ";")
names(BANK)
dim(BANK)
head(BANK)
str(BANK)

#check whether data includes "NA" values
which(is.na(BANK))

#initialization 
result <- data.frame()

#Changing output attribute to categorical variable
BANK$Personal.Loan=as.factor(BANK$Personal.Loan)

#Split data into testing and training
for (j in 1:50) {
set.seed(j) 
train=sample.split(BANK,SplitRatio=0.70) 
traindata=BANK[train,] 
testdata=BANK[!train,]
TestLoan=BANK$Personal.Loan[!train]

#fit the tree model using training data
treemodel=tree(Personal.Loan~.,traindata)
treemodel
plot(treemodel,col="blue")
text(treemodel,cex=0.75) 
summary(treemodel)

#check how the model is performing using the test data
treeprediction=predict(treemodel,testdata,type="class")
mean(treeprediction!=TestLoan)  
treeprediction
t <- table(TestLoan,treeprediction)
result[j,1]<- j
result[j,2]<- sum(diag(t))/sum(t)
result[j,3]<- t[2,2] / (t[2,1]+t[2,2])
result[j,4]<- t[1,1] / (t[1,1]+t[2,1])


#pruning
set.seed(4)
cv_tree=cv.tree(treemodel,FUN = prune.misclass)
names(cv_tree)
plot(cv_tree$size,cv_tree$dev, type="b")
pruned_model=prune.misclass(treemodel,best=7)
plot(pruned_model)
text(pruned_model,pretty = 0)
summary(pruned_model)

##Plotting Roc curve and calculating AUC metric
pred_model<-predict(treemodel,testdata,type="vect")
auc<-auc(TestLoan,pred_model[,2])
auc
plot(roc(TestLoan,pred_model[,2]),col="red",main="ROC CURVE")

