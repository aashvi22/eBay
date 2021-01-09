setwd("users/krinalmanakiwala/desktop/RProjects")
data = read.csv("eBayClean.csv")
library(caTools)
RNGkind(sample.kind = 'Rounding')
set.seed(100)
split = sample.split(data$sold, SplitRatio = 0.7)
train = data[split,]
test = data[!split,]
nrow(train)
nrow(test)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

##Regression Tree
tree1 = rpart(sold~startprice, data = train)
rpart.plot(tree1)
summary(tree1)
##complexity of 0.2244469

##Classification tree
##the second one has less leaves than the regression tree because it has a lower complexity number and the regression tree has a higher densiy
##this one starts witha higher cp number (complexity) so it 
tree2 = rpart(sold~startprice, data = train, method = 'class', cp = 0.00001)
rpart.plot(tree2)
summary(tree2)
##why isn't this working? it should be 39
sum(train$startprice>=152 && train$startprice<172)

##Tree3 (based on storage) regression tree
tree3 = rpart(sold~storage, data = train, method = 'anova')
rpart.plot(tree2)
##There's no split because you can't run regression on catagorical variable
summary(tree3)

##Tree4 (based on storage) classification tree
tree4 = rpart(sold~storage, data = train, method = 'class')
rpart.plot(tree4)
##There's no split because you can't run regression/classification on catagorical variable. you can't split categorical variables!
summary(tree4)

tree5 = rpart(sold~.-UniqueID, data = train, method = 'class')
rpart.plot(tree5)
summary(tree5)
##based on the tree, the most important are biddable, startprice, productline in that order
##you can see this in summary
##primary split is a split in the data and surrogate split is a split caused by the primary split

##type = class for logistic regression 
##question: what is minbucket?
pred = predict(tree5, newdata = test, type = 'class')
ct = table(test$sold, pred); ct

fpr =ct[1,2]/(ct[1,1] + ct[1,2])
fpr
tpr =ct[2,2]/(ct[2,2]+ct[2,1])
tpr
##the logistic had a higher TPR than this decision tree. This means decision tree is slightly worse
library(ROCR)
##ERROR: Error in prediction(pred, test$sold) : Format of predictions is invalid.
ROCRpred = prediction(pred, test$sold)
as.numeric(performance(ROCRpred, 'auc')@y.values)

accuracy = sum(ct[1,1],ct[2,2])/nrow(test)
accuracy

##complexity of 0.4285714