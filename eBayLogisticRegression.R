setwd("users/krinalmanakiwala/desktop/RProjects")
data = read.csv("eBayClean.csv")

str(data)

##what does this library do? - sample.split
library(caTools)

##what's the difference between split and sample.split?
set.seed(100)
RNGkind(sample.kind = "Rounding")
set.seed(100)
split = sample.split(data$sold, SplitRatio = 0.7)
train = data[split,]
test = data[!split,]
nrow(train)
nrow(test)

library(ggplot2)

##why is there no description as a variable?
names(train)

##The average start price for ipads that weren't sold is higher than that of ipads that were sold
tapply(train$startprice, train$sold, mean)
##table of biddability vs selling
tapply(train$sold, train$biddable, mean)
ggplot(data = train, aes(x=biddable, y=sold, fill=biddable)) + geom_bar(stat = 'summary', fun.y='mean')
##table of condition vs selling
tapply(train$sold, train$condition, mean)
ggplot(data = train, aes(x=condition, y=sold, fill=condition)) + geom_bar(stat = 'summary', fun.y='mean')

model = glm(sold~startprice, data=train, family='binomial')
summary(model)

##what is the probability of an iPad priced at $200 selling?
exp(model$coef[1] + model$coef[2]*200)/(1+exp(model$coef[1] + model$coef[2]*200))
##an easier way to do the same thing: (the type='response' tells the predict function that it is exponential, so it'll give probability)
predict(model, newdata=data.frame(startprice = 200), type = 'response')

levels(data$storage)

##I'm not getting the same values as him for some reason :(
model2 = glm(formula = sold~storage, data=train, family='binomial')

summary(model2)

##how many times better is the chance of selling a 16/32/64 GB iPad relative to 128GB
exp(summary(model2)$coef[2])
##This means there's a 1.32 higher chance. for percentage form:
100*exp(summary(model2)$coef[2]-1)

predict(model2,newdata=data.frame(storage='128 GB'), type='response')
model3 = glm(sold~.-UniqueID, data=train, family="binomial")
summary(model3)
##this has a higher AIC than the other model

pred = predict(model3, type='response')

pred[1:5]
##Table can count the frequency of a certain piece of data 
table(as.numeric(pred>0.5))
##makes a classification matrix
ct = table(train$sold, pred>0.5); ct

fpr =ct[1,2]/(ct[1,1] + ct[1,2])
fpr
tpr =ct[2,2]/(ct[2,2]+ct[2,1])
tpr

model4= glm(sold~.-UniqueID, data=test, family="binomial")
pred2 = predict(model4, type='response')

ct2 = table(test$sold, pred2>0.5); ct2

fpr2 =ct2[1,2]/(ct2[1,1] + ct2[1,2])
fpr2
tpr2 =ct2[2,2]/(ct2[2,2]+ct2[2,1])
tpr2

install.packages("ROCR")
library(ROCR)
##This internally calculates FPR, TPR, etc..
ROCRpred = prediction(pred2, test$sold)
as.numeric(performance(ROCRpred, 'auc')@y.values)

ROCRperf = performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf)
##This just relabels the axis
plot(ROCRperf, xlab = '1-specificity', ylab='sensitivitys')

control = rpart
