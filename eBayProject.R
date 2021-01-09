setwd("users/krinalmanakiwala/desktop/RProjects")
data = read.csv("eBayAssignment.csv")
library(dplyr)
library(caTools)

##1. How many rows are in the data? ANS: 1861
nrow(data)

##2. How many iPads are Black in color? ANS: 425
summary(data$color)

##3. Which all types of iPads this dataset contains? ANS: iPad 1, iPad 2, iPad 3, iPad 4, iPad Air 1/2, iPad mini, iPad mini 2, iPad mini Retina, iPad mini3, Unknown 
levels(data$productline)

##4. What is the uniqueID of the iPad with the highest startprice? ANS:11397
data$UniqueID[which(data$startprice == max(data$startprice), arr.ind=TRUE)]

##5. Split the Data into a train sample and a test sample using a seed of 196 such that 80% of the data is in the train sample. Use sample.split from library(caTools). Hereafter, we will only use the train sample for exploring and building the model. The test sample will only be utilized for evaluating the model.How many rows are in the train sample?
RNGkind(sample.kind = "Rounding")
colSums(is.na(data))
set.seed(100)
split = sample.split(data$sold, SplitRatio = 0.8)
train = data[split,]
test = data[!split,]
##ANS: 1489 rows in training
nrow(train)
nrow(test)

##6. What is the median startprice of iPads that sold? ANS: 99
median(data$startprice)

soldData = data %>% filter(data$sold == T)
median(soldData$startprice)

##7. What is the median startprice of iPads that did not sell? ANS: 250

unsoldData = data %>% filter(data$sold == F)
median(unsoldData$startprice)

#8. Build a logistic regression model to predict whether the iPad will be sold or not

model = glm(formula = sold~.-UniqueID, data = train, family='binomial')

##AIC = 1420.4
summary(model)

##9. Now, let us examine individual variables. Which of the variables has a significant influence on whether an iPad is sold or not? (Select all that apply). Use a less conservative alpha of 0.10, i.e., compare p-value to 0.10.
##with a p-value of >0.05: biddable, startprice, condition, cellular, productline
##QUESTION^^ In some variables, only some of its factors are significant... does this mean the entire variable is significant?
##with a p-value of >0.1: biddable, startprice, condition, cellular, productline

##10. Based on the results of the model, does a 99 ending for startprice increase the chance of an iPad being sold? (Yes/No): no

##11. Based on the results of the model, does color of the iPad have an impact on whether an iPad is sold? (Yes/No): No

##12. Simpler models are generally preferred to more complex models because they are less likely to overfit the data. So, let us drop out non-signficant variables from model1 above but keep variables that previous research or experience indicates should have an effect.
##So, estimate generate model2 with the following variables: biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end
##What is the AIC (round to one decimal place)? ANS: 1412.8 (lower than previous model)
model2 = glm(formula = sold~biddable+startprice+condition+storage+productline+upperCaseDescription+startprice_99end, data = train, family='binomial')
summary(model2)

##13. Based on the coefficient of upperCaseDescription, what advice would you give someone selling iPads on eBay?
model2$coef[18]
##Since the coefficient is near 0, whether the description is upper case or not does not affect the likelihood of the iPad getting sold

##14. Review the results of model2 and the coefficients of the variables. (After controlling for the effects of all other variables in the model), what sells better iPad3 or iPad 1?
summary(model2)
##the iPad 3 has a better chance of selling because it has a higher coefficient compared to the iPad 1

##15. If startprice goes up by $1, what will be the % reduction in the chance of selling an iPad? To interpret coefficients in logistic regression, you have to exponentiate the coefficient. E.g., exp(coefficient)
##0.99% reduction
##ok so the coef is the actual value so for a dollar more you gotta add one and then subtract the difference between the predictions of the original and plus one
exp(summary(model2)$coef[3]) - exp(summary(model2)$coef[3]+1)

##16.  Based on model2 (and controlling for the effects of all other variables), how much more (or less) likely is an iPad Air 1/2 to sell compared to an iPad 1? ANS: 7.540753 times more likely
summary(model2)$coef
summary(model2)$coef[12]
exp(summary(model2)$coef[12])

##17. Make predictions on the test set using model2. Place all the predictions in a variable "pred".Now, let us use the model to find out what is the probability of sale for an iPad with UniqueID 10940?

pred = predict(model2, newdata = test, type = 'response')
##Error in eval(predvars, data, env) : object 'biddable' not found

##you gotta use the index of the ID and find it in the predict function since there is no ID in the predict fumction
predict(model2, newdata=data.frame(UniqueID = 10940), type = 'response')

##18. What is the accuracy of model2 on the test set? Use a threshold of 0.5. ANS: 0.7795699
ct = table(test$sold,pred>0.5);ct 
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy

##19.What is the auc of model 2 on test set? ANS: 0.8511773
library(ROCR)
ROCRpred = prediction(pred,test$sold)
as.numeric(performance(ROCRpred,"auc")@y.values)

