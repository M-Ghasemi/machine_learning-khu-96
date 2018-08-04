# ML_Assignment02_Ghasemi_NoOne
# Mohammad Sadegh Ghasemi

# Loading and exploring the dataset
loans = read.csv("loans.csv")
print('loans data loaded')
str(loans)
# summary(loans)
##################################

# Full paid loans
# print('Full paid loans:')
prop.table(table(loans$not.fully.paid))[1] * 100
# nrow(subset(loans, loans$not.fully.paid == 0)) / nrow(loans) * 100
# Ans: 83.99457
####################################

# missing variables
names(loans)[apply(is.na(loans), 2, any)] #or
# names(which(apply(is.na(loans), 2, sum) > 0)) # which gives col names as well as col no
#Ans:log.annual.inc, days.with.cr.line, revol.util, inq.last.6mths, delinq.2yrs and pub.rec

# missing record
missing =  loans[rowSums(is.na(loans)) > 0,]
# missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
# Ans: 62
# Only 62 of 9578 loans have missing data

# how many people with missing values did not pay their loans?
table(missing$not.fully.paid)
# we see that 12 of 62 loans with missing data were not fully paid, or 19.35%.

pMiss = function(x){
    sum(is.na(x)) / length(x) * 100
}
# how many of data are missing?
apply(loans,2,pMiss)
md.pattern(loans)

# visualizing for better understanding
library(VIM)
# if bellow line did not work correctly, run it twice without closing plot window
aggr_plot <- aggr(
    loans,
    col=c('navyblue', 'red'),
    numbers=TRUE,
    sortVars=TRUE,
    labels=setdiff(names(loans), "not.fully.paid"),
    cex.axis=.7,
    gap=3,
    ylab=c("Histogram of missing data","Pattern"))
##########################################################

# Imputing missing data with mice package
# Predicting the missing values using other independent variables.
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
####################################

# - Splitting data into a training and testing set
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)

# library(caret)
# inTrain = createDataPartition(y=loans$not.fully.paid, p=0.7, list=FALSE)
# train = loans[inTrain, ]
# test = loans[-inTrain, ]

# Logistic regression with all variables
model1 = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(model1)
# Significant variables:
#    credit.policy ,purpose2,purpose3, purpose6, purpose7 ,installment ,log.annual.inc ,fico,revol.bal,inq.last.6mths ,pub.rec

# plotting model
plot(model1)

# checking for correlations among variables
# plotting variables correlations
library(corrplot)
corrplot(cor(loans[-2]), method='number')
# corrplot(cor(loans[-2]), method='circle')

# Creating a model with less variables
model2 = glm(
    not.fully.paid ~ . - int.rate - dti - days.with.cr.line - revol.util - delinq.2yrs,
    data=train,
    family=binomial)

check_accuracy = function(model, test) {
    predicted.risk = predict(model,type="response", newdata=test)

    # Confusion matrix with threshold of 0.5
    confusion_matrix = table(test$not.fully.paid, as.numeric(predicted.risk >= 0.5))
    #What is the accuracy of the logistic regression model? Input the accuracy as a number between 0 and 1.
    sum(diag(confusion_matrix)) / sum(confusion_matrix)
}

model1_accuracy = check_accuracy(model1, test)
model2_accuracy = check_accuracy(model2, test)
print(paste("model 1 accuracy: ", model1_accuracy))
print(paste("model 2 accuracy: ", model2_accuracy))
###########################################

# Applying model on test data
test$predicted.risk = predict(model2, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
confusion_matrix = table(test$not.fully.paid, as.numeric(test$predicted.risk >= 0.5))
# Accuracy of the model 2
sum(diag(confusion_matrix)) / sum(confusion_matrix)
#Ans: 0.8364079

library(caret)
confusionMatrix(confusion_matrix)
#######################################
# ROC curve
library(pROC)
plot(roc(not.fully.paid ~ installment, data=train), col='red', main='roc plot of credit.policy')
plot(roc(not.fully.paid ~ installment, data=train), col='red', main='roc plot of installment')
plot(roc(not.fully.paid ~ log.annual.inc, data=train), col='red', main='roc plot of log.annual.inc')
plot(roc(not.fully.paid ~ fico, data=train), col='red', main='roc plot of fico')
plot(roc(not.fully.paid ~ inq.last.6mths, data=train), col='red', main='roc plot of inq.last.6mths')
plot(roc(not.fully.paid ~ pub.rec, data=train), col='red', main='roc plot of pub.rec')
#######################################

# Test set AUC
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)
#Ans:0.6721337
