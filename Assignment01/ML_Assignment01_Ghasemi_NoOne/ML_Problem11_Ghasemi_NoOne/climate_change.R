# ML_Assignment01_Ghasemi_NoOne
# Mohammad Sadegh Ghasemi

# Loading the dataset
climate = read.csv("climate_change.csv")
# str(climate)
print('summary of climate data')
summary(climate)
#########################################

# divide dataset into train and test sets based on year
splitYear = 2006
# splitting the dataset and removing Year and month
train = subset(climate, Year <= splitYear, select= MEI:Temp)
test = subset(climate, Year > splitYear, select= MEI:Temp)
# train = subset(climate, Year <= splitYear)
# test = subset(climate, Year > splitYear)
print('test and train data created')
########################################

# Creating linear model with Temp as independent variable
model1 = lm(Temp ~ ., data=train)
# model1 = lm(
#     Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
#     data = train)

# summary(model1)
# plotting model
print('model1 created by all variables')
print('summary of the model1')
summary(model1)
plot(model1)
#######################################

# Computing R squared

# Simple way
# print(summary(model1)$r.squared)=

# hard way
meanTemp = mean(train$Temp)
SST = sum((train$Temp - meanTemp) ^ 2)
SSE = sum(model1$residuals ^ 2)
r.squared = 1 - (SSE / SST)
print(paste('R Squared of model1: ', r.squared))
#####################################

# checking for correlations among variables
# plotting variables correlations
library(corrplot)
corrplot(cor(train), method='number')
corrplot(cor(train), method='circle')

## Correlations of N2O with other variables
# cor(train)[4,]

## absolute correlation be greater than 0.7
# cor(train)[4,abs(cor(train)[4,]) > 0.7]
####################################

# creating new model with less variables
model2 = lm(Temp ~ MEI + CO2 + TSI + Aerosols, data = train)
s2 = summary(model2)
# plot(model2)

# print R Squared of the model
print(s2$r.squared)
###################################

# Automatically building the model with R's step function
model3 = step(model1)
s3 = summary(model3)

# new R squared value:
print(s3$r.squared)
##################################

# Predicting test set Temp using model3
predicted = predict(model3, test)

# Compute R Squared of test set
SSE = sum((predicted - test$Temp) ^ 2)
SST = sum((mean(train$Temp) - test$Temp) ^ 2)
r.squared = 1 - (SSE / SST)

print(paste('SSE: ', SSE))
print(paste('SST: ', SST))
print(paste('R Squared: ', r.squared))

RMSE = sqrt(SSE / nrow(test))
print(paste('RMSE: ', RMSE))
