# ML_Assignment01_Ghasemi_NoOne
# Mohammad Sadegh Ghasemi

# Loading the dataset
setwd('/home/mohammad/cpas/exercises/ML-Taherian/Assignment02/ML_Assignment02_Ghasemi_NoOne/‫‪ML_Problem21_Ghasemi_NoOne')
gerber <- read.csv('gerber.csv')
str(gerber)

########################
#######Question 1#######
########################
# How many peaple have voted
(voting_table = table(gerber$voting))


# What percentage of people have voted?
print(mean(gerber$voting))
# print(as.numeric(voting_table[2] / sum(voting_table)))

# Which of the four “treatment groups” had the largest percentage of people who actually voted (voting = 1)?
max_voted.group = ''
max_voted.percentage = 0
for (group in names(gerber)[4:length(names(gerber))]) {
    vote_percentage = round(mean(gerber[gerber[group] == 1, ]$voting) * 100)
    print(paste(sep='', group, ': ', vote_percentage, '% voted'))
    if (max_voted.percentage < vote_percentage) {
        max_voted.group = group
        max_voted.percentage = vote_percentage
    }
}
writeLines(paste('\n', max_voted.group, 'group had the largest percentage of people who voted'))

########################
#######Question 2#######
########################

# Logistic regression model for voting using the "civicduty, hawthorne, self, neighbors" as the independent variables
logistic_model <- glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial)

# Which of the variables are significant in the logistic regression model?
summary(logistic_model)
# So all 4 variables "civicduty", "hawthorne", "self", "neighbors" are significant.

# plotting the model
plot(logistic_model)

# Using a threshold of 0.3, what is the accuracy of the logistic regression model?
predict_logistic_model <- predict(logistic_model, type="response")
(confusion_matrix = table(gerber$voting, predict_logistic_model >= 0.3))
accuracy = sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Model accuracy with a threshold of 0.3:", accuracy))

# Using a threshold of 0.5, what is the accuracy of the logistic regression model?
(confusion_matrix = table(gerber$voting, predict_logistic_model >= 0.5))
accuracy = confusion_matrix[1] / sum(confusion_matrix)
print(paste("Model accuracy with a threshold of 0.5:", accuracy))

# Compare your model accuracy to the accuracy of the bellow baseline models
# -All people vote
# -No one vote
all_people_model_accuracy = nrow(subset(gerber, gerber$voting == 1)) / nrow(gerber)
no_one_model_accuracy = nrow(subset(gerber, gerber$voting == 0)) / nrow(gerber)
print(paste("Accuracy of the model in which all people vote:", all_people_model_accuracy))
print(paste("Accuracy of the model in which no one vote:", no_one_model_accuracy))
# Our model accuracy with thresholds of 0.5, 0.3 was 0.68, 0.54 respectively
# so it's not better than the second baseline model

# AUC of the model
library(ROCR)
ROCRpred = prediction(predict_logistic_model, gerber$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)

# AUC of the baseline models
baseline_ROCRpred = prediction(floor(predict_logistic_model), gerber$voting)
baseline_auc = as.numeric(performance(baseline_ROCRpred, "auc")@y.values)
# baseline_ROCRpred = prediction(ceiling(predict_logistic_model), gerber$voting)
# baseline_auc = as.numeric(performance(baseline_ROCRpred, "auc")@y.values)

writeLines(paste(
    "Our model AUC:", round(auc, 4),
    "\nbaseline model AUC:", round(baseline_auc, 4)))
# Our model is a weak predictive model, but a little bit better than baseline model

# plotting performance
perf_tpr_fpr = performance(ROCRpred, "tpr", "fpr")
perf_prec_rec = performance(ROCRpred, "prec", "rec")
perf_sens_spec = performance(ROCRpred, "sens", "spec")

# true_positive_rate/true_negative_rate curve
plot(perf_tpr_fpr)
# precision/recall curve
plot(perf_prec_rec)
# sensitivity/specificity curve
plot(perf_sens_spec)

########################
#######Question 3#######
########################

# Build a CART tree for voting using all data and the same four treatment variables we used before.
library(rpart)
library(rpart.plot)
CART_model1 <- rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
prp(CART_model1)
# There are no splits in the tree, because none of the variables make a big enough effect to be split on.

########################
#######Question 4#######
########################

# Use the parameter cp (cp=0.0) to build the tree.
# What is the order of branching?
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

# What is the order of branching?
# The highest fraction of voters are in the Neighbors, Self, Hawthorne and Civic Duty groups respectively.

summary(CARTmodel2)

########################
#######Question 5#######
########################

# Using only the CART tree plot, determine what fraction of "Civic Duty" group voted?
# 0.31

########################
#######Question 6#######
########################

# Add "sex" to the variables used before, again with cp = 0.0.
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex + control, data=gerber, cp=0.0)
prp(CARTmodel3)

# Pay attention to the importance of the sex variable.
# sex is the least important variable.

# In the control group, which gender is more likely to vote?
# Men

# What about "civic duty" group?
# Men

########################
#######Question 7#######
########################

# Create a regression tree using just the "control" variable.
# Also create a tree with the "control" and "sex" variables.
# Set cp=0.0 for both of above trees.
CARTcontrol_tree = rpart(voting ~ control, data=gerber, cp=0.0)
CARTcontrol_sex_tree = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontrol_tree, digits=6)
prp(CARTcontrol_sex_tree, digits=6)

# What is the absolute difference in the predicted probability of voting between being in the control group versus being in other group's, In the "control" only tree?
# ‫‪abs(control‬‬ ‫‪prediction‬‬ ‫–‬ ‫‪non-control‬‬ ‫)‪prediction‬‬
control_splitted_voting = CARTcontrol_tree$frame$yval[CARTcontrol_tree$frame$var == "<leaf>"]
print(abs(control_splitted_voting[1] - control_splitted_voting[2]))

# Now, using the second tree, determine what gender is affected more by NOT being in the control group
c_s_splitted_voting = CARTcontrol_sex_tree$frame$yval[CARTcontrol_sex_tree$frame$var == "<leaf>"]
affected_women = abs(c_s_splitted_voting[1] - c_s_splitted_voting[3])
affected_men = abs(c_s_splitted_voting[2] - c_s_splitted_voting[4])
writeLines(paste(
    "The amount of affected men:", round(affected_men, 6),
    "\nThe amount of affected women:", round(affected_women, 6),
    "\nWomen are more affected about:", round(affected_women - affected_men, 6),
    "\nSo they are affected about the same because the difference is very small."
    ))

########################
#######Question 8#######
########################

# Go back to logistic regression.
# Create a model using "sex" and "control".
sex_control_log_model = glm(voting ~ control + sex, data=gerber, family="binomial")
summary(sex_control_log_model)

# What is the coefficient for "sex" and what does it mean?
as.numeric(sex_control_log_model$coefficients['sex'])

# What is the absolute difference between the logistic regression and the decision tree prediction for the (Woman, Control)?
woman_control = data.frame(sex=1, control=1)
woman_control_predict = as.numeric(
    predict(sex_control_log_model, newdata=woman_control, type='response'))
print(round(abs(woman_control_predict - c_s_splitted_voting[1]), 5))

# Add combination of sex and control to logistic regression as a new variable.
# 1 if person is woman and belongs to control group
sex_control_combination_lm = glm(
    voting ~ sex + control + sex:control,
    data=gerber, family="binomial")
summary(sex_control_combination_lm)

# How the new variable related to the output(dependent variable)?
# Variable coefficient is negative (-0.007259), so a value of 1 in this variable decreases the chance of voting.
# If the person is a woman belonging to the control group, there is less chance that she will vote.

# What is the absolute difference between the last logistic regression model and the decision tree prediction for the (Woman, Control)?
woman_control_predict = as.numeric(
    predict(sex_control_combination_lm, newdata=woman_control, type='response'))
print(round(abs(woman_control_predict - c_s_splitted_voting[1]), 5))
