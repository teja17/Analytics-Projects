# get n set working directory
getwd()
setwd("C:\\Users\\Tejaswini\\Desktop\\Advanced Business Analytics\\Assignment")
# read csv file
data <- read.csv("election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 
# generate summary of data
summary(data)
colnames(data)
# drop columns
data <- subset(data,select=-c(cand_id, last_name, first_name, twitterbirth, facebookdate, facebookjan, youtubebirth))
# factor variables
data$twitter = as.factor(data$twitter)
data$facebook = as.factor(data$facebook)
data$youtube = as.factor(data$youtube)
data$cand_ici = as.factor(data$cand_ici)
data$gen_election = as.factor(data$gen_election)

# drop NA rows
data <- data[complete.cases(data),]
table(is.na(data))
summary(data)

## Create the training and test data:
n = nrow(data) # n will be ther number of obs. in data
trainIndex = sample(1:n, 
                    size = round(0.7*n), 
                    replace=FALSE) # We create an index for 70% of obs. by random
train_data = data[trainIndex,] # We use the index to create training data
test_data = data[-trainIndex,] # We take the remaining 30% as the testing data

summary(train_data)
summary(test_data)

# Install packages required for random forest:
#install.packages("randomForest")

# Load packages required for random forest:
library(randomForest)

?randomForest()
# build random forest model
rf <- randomForest(gen_election~., data=train_data, ntree=40, na.action=na.exclude, importance=T,proximity=T) 
print(rf)

#optimal mtry
mtry <- tuneRF(train_data[-26], train_data$gen_election, ntreeTry=50,  stepFactor=1.5, improve=0.01, trace=TRUE, plot=TRUE)

best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

print(mtry)
print(best.m)

# build random forest model 
rf <- randomForest(gen_election~.,mtry=best.m, data=train_data, ntree=50, na.action=na.exclude, importance=T,proximity=T) 
print(rf)

# use the classifier to make predictions and create the confusion matrix:
library(caret)
predicted_values <- predict(rf, test_data,type= "prob")
head(predicted_values)

threshold <- 0.5
pred <- factor( ifelse(predicted_values[,2] > threshold, 'W', 'L') )
levels(test_data$gen_election)

confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])
?confusionMatrix()


#create the ROC curve as well as calculate AUC
library(ROCR)
library(ggplot2)

predicted_values <- predict(rf, test_data,type= "prob")[,2] 
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

#Evaluate variable importance
importance(rf)
varImpPlot(rf)

# Load packages required for ANN:
library("nnet")

summary(train_data)
ann <- nnet(gen_election ~ ., data=train_data, size=5, maxit=1000) # Size is the number of units (nodes) in the hidden layer.
# In the output, b represents the bias associated with a node, h1 represents hidden layer node 1, i1 represents input node 1 (i.e., input variable 1), o  represents the output node.
summary(ann)
print(ann)

predicted_values <- predict(ann, test_data,type= "raw") # Use the classifier to make the predictions. With the package that we used, type "raw" will give us the probabilities 
head(predicted_values) # predictions (probabilities)

threshold <- 0.5 
pred <- factor( ifelse(predicted_values[,1] > threshold, "W", "L") ) # We ask R to use the threshold and convert the probabilities to class labels (zero and one)
head(pred) #  predicted class labels

levels(test_data$gen_election)[2]

# pred <- relevel(pred, 1)   
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2]) # Creates the confusion matrix. It is important to determine positive in this function. The option positive sets the 1s as class positive.

predicted_values <- predict(ann, test_data,type= "raw")
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]

roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")

ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))


#find max no of hidden nodes possible
ann1 <- nnet(gen_election ~ ., data=train_data, size=24, maxit=1000)
#ROC and AUC
predicted_values <- predict(ann1, test_data,type= "raw")
pred <- prediction(predicted_values, test_data$gen_election)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc))

head(train_data)
?ftable()


#three way frequency table
ftable(xtabs(~twitter+facebook+youtube+gen_election, data=data)) 

ftable(xtabs(~twitter+gen_election, data=data)) 
ftable(xtabs(~facebook+gen_election, data=data)) 
ftable(xtabs(~youtube+gen_election, data=data)) 



predicted_values1 <- predict(ann1, test_data,type= "raw") # Use the classifier to make the predictions. With the package that we used, type "raw" will give us the probabilities 
head(predicted_values) # predictions (probabilities)

threshold <- 0.5 
pred <- factor( ifelse(predicted_values1[,1] > threshold, "W", "L") ) # We ask R to use the threshold and convert the probabilities to class labels (zero and one)
head(pred) #  predicted class labels

levels(test_data$gen_election)[2]

# pred <- relevel(pred, 1)   
confusionMatrix(pred, test_data$gen_election, 
                positive = levels(test_data$gen_election)[2])
