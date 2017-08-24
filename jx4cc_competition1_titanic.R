library(MASS)

train <- read.csv("/Users/jamesxie/Desktop/Data Mining/train.csv")
train$Survived

#Independent Variables used: Gender, Age, Pclass, Fare, Number of Parents/Children,
#and Number of Siblings/Spouses
lm(formula = train$Survived ~ as.numeric(train$Sex) + train$Age + 
      train$Fare + train$Parch + train$SibSp, data = train)

test$survived <- NA
for(i in 1:418) {
  if(is.na(test$Age[i])) {
    test$Age[i] <- 0
  }
  if(is.na(test$Fare[i])) {
    test$Fare[i] <- 0
  }
  test$survived[i] <- as.numeric(1.334610 - 0.526321*as.numeric(test$Sex[i]) - 
    0.003120*test$Age[i] + 0.001925*test$Fare[i] - 0.030355*test$Parch[i] - 
      0.057025*test$SibSp[i] > 0.5)
}

results <- data.frame(matrix(ncol = 2, nrow = 418))
colnames(results) <- c("PassengerID", "Survived")
for (i in 1:418) {
  results$`PassengerID`[i] <- test$PassengerId[i]
  results$Survived[i] <- test$survived[i]
}
write.csv(results, file = "jx4cc_comp1_predictions.csv")

