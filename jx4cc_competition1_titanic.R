library(MASS)

train <- read.csv("/Users/jamesxie/Desktop/Data Mining/train.csv")
train$Survived <- as.factor(train$Survived)

#The first thing that I want to test is the correlation between every individual variable
#and whether or not the passenger had survived, I'll be paying special attention to
#the ones with the lowest deviances.

#PClass vs Survival
glm(formula = train$Survived ~ as.factor(train$Pclass), family=binomial(link="logit"))
#Residual Deviance: 1083

#Gender vs Survival
glm(formula = train$Survived ~ as.factor(train$Sex), family=binomial(link="logit"))
#Residual Deviance: 917.8

#Age vs Survival
glm(formula = train$Survived ~ train$Age, family=binomial(link="logit"))
#Residual Deviance: 960.2

#Num of Siblings/Spouses vs Survival
glm(formula = train$Survived ~ train$SibSp, family = binomial(link = "logit"))
#Residual Deviance: 1186

#Num of Parents/Children vs Survival
glm(formula = train$Survived ~ train$Parch, family = binomial(link = "logit"))
#Residual Deviance: 1181

#Fare vs Survival
glm(formula = train$Survived ~ train$Fare, family = binomial(link = "logit"))
#Residual Deviance: 1118

#Embarkation Port vs Survival
glm(formula = train$Survived ~ as.factor(train$Embarked), family = binomial(link = "logit"))
#Residual Deviance: 1157 

#Now I'll look for correlations between multiple variables and survival,
#and try to find the model with the lowest residual deviance.

#Independent Variables: Gender and Age
glm(formula = train$Survived ~ train$Sex + train$Age, family = binomial(link = "logit"))
# Residual Deviance: 750

#Independent Variables: Gender and Pclass
glm(formula = train$Survived ~ train$Sex + train$Pclass, family = binomial(link = "logit"))
#Residual Deviance: 827.2

#Independent Variables: Gender, Age, and Pclass
glm(formula = train$Survived ~ train$Sex + train$Age + train$Pclass, family = binomial(link = "logit"))
# Residual Deviance: 647.3

#Independent Variables: Gender, Age, Pclass, and Fare
glm(formula = train$Survived ~ train$Sex + train$Age + train$Pclass + train$Fare, family = binomial(link = "logit"))
# Residual Deviance: 647.2

#Independent Variables: Gender, Age, Pclass, Fare, and Embarkation Port
glm(formula = train$Survived ~ train$Sex + train$Age + train$Pclass 
    + train$Fare + train$Embarked, family = binomial(link = "logit"))
# Residual Deviance: 642.7

#Independent Variables: Gender, Age, Pclass, Fare, Embarkation Port, Number of Parents/Children
glm(formula = train$Survived ~ train$Sex + train$Age + 
      train$Pclass + train$Fare + train$Embarked + train$Parch, family = binomial(link = "logit"))
# Residual Deviance: 640.9

#Independent Variables: Gender, Age, Pclass, Fare, Embarkation Port, Number of Parents/Children,
#and Number of Siblings/Spouses
glm(formula = train$Survived ~ as.factor(train$Sex) + train$Age + 
      as.factor(train$Pclass) + train$Fare + train$Embarked + train$Parch +
      train$SibSp, family = binomial(link = "logit"))
# Residual Deviance: 632.3

#Just looking at the individual variables, gender has the highest correlation with a passenger's survival.
#But the model that gives us the lowest residual deviance includes all the variables that I used,
#which consists of: Gender, Age, Pclass, Fare, Embarkation Port, Num of Parents/Children,
#and Number of Siblings/Spouses. Hence, that will be the model that I'll use to predict
#whether or not the test subjects survive the Titanic sinking or not.

test$survived <- NA
pClassCoef <- 0
EmbarkCoef <- 0
for(i in 1:418) {
  if(test$Pclass == 2) {
    pClassCoef <- -1.189637
  }
  else if (test$Pclass == 3) {
    pClassCoef <- -2.395220
  }
  if(test$Embarked == "C") {
    EmbarkCoef <- -12.257443
  }
  else if(test$Embarked == "Q") {
    EmbarkCoef <- -13.080988
  }
  else if(test$Embarked == "S") {
    EmbarkCoef <- -12.658656
  }
  test$survived[i] <- 16.691979 - 2.637859*test$Sex[i] - 0.043308*test$Age[i] + pClassCoef*test$Pclass[i] + 
    0.001451*test$Fare[i] + EmbarkCoef*test$Embarked[i] - 0.060365*test$Parch[i] - 0.362925*test$SibSp[i]
}





