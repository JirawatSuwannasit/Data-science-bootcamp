library(titanic)

head(titanic_train)

## DROP NA (missing values)
titanic_train <- na.omit(titanic_train)
nrow(titanic_train)

## convert sex to factor
titanic_train$sex_new <- factor(titanic_train$Sex,
                    levels = c("male","female"),
                    labels = c("0", "1"))

## SPLIT DATA
set.seed(33)
n <- nrow(titanic_train)
id <- sample(1:n, size=n*0.7) ##70% train 30% test
train_data <- titanic_train[id, ]
test_data <- titanic_train[-id, ]

## Train Model
train_model <- glm(Survived ~ Pclass + Age + sex_new, data = train_data, family = "binomial")

summary(train_model)

## Predict and Evaluate Model
train_data$prob_Survived <- predict(train_model, type="response")
train_data$prod_Survived <- ifelse(train_data$prob_Survived >= 0.5, 1,0)

## confusion matrix
conM <- table(train_data$prod_Survived, train_data$Survived, dnn= c("Predicted", "Actual"))

## Model Evaluation
accuracy_train <- ((conM[1,1] + conM[2,2])/sum(conM))
precision_train <- (conM[2,2]/(conM[2,1] + conM[2,2]))
recall_train <- (conM[2,2]/(conM[1,2] + conM[2,2]))

F1_train <-(2*((precision_train * recall_train)/(precision_train + recall_train)))

## Test Model

## Predict and Evaluate Model
test_data$prob_Survived <- predict(train_model,newdata = test_data, type="response")
test_data$prod_Survived <- ifelse(test_data$prob_Survived >= 0.5, 1,0)

## confusion matrix
conN <- table(test_data$prod_Survived, test_data$Survived, dnn= c("Predicted", "Actual"))

## Model Evaluation
accuracy_test <- ((conN[1,1] + conN[2,2])/sum(conN))
precision_test <- (conN[2,2]/(conN[2,1] + conN[2,2]))
recall_test <- (conN[2,2]/(conN[1,2] + conN[2,2]))

F1_test <- (2*((precision_test * recall_test)/(precision_test + recall_test)))
