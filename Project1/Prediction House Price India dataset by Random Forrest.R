library(caret)
library(readxl)
library(dplyr)
library(ggplot2)

df <- read_excel("House Price India.xlsx")

df_select <- select(df, 3, 4, 5, 6, 7, 11, 12, 22, 23)


#check type each column
str(df_select)

# Check for missing values
is.na(df_select)

## DROP NA (missing values)
df_train <- na.omit(df_select)
nrow(df_train)

##Revise the structuring

colnames(df_train)[1:9] <- c("no_bed", "no_bath", "liv_area", "lot_area", "no_floor", "grade_house", "area_house", "airport_near", "price")
head(df_train)

## Split Data
set.seed(33)
n <- nrow(df_train)
id <- sample(1:n, size=n*0.8)
train_data <- df_train[id, ]
test_data <- df_train[-id, ]

#check distribution of price 
ggplot(train_data, aes(x = price)) + geom_histogram()

#normalize data
train_data$price_norm <- log(train_data$price)
test_data$price_norm <- log(test_data$price)

#check distribution of price after normalizing
ggplot(train_data, aes(x = price_norm)) + geom_histogram()

#train model
ctrl <- trainControl(
  method = "cv", #k-fold
  number = 5, # k-5
  verboseIter = TRUE
)

model <- train(price_norm ~ no_bed + liv_area + lot_area + grade_house + airport_near,
               data = train_data,
               method = 'rf',
               trControl = ctrl )
               
## Score Test model
p_test <- predict(model, newdata = test_data)

## Model Evaluation
error_test <- (test_data$price_norm - p_test)**2
rmse_test <- sqrt(mean(error_test))
