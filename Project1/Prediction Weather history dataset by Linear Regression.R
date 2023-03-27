library(dplyr)
df <- read.csv("weatherHistory.csv", stringsAsFactors = FALSE)
View(df)

df <- df %>%
  select(Temperature..C., Apparent.Temperature..C., Humidity, Wind.Speed..km.h.)

head(df)

#check type each column
str(df)

#Check for missing values in df
is.na(df)

## DROP NA (missing values)
df_train <- na.omit(df)
nrow(df_train)

#Change the column names for convenience
colnames(df_train)[1:4] <- c("temp", "app_temp", "humidity", "wind_speed")
head(df_train)

#Split train 80%, test 20%
set.seed(33)
n <- nrow(df_train)
id <- sample(1:n, size=n*0.8)
train_data <- df_train[id, ]
test_data <- df_train[-id, ]

## Train model

model <- lm(app_temp ~ humidity + wind_speed + temp, data = train_data)

p_train <- predict(model)
error_train <- train_data$App_Temp - p_train
(rmse_train <- sqrt(mean( error_train**2)))

## Test model
p_test <- predict(model, newdata = test_data)
error_test <- test_data$app_temp - p_test
(rmse_test <- sqrt(mean( error_test**2)))
