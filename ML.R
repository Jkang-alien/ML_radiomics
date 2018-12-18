library(dplyr)
library(recipes)
library(caret)
library(purrr)
library(ggplot2)
library(pROC)
library(yardstick)

rec_basic <- recipe(class ~ ., training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())

ctrl <- trainControl(
  method = 'cv',
  classProbs = TRUE)

train(rec_basic,
      data = training,
      method = 'rpart',
      metric = "ROC",
      trControl = ctrl)

colnames(training)
