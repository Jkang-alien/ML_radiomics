library(dplyr)
library(recipes)
library(caret)
library(purrr)
library(ggplot2)
library(pROC)
library(yardstick)

rec_basic <- recipe(class ~., training) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_zv(all_predictors())

ctrl <- trainControl(
  method = 'cv',
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  savePredictions = 'final',
  sampling = 'down'
)

smoothing_grid <- expand.grid(usekernel = TRUE, fL = 0, adjust = seq(0.5, 3.5, by = 0.5))

train(rec_basic,
      data = training,
      method = 'lda',
      metric = "ROC",
      #tuneGrid = smoothing_grid,
      trControl = ctrl)
1
