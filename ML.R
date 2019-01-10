library(dplyr)
library(recipes)
library(caret)
library(purrr)
library(ggplot2)
library(pROC)
library(yardstick)

rec_basic <- recipe(class ~., training) %>%
  step_knnimpute(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
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

smoothing_grid <- expand.grid(usekernel = TRUE,
                              fL = 0,
                              adjust = seq(0.5, 3.5, by = 0.5))

###############################################################
#################### linear discriminant analysis #############

lda_step <- train(rec_basic,
      data = training,
      method = 'stepLDA',
      metric = "ROC",
      #tuneGrid = smoothing_grid,
      trControl = ctrl)

###############################################################
################# Logistic regression #########################

glm_step <- train(rec_basic,
                  data = training,
                  method = 'glmStepAIC',
                  metric = "ROC",
                  #tuneGrid = smoothing_grid,
                  trControl = ctrl)

###############################################################
################ Support Vector Machine #######################

SVMgrid <- expand.grid(sigma = c(0.05,0.0456,0.0577), C = c(1.5,1.596,1.65,1.89,1.95,2,2.2,2.44))

SVM_ctrl <- trainControl(method = "repeatedcv", 
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary,
                         savePredictions = 'final',
                         repeats=5,
                         verbose = FALSE)
SVM_mod <- train(rec_basic,
                  data = training,
                  method="svmRadial",
                  metric = "ROC",
                  tuneGrid = SVMgrid,
                  trControl = SVM_ctrl)

###############################################################
################ Neural network ###############################

numFolds <- trainControl(method = 'cv',
                         number = 10,
                         classProbs = TRUE,
                         verboseIter = TRUE,
                         summaryFunction = twoClassSummary,
                         preProcOptions = list(thresh = 0.75, ICAcomp = 3, k = 5))

nn_mod <- train(rec_basic,
                data = training,
                method = 'nnet',
                trControl = numFolds,
                MaxNWts = 2000,
                tuneGrid=expand.grid(size=c(10), decay=c(0.1)))
nn_mod

test_res <- test %>%
  dplyr::select(class) %>%
  mutate(
    prob = predict(nn_mod, test, type = "prob")[, "Type1"],
    pred = predict(nn_mod, test)
  )
roc_curve <- roc(test_res$class, test_res$prob, levels = c("Type2", "Type1"))
roc_curve

plot(
  roc_curve,
  print.thres = .5
)


plot_roc <- function(x, ...) {
  roc_obj <- roc(
    response = x[["obs"]],
    predictor = x[["Type1"]],
    levels = rev(levels(x$obs))
  )
  plot(roc_obj, ...)
}
plot_roc(SVM_mod$pred)
plot_roc(nn_mod$pred)
SVM_mod$pred

plot(SVM)

SVM_mod$pred[["obs"]]
SVM_mod$pred[["Type1"]]

nn_mod$pred
