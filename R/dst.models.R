# dst.models.R
# various models to evaluate dst performance


dst.model.list.cl <- function(t, n=7) {
  # Uses caretList to train n classification models
  #
  # Args:
  #   t: dataframe of training data
  #   n: number of models to train
  #
  # Returns:
  #   list of caret::train objects

  caretList(
    y ~ ., data=t,
    preProcess=c("center", "scale"),
    trControl=trainControl(
      method="boot", number=10,
      savePredictions="final", classProbs=TRUE,
      index=createResample(training$y, 25),
      summaryFunction=twoClassSummary
    ),
    metric='ROC',
    methodList=c("glm", "avNNet", "knn", "LogitBoost",
                 "ranger", "C5.0", "svmRadial")
  )
}

dst.model.ensemble.cl <- function(ml) {
  # Creates ensemble classification model
  #
  # Args:
  #   ml: list of caret::train objects
  #
  # Returns:
  #   caretEnsemble object

  caretEnsemble (
    ml,
    metric="ROC",
    trControl=trainControl(
      number=2,
      summaryFunction=twoClassSummary,
      classProbs=TRUE
     )
  )
}

dst.model.preds.cl <- function(ml, t){
  # Creates dataframe of predictions from ensemble classification model
  #
  # Args:
  #   ml: list of caret::train objects
  #    t: dataframe of testing data
  #
  # Returns:
  #   dataframe of predictions + label from testing set

  model_preds <- lapply(ml, predict, newdata=t, type="prob")
  model_preds <- lapply(model_preds, function(x) x[,"y"])
  data.frame(model_preds)
}

dst.model.stack.cl <- function(ml){
  # Creates stacked model from ensemble classification model
  #
  # Args:
  #   ml: list of caret::train objects
  #
  # Returns:
  #   caretStack

  caretStack(
    ml, method="glm", metric="ROC",
    trControl=trainControl(
      method="boot", number=10,
      savePredictions="final", classProbs=TRUE,
      summaryFunction=twoClassSummary
    )
  )
}

dst.model.eval.cl <- function(ml, t, metric="avg"){
  # Evaluates predictions from ensemble classification model
  #
  # Args:
  #   ml: list of caret::train objects
  #   t: dataframe of testing data
  #   metric: string, either "avg" or "vote"
  #
  # Returns:
  #   colAUC function call

  model_preds <- lapply(model_list, predict, newdata=t, type="prob")
  model_preds <- lapply(model_preds, function(x) x[,"y"])
  model_preds <- data.frame(model_preds)

  if (metric == 'avg') {
    model_preds2 <- model_preds
    model_preds2$ensemble <- predict(glm_ensemble, newdata=testing, type="prob")
    CF <- coef(glm_ensemble$ens_model$finalModel)[-1]
    colAUC(model_preds2, testing$y)
  } else {
    model_preds3 <- as.data.frame(apply(model_preds2, 2, function(x) ifelse(x > .5, 1, 0)))
    model_preds3$rsum <- rowSums(model_preds3)
    model_preds3$pred <- factor(ifelse(model_preds3$rsum > 4, 'y', 'n'))
    confusionMatrix(model_preds3$pred, testing$y)
  }
}
