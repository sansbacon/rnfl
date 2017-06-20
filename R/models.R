# models.R
# fantasy football machine learning models

model.dst.defaultfeatures <- function() {
  # Default features for dst model
  # Args:
  #   None
  #
  # Returns:
  #   character vector of feature names

  c('opp_impltot', 'fpts_rm', 'fpts_ema',
    'sacks_rm', 'sacks_ema', 'offdvoa',
    'defdvoa', 'oppoffdvoa', 'oppdefdvoa')
}

model.dst.rfcl <- function(ds, features) {
  # Random forest classification model
  # DST scored >= n points
  #
  # Args:
  #   ds: dataframe
  #   features: character vector of feature names
  #
  # Returns:
  #   list - m is model, cm is confusion matrix

  ds.xform <- ds %>%
    .[complete.cases(.),] %>%
    select_(.dots=c(features, 'y'))
  trainIndex <- createDataPartition(ds.xform$y, p=.6, list=FALSE)
  training <- ds.xform[trainIndex,]
  testing <- ds.xform[-trainIndex,]
  rf <- ranger(y ~ ., data=training, num.trees=1000, importance='impurity')
  pred <- predict(rf, testing)
  confmax <- confusionMatrix(pred$predictions, testing$y, positive='TRUE')
  list(m=rf, cm=confmax, ntr=table(training$y), nte=table(testing$y))
}
