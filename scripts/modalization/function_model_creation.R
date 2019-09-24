# -------------------------------------------------------------------------
# GOAL: function to create any kind of model
# -------------------------------------------------------------------------

modalization <- function(data, p_partition, cv_repeats, cv_number,  
                         x, y, model_name, set_seed)  {
  # libraries
  if (require(pacman) == FALSE) {
    install.packages("pacman")
  }
  pacman::p_load(caret, doParallel)
  # set seed 
  set.seed(set_seed)
  # create data partition
  train_id <- createDataPartition(
    y = data[[1]], 
    p = p_partition, 
    list = F
  )
  train <- data[train_id,]
  test <- data[-train_id,]
  # create a cross validation
  ctrl <- trainControl(method = "repeatedcv",
                       repeats = cv_repeats, 
                       number = cv_number)
  # open the cluster to increase preprocess power
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cores = cl)
  # create the model
  system.time({
    mod <- caret::train(
      as.formula(paste(x, "~ .")),
      data = train[y],
      trControl = ctrl,
      method = model_name 
      # preProcess = c("center","scale")
    )
  })
  stopCluster(cl)
  
  # train results and metrics
  train$train_results <- predict(object = mod, newdata = train)
  train_metrics <- postResample(pred = train$train_results, obs = train[[x]])
  # test results and metrics
  test$test_results <- predict(object = mod, newdata = test)
  test_metrics <- postResample(pred = test$test_results, obs = test[[x]])
  
  # print the results
  print("TRAIN metrics:")
  print(train_metrics)
  print("TEST metrics:")
  print(test_metrics)
  
  # return relevant values
  return(
    list(
      model = mod,
      train_data = train, train_metrics = train_metrics,
      test_data = test, test_metrics = test_metrics
    )
  )
}