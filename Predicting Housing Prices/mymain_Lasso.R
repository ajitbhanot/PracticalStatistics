# Initialize environment
evaluatemyLasso = function() {
  rm(list = ls())
  set.seed(0322)
  mypackages = c("MASS", "glmnet", "psych", "xgboost")   # required packages
  tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
  if (length(tmp) > 0)
    install.packages(tmp)
  lapply(mypackages, require, character.only = TRUE)
  #generic transform functions for model 1 and model 2
  transformData = function (data) {
    data$bath = data$Full_Bath + 0.5 * data$Half_Bath + data$Bsmt_Full_Bath +
      data$Bsmt_Half_Bath
    #data$totalarea = data$Gr_Liv_Area+data$Total_Bsmt_SF
    
    data = subset(
      data,
      select = -c(
        Condition_2,
        Garage_Yr_Blt,
        Latitude,
        Longitude,
        Pool_QC,
        Street,
        Utilities,
        Garage_Cars,
        Total_Bsmt_SF,
        Garage_Cond,
        Year_Remod_Add,
        Full_Bath,
        Half_Bath,
        Bsmt_Full_Bath,
        Bsmt_Half_Bath,
        Gr_Liv_Area
      )
    )
    
    
    data.PID_and_Sale_Price = subset(data, select = c(PID, Sale_Price))
    data.without_PID_and_Sale_Price = subset(data, select = -c(PID, Sale_Price))
    
    for (i in 1:ncol(data.without_PID_and_Sale_Price)) {
      if (as.character(class(data.without_PID_and_Sale_Price[, i]) == "integer")) {
        data.without_PID_and_Sale_Price[, i] = as.integer(winsor(data.without_PID_and_Sale_Price[, i], trim = 0.05))
      }
    }
    
    data = data.frame(data.PID_and_Sale_Price,
                      data.without_PID_and_Sale_Price)
    
    return(data)
  }
  
  convertToDoubleMatrix = function (df) {
    return_matrix = matrix(0, nrow = nrow(df), ncol = ncol(df))
    
    for (i in 1:ncol(df)) {
      return_matrix[, i] = as.numeric(df[, i])
    }
    
    return(return_matrix)
  }

  #one step lasso
  one_step_lasso = function(r, x, lam) {
    xx = sum(x ^ 2)
    xr = sum(r * x)
    b = (abs(xr) - lam / 2) / xx
    b = sign(xr) * ifelse(b > 0, b, 0)
    return(b)
  }
  
  # cd lasso function
  cdlasso = function(X, y, lam, n.iter)
  {
    p = ncol(X)
    n = nrow(X)
    
    b = rep(0, p)
    r = y
    
    for (step in 1:n.iter) {
      for (j in 1:p) {
        r = r + X[, j] * b[j]
        b[j] = one_step_lasso(r, X[, j], lam)
        r = r - X[, j] * b[j]
      }
    }
    
    b0 = mean(y) - sum(b * colMeans(X))
    
    return(c(b0, b))
  }
  
  
  
  data.train = read.csv("train.csv")
  data.train.pids = data.train$PID
  data.test = read.csv("test.csv")
  data.test.pids = data.test$PID
  data.test.with_null_sale_price = data.frame(data.test, Sale_Price = rep(NA, times = nrow(data.test)))
  data.all = rbind(data.train, data.test.with_null_sale_price)
  
  data.test.trueprice = read.csv("trueprice.csv")
  names(data.test.trueprice)[2] = "True_Sale_Price"

  ##### model 3 starts here
  
  ## data transformation for model 3
  transformed_data = transformData(data.all)
  
  transformed_training_data = transformed_data[transformed_data$PID %in% data.train.pids, ]
  transformed_train_features = model.matrix( ~ . - 1,
                                           data = subset(transformed_training_data, select = -c(PID, Sale_Price)))
  transformed_train_features = scale(transformed_train_features)
  transformed_train_features[is.nan(transformed_train_features)] = 0
  transformed_train_labels = log(transformed_training_data$Sale_Price)
  
  transformed_test_data = transformed_data[transformed_data$PID %in% data.test.pids, ]
  transformed_test_data_features = model.matrix( ~ . - 1,
                                          data = subset(transformed_test_data, select = -c(PID, Sale_Price)))
  transformed_test_data.PID = subset(transformed_test_data, select = c(PID))
  transformed_test_data_features = scale(transformed_test_data_features)
  transformed_test_data_features[is.nan(transformed_test_data_features)] = 0
  coefficients = cdlasso(
    transformed_train_features,
    transformed_train_labels,
    lam = 10,
    n.iter = 500
  )
  test.X.prepared_for_prediction = as.matrix(data.frame(intercept = 1, transformed_test_data_features))
  predictions = test.X.prepared_for_prediction %*% coefficients
  
  predictions.withPID = data.frame(PID = transformed_test_data.PID, Sale_Price = exp(predictions))
  
  write.csv(
    predictions.withPID,
    file = "mysubmission3.txt",
    row.names = FALSE,
    quote = FALSE
  )

}
evaluatemyLasso()