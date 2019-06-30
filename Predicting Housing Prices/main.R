# Initialize environment
evaluate = function() {
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
  # prediction Model 1 - Glmnet lasso
  predictWithGlmnet <-
    function (data.train.x,
              data.train.y,
              data.test.x) {
      model = cv.glmnet(x = convertToDoubleMatrix(data.train.x),
                        y = data.train.y,
                        alpha = 1)
      
      predictions = predict(model,
                            s = model$lambda.min,
                            newx = convertToDoubleMatrix(data.test.x))
      
      output.predictions = data.frame(data.test.x[, 1],
                                      ifelse(is.na(exp(predictions)), 0, exp(predictions)))
      colnames(output.predictions) = c("PID", "Sale_Price")
      
      return(output.predictions)
    }
  
  #prediction model 2 with Gboost
  
  predictWithXgboost = function (data.train.x,
                                 data.train.y,
                                 data.test.x) {
    model <- xgboost(
      data = convertToDoubleMatrix(data.train.x),
      label = data.train.y,
      nrounds = 1000,
      max.depth = 15,
      eta = 0.03,
      gamma = 0.1,
      objective = "reg:linear",
      verbose = 0
    )
    
    predictions = predict(model, convertToDoubleMatrix(data.test.x))
    output.predictions = data.frame(data.test.x[, 1],
                                    ifelse(is.na(exp(predictions)), 0, exp(predictions)))
    colnames(output.predictions) = c("PID", "Sale_Price")
    
    return(output.predictions)
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
  print(5)
  names(data.test.trueprice)[2] = "True_Sale_Price"

    ## Apply transformations
  data.all.transformed = transformData(data.all)
  
  data.train.transformed = data.all.transformed[data.all.transformed$PID %in% data.train.pids, ]
  data.train.transformed.x = scale(model.matrix( ~ . - 1,
                                                 data = subset(
                                                   data.train.transformed,
                                                   select = -c(PID, Sale_Price)
                                                 )))
  data.train.transformed.x[is.nan(data.train.transformed.x)] = 0
  data.train.transformed.x = cbind(PID = data.train.pids, data.train.transformed.x)
  data.train.transformed.y = log(data.train.transformed$Sale_Price)
  
  data.test.transformed = data.all.transformed[data.all.transformed$PID %in% data.test.pids, ]
  data.test.transformed.PID = subset(data.test.transformed, select = c(PID))
  
  data.test.transformed.x = scale(model.matrix( ~ . - 1,
                                                data = subset(
                                                  data.test.transformed,
                                                  select = -c(PID, Sale_Price)
                                                )))
  data.test.transformed.x[is.nan(data.test.transformed.x)] = 0
  data.test.transformed.x = cbind(PID = data.test.pids, data.test.transformed.x)
  
  model1.prediction = predictWithGlmnet(data.train.transformed.x,
                                        data.train.transformed.y,
                                        data.test.transformed.x)
  #
  # # Model 2
  model2.prediction = predictWithXgboost(data.train.transformed.x,
                                         data.train.transformed.y,
                                         data.test.transformed.x)
  
  ## simple model
  # model3.prediction = predictwithlm(data.train.transformed.x,
  #                                   data.train.transformed.y,
  #                                   data.test.transformed.x)
  #
  write.csv(
    model1.prediction,
    file = "mysubmission1.txt",
    row.names = FALSE,
    quote = FALSE
  )
  write.csv(
    model2.prediction,
    file = "mysubmission2.txt",
    row.names = FALSE,
    quote = FALSE
  )
  ##### model 3 starts here
  
  ## data transformation for model 3
  
  data.train.transformed = data.all.transformed[data.all.transformed$PID %in% data.train.pids, ]
  data.train.transformed.x = model.matrix( ~ . - 1,
                                           data = subset(data.train.transformed, select = -c(PID, Sale_Price)))
  data.train.transformed.x = scale(data.train.transformed.x)
  data.train.transformed.x[is.nan(data.train.transformed.x)] = 0
  data.train.transformed.y = log(data.train.transformed$Sale_Price)
  
  data.test.transformed = data.all.transformed[data.all.transformed$PID %in% data.test.pids, ]
  data.test.transformed.x = model.matrix( ~ . - 1,
                                          data = subset(data.test.transformed, select = -c(PID, Sale_Price)))
  data.test.transformed.PID = subset(data.test.transformed, select = c(PID))
  data.test.transformed.x = scale(data.test.transformed.x)
  data.test.transformed.x[is.nan(data.test.transformed.x)] = 0
  coefficients = cdlasso(
    data.train.transformed.x,
    data.train.transformed.y,
    lam = 10,
    n.iter = 100
  )
  test.X.prepared_for_prediction = as.matrix(data.frame(intercept = 1, data.test.transformed.x))
  predictions = test.X.prepared_for_prediction %*% coefficients
  # data.test.transformed.PID = subset(data.test.transformed, select = c(PID))
  
  predictions.withPID = data.frame(PID = data.test.transformed.PID, Sale_Price = exp(predictions))
  
  write.csv(
    predictions.withPID,
    file = "mysubmission3.txt",
    row.names = FALSE,
    quote = FALSE
  )
  
  
  calculateRMSE = function (price, predictions) {
    pred = merge(predictions, price, by = "PID")
    rmse = sqrt(mean((
      log(pred$Sale_Price) - log(pred$True_Sale_Price)
    ) ^ 2))
    return(rmse)
  }
  print(paste(
    calculateRMSE(data.test.trueprice, read.csv("mysubmission1.txt")),
    sep = ": "
  ))
  print(paste(
    "Model 2",
    calculateRMSE(data.test.trueprice, read.csv("mysubmission2.txt")),
    sep = ": "
  ))
  print(paste(
    "Model 3",
    calculateRMSE(data.test.trueprice, read.csv("mysubmission3.txt")),
    sep = ": "
  ))
}
evaluate()