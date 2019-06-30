# Initialize environment
evaluateMain = function() {
  rm(list = ls())
  set.seed(0322)
  mypackages = c("MASS","glmnet", "psych", "xgboost")   # required packages
  tmp = setdiff(mypackages, rownames(installed.packages()))  # packages need to be installed
  if (length(tmp) > 0)
    install.packages(tmp)
  lapply(mypackages, require, character.only = TRUE)
  
  #generic transform functions for model 1 and model 2
  transformData = function (data) {
    data$bath = data$Full_Bath + 0.5 * data$Half_Bath + data$Bsmt_Full_Bath +
      data$Bsmt_Half_Bath
    #data$Remod = ifelse(data$Year_Built==data$Year_Remod_Add, 0, 1) #0=No Remodeling, 1=Remodeling
    #data$IsNew = ifelse(data$YrS_old==data$Year_Built, 1, 0)
    
    # data = subset(
    #   data,
    #   select = -c(
    #     Condition_2,
    #     Garage_Yr_Blt,
    #     Latitude,
    #     Longitude,
    #     Pool_QC,
    #     Street,
    #     Utilities
    #   )
    # )
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
     #   Total_Bsmt_SF,
    #    Garage_Cond,
    #    Year_Remod_Add,
        Full_Bath,
        Half_Bath,
        Bsmt_Full_Bath,
        Bsmt_Half_Bath
      )
    )

    
    # levels(data$Overall_Cond) = list(
    #   One = c("Very_Poor", "Poor", "Fair"),
    #   Two = c("Below_Average","Average", "Above_Average"),
    #   Three = c("Good", "Very_Good","Excellent"))
    # 
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
  
  MatrixNumericConversion = function (df) {
    return_matrix = matrix(0, nrow = nrow(df), ncol = ncol(df))
    
    for (i in 1:ncol(df)) {
      return_matrix[, i] = as.numeric(df[, i])
    }
    
    return(return_matrix)
  }
  # prediction Model 1 - Glmnet lasso
  LassoModel <-
    function (features_train_data,
              labels_train_data,
              features_test_data) {
      set.seed(0322)
      
      model = cv.glmnet(x = MatrixNumericConversion(features_train_data),
                        y = labels_train_data,
                        alpha = 1)
      
      predictions = predict(model,
                            s = model$lambda.min,
                            newx = MatrixNumericConversion(features_test_data))
      
      output.predictions = data.frame(features_test_data[, 1],
                                      ifelse(is.na(exp(predictions)), 0, exp(predictions)))
      colnames(output.predictions) = c("PID", "Sale_Price")
      
      return(output.predictions)
    }
  
  #prediction model 2 with Gboost
  
  ExtremeGBoostModel = function (features_train_data,
                                 labels_train_data,
                                 features_test_data) {
      set.seed(0322)
        model <- xgboost(
      data = MatrixNumericConversion(features_train_data),
      label = labels_train_data,
      nrounds = 1000,
      max.depth = 10,
      eta = 0.3,
      gamma = 0.1,
      objective = "reg:linear",
      verbose = 0
    )
    
    predictions = predict(model, MatrixNumericConversion(features_test_data))
    output.predictions = data.frame(features_test_data[, 1],
                                    ifelse(is.na(exp(predictions)), 0, exp(predictions)))
    colnames(output.predictions) = c("PID", "Sale_Price")
    
    return(output.predictions)
  }
  
  data.train = read.csv("train.csv")
  data.train.pids = data.train$PID
  data.test = read.csv("test.csv")
  data.test.pids = data.test$PID
  data.test.with_null_sale_price = data.frame(data.test, Sale_Price = rep(NA, times = nrow(data.test)))
  data.all = rbind(data.train, data.test.with_null_sale_price)
  
 

    ## Apply transformations
  transformed_data = transformData(data.all)
  
  transformed_training_data = transformed_data[transformed_data$PID %in% data.train.pids, ]
  scaled_train_features = scale(model.matrix( ~ . - 1,
                                                 data = subset(
                                                   transformed_training_data,
                                                   select = -c(PID, Sale_Price)
                                                 )))
  scaled_train_features[is.nan(scaled_train_features)] = 0
  scaled_train_features = cbind(PID = data.train.pids, scaled_train_features)
  scaled_train_labels = log(transformed_training_data$Sale_Price)
  
  transformed_test_data = transformed_data[transformed_data$PID %in% data.test.pids, ]
  transformed_test_data.PID = subset(transformed_test_data, select = c(PID))
  
  scaled_test_labels = scale(model.matrix( ~ . - 1,
                                                data = subset(
                                                  transformed_test_data,
                                                  select = -c(PID, Sale_Price)
                                                )))
  scaled_test_labels[is.nan(scaled_test_labels)] = 0
  scaled_test_labels = cbind(PID = data.test.pids, scaled_test_labels)
  
 
  
    model1.prediction = LassoModel(scaled_train_features,
                                          scaled_train_labels,
                                          scaled_test_labels)

    write.csv(
    model1.prediction,
    file = "mysubmission1.txt",
    row.names = FALSE,
    quote = FALSE
  )
    
  model2.prediction = ExtremeGBoostModel(scaled_train_features,
                                         scaled_train_labels,
                                         scaled_test_labels)
  
    write.csv(
    model2.prediction,
    file = "mysubmission2.txt",
    row.names = FALSE,
    quote = FALSE
  )

  
}
evaluateMain()

