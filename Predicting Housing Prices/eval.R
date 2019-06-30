
splits = read.table("Project1_test_id.txt")
data = read.csv("Ames_data.csv")
source("mymain.R")
source("mymain_Lasso.R")
# test with taking out the outliers, not used in the final run
data = data[-c(182,1554,566,936),]

calculateRMSE = function (price, predictions) {
  pred = merge(predictions, price, by = "PID")
  rmse = sqrt(mean((
    log(pred$Sale_Price) - log(pred$True_Sale_Price)
  ) ^ 2))
  return(rmse)
}

rmse_results = matrix(0,nrow=10,ncol = 5)
colnames(rmse_results) = c("Model1: CV", "Model2: XGB", "Model3: CD ", "Time for Part I","Time for Part II")


for (i in 1:ncol(splits)) {
  
  data.test.indexes = which(data$PID %in% splits[,i])
  data.training = data[-data.test.indexes,]
  data.testing = data[data.test.indexes,-83]
  data.test.trueprice = data[data.test.indexes, c(1, 83)]
  names(data.test.trueprice)[2] = "True_Sale_Price"
  write.csv(data.training, file = paste("train.csv", sep = ""), row.names = FALSE)
  write.csv(data.testing, file = paste("test.csv", sep = ""), row.names = FALSE)
  
  time.start = proc.time()
  evaluateMain()
  time.end = proc.time()
  rmse_results[i,4]= (time.end - time.start)["elapsed"]
  print(paste("Model 1: CV ","iteration -- > ",i))
  rmse_results[i,1] = calculateRMSE(data.test.trueprice, read.csv("mysubmission1.txt"))
  print(paste("Model 2 XGB ","iteration ---> ",i))
  rmse_results[i,2] = calculateRMSE(data.test.trueprice, read.csv("mysubmission2.txt"))
  
  time.start = proc.time()
  evaluatemyLasso()
  time.end = proc.time()
  rmse_results[i,5]= (time.end - time.start)["elapsed"]
  print(paste("Model 3 CD ","iteration ---> ",i))
  rmse_results[i,3] = calculateRMSE(data.test.trueprice, read.csv("mysubmission3.txt"))
  
  print(rmse_results)
  
}

