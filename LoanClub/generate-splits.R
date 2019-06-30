#!/usr/bin/env Rscript

data.all = read.csv("loan_stat542.csv")
test_ids = read.csv("Project3_test_id.csv")

for (i in 1:ncol(test_ids)) {
  data.test.ids = test_ids[,i]

  data.test = data.all[data.all[,1] %in% data.test.ids,]
  write.csv(data.frame(id = data.test$id,
                       y = ifelse(data.test$loan_status %in% c("Default", "Charged Off"), 1, 0)),
            paste0("label-", i, ".csv"), 
            row.names = FALSE, 
            quote = FALSE)
  
  data.test = subset(data.test, select = -loan_status)
  write.csv(data.test, paste0("test-", i, ".csv"))

  data.train = data.all[!(data.all[,1] %in% data.test.ids), ]
  write.csv(data.train, paste0("train-", i, ".csv"))
}
