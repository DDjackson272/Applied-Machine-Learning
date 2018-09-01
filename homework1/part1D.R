library(caret)
library(klaR)
data <- read.table("pima-indians-diabetes.csv",sep = ',')
colnames(data) <- c("feat_a","feat_b","feat_c","feat_d","feat_e","feat_f","feat_g","feat_h","res")

rows_to_delete <- vector()
count <- 1
for (i in 1:nrow(data)){
  if (data[i,]$feat_a == 0 || data[i,]$feat_d == 0 || data[i,]$feat_f == 0 || data[i,]$feat_h == 0){
    rows_to_delete[count] <- i
    count <- count + 1
  }
}

data <- data[-rows_to_delete,]

start <- 1
end <- round(nrow(data)/10)
total_acc <- 0
for (i in 1:10){
  eva_data = data[start:end,]
  ind <- createDataPartition(eva_data$res, times=1, p=0.8, list=F)
  train_data = eva_data[ind,]
  test_data = eva_data[-ind,]
  model <- svmlight(res~., data=train_data)
  pred <- predict(model, newdata=test_data)
  svm_table <- table(actual=test_data$res, predict=pred$class)
  ratio <- sum(diag(svm_table))/sum(svm_table)
  
  print(svm_table)
  print(ratio)
  
  total_acc <- total_acc + ratio
  
  start <- start + round(nrow(data)/10)
  end <- end + round(nrow(data)/10)
  if(end > nrow(data))
    end <- nrow(data)
}

print(total_acc/10)

