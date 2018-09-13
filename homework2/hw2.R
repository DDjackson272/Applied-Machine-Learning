library(caret)
library(e1071)
setwd("C:\\Users\\acdjackson\\Desktop\\CS498\\homework2")
train_data = read.table("train.csv", sep=",")
test_data = read.table("test.csv", sep=",")

# Function that translate string in input dataframe to digits
translate_data <- function(input_data){
  for (i in 1:length(input_data)){
    lev <- levels(input_data[[sprintf("V%d", i)]])
    if (length(lev) != 0){
      for (j in 1:length(lev)){
        target <- as.character(lev[j])
        input_data[[sprintf("V%d", i)]] <- as.character(input_data[[sprintf("V%d", i)]])
        input_data[[sprintf("V%d", i)]][which(input_data[[sprintf("V%d", i)]]==" ?")] <- NA
        input_data[[sprintf("V%d", i)]][which(input_data[[sprintf("V%d", i)]]==target)] <- j
      }
    }
  }
  return (input_data)
}

train_data <- translate_data(train_data)
train_data$V15 <- as.factor(train_data$V15)
test_data <- translate_data(test_data)
ind <- createDataPartition(train_data$V15, times=1, p=0.9, list=F)
val_train <- train_data[ind,]
val_test <- train_data[-ind,]

model <- naiveBayes(V15~., data=val_train, na.rm=T)
pred <- predict(model, newdata=val_test)
nb_table <- table(actual=val_test$V15, predict=pred)
ratio <- sum(diag(nb_table))/sum(nb_table)

print(nb_table)
print(ratio)
