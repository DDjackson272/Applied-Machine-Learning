library(caret)
library(e1071)
library(pracma)
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
    input_data[[sprintf("V%d", i)]] <- as.numeric(input_data[[sprintf("V%d", i)]])
    
    # normalizing input data
    if (i != 15){
      mean <- mean(input_data[[sprintf("V%d", i)]], na.rm=T)
      sd <- sd(input_data[[sprintf("V%d", i)]], na.rm=T)
      input_data[[sprintf("V%d", i)]] <- (input_data[[sprintf("V%d", i)]]-mean)/sd
    }
  }
  return (input_data)
}

train_data <- translate_data(train_data)
train_data$V15 <- as.character(train_data$V15)
train_data$V15[train_data$V15=="2"] <- "-1"
train_data$V15 <- as.factor(train_data$V15)
test_data <- translate_data(test_data)

ind <- createDataPartition(train_data$V15, times=1, p=0.9, list=F)
val_train <- train_data[ind,]
val_test <- train_data[-ind,]


a <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
b <- 1

Ns <- 5000
step <- 50
lambda <- 10**-7

for (i in 1:Ns){
  g_s <- 1/(i+100)
  for (j in 1:step){
    r <- val_train[round(runif(1)*nrow(val_train)),]
    if (as.integer(r$V15)*(sum(a*r[1:14], na.rm = T)+b) >= 1){
      a <- a - g_s*lambda*a
      b <- b
    } else {
      a <- a - g_s*(lambda*a - as.integer(r$V15)*r[1:14])
      b <- b + g_s * as.integer(r$V15)
    }
  }
  pred <- c()
  for (k in 1:nrow(val_test)){
    if (sum(a*val_test[k,][1:14], na.rm = T)+b > 0)
      pred[k] <- 1
    else
      pred[k] <- -1
  }
  print(sum(a*a, na.rm = T))
  table <- table(actual=as.factor(val_test$V15), predict=as.factor(pred))
  ratio <- sum(diag(table))/sum(table)
  print(table)
  print(ratio)
}
# model <- naiveBayes(V15~., data=val_train, na.rm=T)
# pred <- predict(model, newdata=val_test)
# nb_table <- table(actual=val_test$V15, predict=pred)
# ratio <- sum(diag(nb_table))/sum(nb_table)
# 
# print(nb_table)
# print(ratio)
