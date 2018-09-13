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
        # input_data[[sprintf("V%d", i)]][which(input_data[[sprintf("V%d", i)]]==" ?")] <- NA
        input_data[[sprintf("V%d", i)]][which(input_data[[sprintf("V%d", i)]]==target)] <- j
      }
    }
    input_data[[sprintf("V%d", i)]] <- as.numeric(input_data[[sprintf("V%d", i)]])
    
    # normalizing input data
    if (i != 5){
      mean <- mean(input_data[[sprintf("V%d", i)]], na.rm=T)
      sd <- sd(input_data[[sprintf("V%d", i)]], na.rm=T)
      input_data[[sprintf("V%d", i)]] <- (input_data[[sprintf("V%d", i)]]-mean)/sd
    }
  }
  return (input_data)
}

train_data <- translate_data(train_data)
train_data$V5 <- as.character(train_data$V5)
train_data$V5[train_data$V5=="2"] <- "-1"
train_data$V5 <- as.factor(train_data$V5)
test_data <- translate_data(test_data)

ind <- createDataPartition(train_data$V5, times=1, p=0.9, list=F)
val_train <- train_data[ind,]
val_test <- train_data[-ind,]


a <- c(1,1,1,1)
b <- 1

Ns <- 50
step <- 300
lambda <- 10**0

x <- c()
y <- c()
countxy <- 1

for (i in 1:Ns){
  g_s <- 1/(0.01*i+50)
  for (j in 1:step){
    r <- val_train[round(runif(1)*nrow(val_train)),]
    if (as.integer(r$V5)*(sum(a*r[1:4], na.rm = T)+b) >= 1){
      a <- a - g_s*lambda*a
      b <- b
    } else {
      a <- a - g_s*(lambda*a - as.integer(r$V5)*r[1:4])
      b <- b + g_s * as.integer(r$V5)
    }
    if (j %% 30 == 0){
        val_sample = val_train[sample(nrow(val_train), 50), ]
        correct <- 0
        for (k in 1:nrow(val_sample)){
          if (sum(a*val_sample[k,][1:4], na.rm = T)+b >= 0 && val_sample[k,]$V5 == 1)
            correct <- correct + 1
          else if (sum(a*val_sample[k,][1:4], na.rm = T)+b < 0 && val_sample[k,]$V5 == -1)
            correct <- correct + 1
        }
        x[countxy] <- (i-1)*step+j
        y[countxy] <- correct/50
        countxy <- countxy + 1
        plot(x,y)
    }
  }
  pred <- c()
  for (k in 1:nrow(val_test)){
    if (sum(a*val_test[k,][1:4], na.rm = T)+b > 0)
      pred[k] <- 1
    else
      pred[k] <- -1
  }
  table <- table(actual=as.factor(val_test$V5), predict=as.factor(pred))
  ratio <- sum(diag(table))/sum(table)
  print(table)
  print(ratio)
}

