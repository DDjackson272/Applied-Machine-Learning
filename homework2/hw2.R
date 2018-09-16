library(caret)
library(e1071)
library(pracma)
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework2")
train_data = read.table("train.csv", sep=",")
test_data = read.table("test.csv", sep=",")

# Function that translate string in input dataframe to digits
translate_data <- function(input_data){
  for (i in c(1,3,5,11:13)){
    # normalizing input data
    mean <- mean(input_data[[sprintf("V%d", i)]], na.rm=T)
    sd <- sd(input_data[[sprintf("V%d", i)]], na.rm=T)
    input_data[[sprintf("V%d", i)]] <- (input_data[[sprintf("V%d", i)]]-mean)/sd
  }
  return (input_data)
}

train_data <- translate_data(train_data)
train_data$V15 <- as.character(train_data$V15)
train_data$V15[train_data$V15==" <=50K"] <- "1"
train_data$V15[train_data$V15==" >50K"] <- "-1"
train_data$V15 <- as.factor(train_data$V15)
test_data <- translate_data(test_data)

ind <- createDataPartition(train_data$V15, times=1, p=0.9, list=F)
val_train <- train_data[ind,]
val_test <- train_data[-ind,]


a <- c(0.230287, 0.2069413, -0.02692105, 0.7772013, 0.6647214, 0.2539046)
b <- 2.039684

Ns <- 50
step <- 300
lambda <- 10**-7

x <- c()
y <- c()
countxy <- 1

for (i in 1:Ns){
  g_s <- 1/(0.01*i+50)
  for (j in 1:step){
    r <- val_train[round(runif(1)*nrow(val_train)),]
    if (as.integer(r$V15)*(sum(a*r[c(1,3,5,11:13)], na.rm = T)+b) >= 1){
      a <- a - g_s*lambda*a
      b <- b
    } else {
      a <- a - g_s*(lambda*a - as.integer(r$V15)*r[c(1,3,5,11:13)])
      b <- b + g_s * as.integer(r$V15)
    }
    # if (j %% 30 == 0){
    #     val_sample = val_train[sample(nrow(val_train), 50), ]
    #     correct <- 0
    #     for (k in 1:nrow(val_sample)){
    #       if (sum(a*val_sample[k,][c(1,3,5,11:13)], na.rm = T)+b >= 0 && val_sample[k,]$V15 == 1)
    #         correct <- correct + 1
    #       else if (sum(a*val_sample[k,][c(1,3,5,11:13)], na.rm = T)+b < 0 && val_sample[k,]$V15 == -1)
    #         correct <- correct + 1
    #     }
    #     x[countxy] <- (i-1)*step+j
    #     y[countxy] <- correct/50
    #     countxy <- countxy + 1
    #     plot(x,y)
    # }
  }
  pred <- c()
  for (k in 1:nrow(val_test)){
    if (sum(a*val_test[k,][c(1,3,5,11:13)])+b > 0)
      pred[k] <- 1
    else
      pred[k] <- -1
  }
  table <- table(actual=as.factor(val_test$V15), predict=as.factor(pred))
  ratio <- sum(diag(table))/sum(table)
  print(table)
  print(ratio)
}

