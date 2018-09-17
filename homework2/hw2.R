library(caret)
# Take in data.
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework2")
train_data = read.table("train.csv", sep=",")
test_data = read.table("test.csv", sep=",")

# Function that normalize data.
translate_data <- function(input_data){
  for (i in c(1,3,5,11:13)){
    input_data[[sprintf("V%d", i)]] <- scale(input_data[[sprintf("V%d", i)]])
  }
  return (input_data)
}

train_data <- translate_data(train_data)
train_data$V15 <- as.character(train_data$V15)
train_data$V15[train_data$V15==" <=50K"] <- "-1"
train_data$V15[train_data$V15==" >50K"] <- "1"
train_data$V15 <- as.factor(train_data$V15)
test_data <- translate_data(test_data)


# Initialize some paramaters
a <- c(1,1,1,1,1,1)
b <- 1

Ns <- 50
step <- 300
lambda_list <- c(10**0, 10**-1, 10**-2, 10**-3)

x <- c()
y <- c()
mag_a <- c()
countxy <- 1
accuracy <- 0
final_a <- c(0,0,0,0,0,0)
final_b <- 0


# Randomly split training data to 2 parts, 90% training part adn 10% validation part.
ind <- createDataPartition(train_data$V15, times=1, p=0.9, list=F)
val_train <- train_data[ind,]
val_test <- train_data[-ind,]

# start loop: this is a three layer loop, the first layer is to choose which regulatization constant has the highest
# accuracy rate, the second layer is season/epoch and the last layer is step.
for (m in 1:length(lambda_list)){
  lambda <- lambda_list[m]
  for (i in 1:Ns){
    # Setting the learning rate.
    g_s <- 1/(0.01*i+50)
    for (j in 1:step){
      index <- sample(1:nrow(val_train), 1)
      r <- val_train[index,]
      if (as.numeric(levels(r$V15)[r$V15])*(sum(a*r[c(1,3,5,11:13)])+b) >= 1){
        a <- a - g_s*lambda*a
        b <- b
      } else {
        a <- a - g_s*(lambda*a - as.numeric(levels(r$V15)[r$V15])*r[c(1,3,5,11:13)])
        b <- b + g_s * as.numeric(levels(r$V15)[r$V15])
      }
    }
    
    # Test after the step-loop in each season, calculate the accuracy and magnitude of vector a. 
    # pred_per_epoch <- c()
    # for (k in 1:nrow(val_test)){
    #   if (sum(a*val_test[k,][c(1,3,5,11:13)])+b >= 0)
    #     pred_per_epoch[k] <- 1
    #   else
    #     pred_per_epoch[k] <- -1
    # }
    # pred_epoch_table <- table(actual=val_test$V15, predict=pred_per_epoch)
    # acc <- sum(diag(pred_epoch_table))/sum(pred_epoch_table)
    # x[countxy] <- i
    # y[countxy] <- acc
    # mag_a[countxy] <- sum(a*a)
    # countxy <- countxy + 1
  } 
  
  # Calculate accuracy in different regularization constant and
  # choose the regularization constant with the highest accuracy. 
  pred <- c()
  for (k in 1:nrow(val_test)){
    if (sum(a*val_test[k,][c(1,3,5,11:13)])+b >= 0)
      pred[k] <- 1
    else
      pred[k] <- -1
  }
  pred_table <- table(actual=val_test$V15, predict=pred)
  if (sum(diag(pred_table))/sum(pred_table) > accuracy){
    accuracy <- sum(diag(pred_table))/sum(pred_table)
    final_a <- a
    final_b <- b
    final_lambda <- lambda
  }
}

# # Make data to be ploted
# matx <- matrix(x,ncol=length(lambda_list))
# maty <- matrix(y,ncol=length(lambda_list))
# matMag_a <- matrix(mag_a,ncol=length(lambda_list))
# 
# #Plot the data
# matplot(matx, maty, type = c("b"),pch=1,col = 1:4)
# matplot(matMag_a, maty, type = c("b"),pch=1,col = 1:4) 

# Predict data in test.csv.
pred_test = c()
example = c()
for (i in 1:nrow(test_data)){
  example[i] <- sprintf("'%d'", i-1)
  if (sum(final_a*test_data[i,][c(1,3,5,11:13)])+final_b > 0)
    pred_test[i] <- ">50K"
  else
    pred_test[i] <- "<=50K"
}
csv.data <- data.frame("Example"=example,
                       "Label"=pred_test)
write.csv(csv.data, file="res.csv", row.names = F)
