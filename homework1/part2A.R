library(e1071)
library(sparklyr)
library(dplyr)
Sys.setenv(
  SPARK_HOME="C:\\spark\\spark-2.3.1-bin-hadoop2.7"
)
sc <- spark_connect(master = "local")

# Take in data
image_train_data <- read.table("train.csv", sep = ',')
# image_test_data <- read.table("val.csv", sep=",")

# Rename column of data frame
colnames(image_train_data)[1] <- "Number"
# colnames(image_test_data)[1] <- "Number"
colnames(image_train_data)[2:785] <- sprintf("pixel%d", 1:784)
# colnames(image_test_data)[2:785] <- sprintf("pixel%d", 1:784)
image_train_data$Number <- as.factor(image_train_data$Number)
# image_test_data$Number <- as.factor(image_test_data$Number)

# Set a threshold (127)
image_train_data[image_train_data <= 127] <- 0
image_train_data[image_train_data >= 128] <- 1 
# image_test_data[image_test_data <= 127] <- 0
image_test_data[image_test_data >= 128] <- 1

# Resizing function that resize and strech the image
resizingFunc <- function(inputdata){
  print("Start truncating vector!")
  # Resizing from 28*28 to 20*20
  col_to_delete <- vector()
  count <- 1
  for (i in 2:length(inputdata[1,])){
    row <- floor((i-2)/28)
    col <- (i-2) %% 28
    if ((row <= 3 || row >= 24) || (col <= 3 || col >= 24)){
      col_to_delete[count] <- i
      count <- count + 1
    }
  }
  boundingData <- inputdata[-col_to_delete]
  colnames(boundingData) <- c(0:400)
  colnames(boundingData)[1] <- "Number"
  print("Start stretching vector!")
  # Stretch the image
  max_col <- 1
  min_col <- 20
  max_row <- 1
  min_row <- 20
  for (k in 1:nrow(boundingData)){
    print(k)
    image_1 <- boundingData[k,][2:401]
    max_col <- 1
    min_col <- 20
    max_row <- 1
    min_row <- 20
    for (i in 1:20){
      for (j in 1:20){
        if(image_1[[(i-1)*20+j]] == 1){
          if (i < min_row)
            min_row <- i
          if (i > max_row)
            max_row <- i
          if (j < min_col)
            min_col <- j
          if (j > max_col)
            max_col <- j 
        }
      }
    }
    row_a <- 20/(max_row-min_row)
    row_b <- 20*min_row/(min_row-max_row)
    col_a <- 20/(max_col-min_col)
    col_b <- 20*min_col/(min_col-max_col)
    for (i in 1:20){
      for (j in 1:20){
        if(image_1[[(i-1)*20+j]] == 1){
          end_y <- round(col_a*j+col_b)
          if (end_y < 1 || end_y > 20)
            end_y <- j
          image_1[[(i-1)*20+end_y]] <- 1
          end_x <- round(row_a*i+row_b)
          if (end_x < 1 || end_x > 20)
            end_x <- i
          image_1[[(end_x-1)*20+j]] <- 1
        }
      }
    }
    boundingData[k,][2:401] <- image_1
  }
  return(boundingData)
}


# Gaussian & untouched
image_res_data <- read.table("test.csv", sep=",")
colnames(image_res_data) <- sprintf("pixel%d", 1:784)
image_res_data[image_res_data <= 127] <- 0
image_res_data[image_res_data >= 128] <- 1
image_model <- naiveBayes(Number~., data=image_train_data)
image_pred <- predict(image_model, newdata=image_res_data)
# image_table <- table(actual=image_test_data$Number, predict=image_pred)
# ratio <- sum(diag(image_table))/sum(image_table)
# 
# print(image_table)
# print(ratio)

# Gaussian & streched bounding (28*28 -> 20*20)

# resizing bounding first
image_train_data <- read.table("train_stretched_noheader.csv", sep=",")
colnames(image_train_data)[1] <- "Number"
colnames(image_train_data)[2:401] <- sprintf("pixel%d", 1:400)
image_train_data$Number <- as.factor(image_train_data$Number)
image_res_data <- read.table("test_stretched2.csv", sep=",")
colnames(image_res_data) <- sprintf("pixel%d", 1:400)

image_model <- naiveBayes(Number~., data=image_train_data)
image_pred <- predict(image_model, newdata=image_res_data)


# Bernoulli & untouched
# 
image_res_data <- read.table("test.csv", sep=",")
colnames(image_res_data) <- sprintf("pixel%d", 1:784)
image_res_data[image_res_data <= 127] <- 0
image_res_data[image_res_data >= 128] <- 1

image_train_tbl <- copy_to(sc, image_train_data[1:30000,])
image_res_tbl <- copy_to(sc, image_res_data)
image_ber_model <- image_train_tbl %>%
  ml_naive_bayes(Number~., model_type="bernoulli")
image_ber_pred <- predict(image_ber_model, newdata=image_res_tbl)
# image_ber_table <- table(actual=image_test_data[,1], predict=image_ber_pred)
# ratio <- sum(diag(image_ber_table))/sum(image_ber_table)
# 
# print(image_ber_table)
# print(ratio)

# Bernoulli & streched bounding (28*28 -> 20*20)

image_train_data <- read.table("train_stretched_noheader.csv", sep=",")
colnames(image_train_data)[1] <- "Number"
colnames(image_train_data)[2:401] <- sprintf("pixel%d", 1:400)
image_train_data$Number <- as.factor(image_train_data$Number)
image_res_data <- read.table("test_stretched2.csv", sep=",")
colnames(image_res_data) <- sprintf("pixel%d", 1:400)

image_train_tbl <- copy_to(sc, image_train_data[1:30000,], overwrite = TRUE)
image_res_tbl <- copy_to(sc, image_res_data, overwrite = TRUE)
image_ber_model <- image_train_tbl %>%
  ml_naive_bayes(Number~., model_type="bernoulli")
image_ber_pred <- predict(image_ber_model, newdata=image_res_tbl)
# image_ber_table <- table(actual=image_test_data[,1], predict=image_ber_pred)
# ratio <- sum(diag(image_ber_table))/sum(image_ber_table)
# 
# print(image_ber_table)
# print(ratio)

