library(e1071)
library(sparklyr)
library(dplyr)
Sys.setenv(
  SPARK_HOME="C:\\spark\\spark-2.3.1-bin-hadoop2.7"
)
sc <- spark_connect(master = "local")

# Take in data
image_train_data <- read.table("train.csv", sep = ',')
image_test_data <- read.table("val.csv", sep=",")

# Delete unwanted data column
image_train_data <- image_train_data[-1]

# Rename column of data frame
colnames(image_train_data) <- image_train_data[1,]
colnames(image_test_data) <- image_test_data[1,]
colnames(image_train_data)[1] <- "Number"
colnames(image_test_data)[1] <- "Number"

# Delete unwanted data row
image_train_data <- image_train_data[-1,]
image_test_data <- image_test_data[-1,]

# Set a threshold (127)
image_train_data[image_train_data <= 127] <- 0
image_train_data[image_train_data >= 128] <- 1 
image_test_data[image_test_data <= 127] <- 0
image_test_data[image_test_data >= 128] <- 1 

# Resizing function that resize and strech the image
resizingFunc <- function(inputdata){
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

  # Stretch the image
# 
#   for (j in 2:nrow(boundingData)){
#     min_col <- 20
#     max_col <- 0
#     min_row <- 20
#     max_row <- 0
#     for (i in 2:length(boundingData[1,])){
#       row <- floor((i-2)/20)
#       col <- (i-2) %% 20
#       if (boundingData[j,][i] == 1){
#         if (row <= min_row)
#           min_row <- row
#         else if (row >= max_row)
#           max_row <- row
#         if (col <= min_col)
#           min_col <- col
#         else if (col >= max_col)
#           max_col <- col
#       }
#     }
#     # row_a <- 20/(max_row-min_row)
#     # col_a <- 20/(max_col-min_col)
#     # row_b <- 20/(min_row-max_row)
#     # col_b <- 20/(min_col-max_col)
#     # for (i in 2:length(boundingData[1,])){
#     #   row <- floor((i-2)/20)
#     #   col <- (i-2) %% 20
#     #   if (boundingData[j,][i]==1){
#     #     row_after <- round(row*row_a + row_b)
#     #     col_after <- round(col*col_a + col_b)
#     #     boundingData[j,][row_after * 20 + col_after] <- 1
#     #   }
#     # }
#   }

  return(boundingData)
}


# Gaussian & untouched

# image_model <- naiveBayes(Number~., data=image_train_data)
# image_pred <- predict(image_model, newdata=image_test_data)
# image_table <- table(actual=image_test_data[,1], predict=image_pred)
# ratio <- sum(diag(image_table))/sum(image_table)
# 
# print(image_table)
# print(ratio)

# Gaussian & streched bounding (28*28 -> 20*20)

# resizing bounding first
# image_train_data <- resizingFunc(image_train_data)
# image_test_data <- resizingFunc(image_test_data)
# image_model <- naiveBayes(Number~., data=image_train_data)
# image_pred <- predict(image_model, newdata=image_test_data)
# image_table <- table(actual=image_test_data[,1], predict=image_pred)
# ratio <- sum(diag(image_table))/sum(image_table)
# 
# print(image_table)
# print(ratio)


# Bernoulli & untouched

# image_train_tbl <- copy_to(sc, image_train_data[1:30000,])
# image_test_tbl <- copy_to(sc, image_test_data)
# image_ber_model <- image_train_tbl %>% 
#   ml_naive_bayes(Number~., model_type="bernoulli")
# image_ber_pred <- predict(image_ber_model, newdata=image_test_tbl)
# image_ber_table <- table(actual=image_test_data[,1], predict=image_ber_pred)
# ratio <- sum(diag(image_ber_table))/sum(image_ber_table)
# 
# print(image_ber_table)
# print(ratio)

# Bernoulli & streched bounding (28*28 -> 20*20)

# resizing bounding first
image_train_data <- resizingFunc(image_train_data)
image_test_data <- resizingFunc(image_test_data)

image_train_tbl <- copy_to(sc, image_train_data[1:30000,])
image_test_tbl <- copy_to(sc, image_test_data)
image_ber_model <- image_train_tbl %>%
  ml_naive_bayes(Number~., model_type="bernoulli")
image_ber_pred <- predict(image_ber_model, newdata=image_test_tbl)
image_ber_table <- table(actual=image_test_data[,1], predict=image_ber_pred)
ratio <- sum(diag(image_ber_table))/sum(image_ber_table)

print(image_ber_table)
print(ratio)
