library(caret)
library(e1071)
library(sparklyr)
library(dplyr)
sc <- spark_connect(master = "local")
Sys.setenv(
  SPARK_HOME="C:\\Users\\acdjackson\\AppData\\Local\\spark\\spark-2.3.0-bin-hadoop2.7"
)
library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

image_train_data <- read.table("train.csv", sep = ',')
image_test_data <- read.table("val.csv", sep=",")

image_train_data <- image_train_data[-1]

colnames(image_train_data) <- image_train_data[1,]
colnames(image_test_data) <- image_test_data[1,]
colnames(image_train_data)[1] <- "label"
colnames(image_test_data)[1] <- "label"

image_train_data <- image_train_data[-1,]
image_test_data <- image_test_data[-1,]

image_train_data[image_train_data <= 127] <- 0
image_train_data[image_train_data >= 128] <- 1 

image_test_data[image_test_data <= 127] <- 0
image_test_data[image_test_data >= 128] <- 1 


# Gaussian & untouched

# image_model <- naiveBayes(label~., data=image_train_data)
# image_pred <- predict(image_model, newdata=image_test_data)
# image_table <- table(actual=image_test_data[,1], predict=image_pred)
# ratio <- sum(diag(image_table))/sum(image_table)
# 
# print(image_table)
# print(ratio)

# Bernoulli & untouched
sparkR.session(appName = "Bernoulli-untouched")
Ber_df <- as.DataFrame(sc, image_train_data)
image_Ber_model <- spark.naiveBayes(Ber_df, label~.)
image_Ber_pred <- predict(image_Ber_model, newdata=image_test_data)
image_Ber_table <- table(actual=image_test_data[,1], predict=image_Ber_pred)
ratio <- sum(diag(image_Ber_table))/sum(image_Ber_table)

print(image_Ber_table)
print(ratio)

