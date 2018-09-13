library(randomForest)
# take in data from csv
image_train_data_untouched <- read.table("train_noheader.csv", sep = ',')
colnames(image_train_data_untouched)[1] <- "Number"
colnames(image_train_data_untouched)[2:785] <- sprintf("pixel%d", 1:784)
image_train_data_untouched$Number <- as.factor(image_train_data_untouched$Number)
image_train_data_untouched[image_train_data_untouched <= 127] <- 0
image_train_data_untouched[image_train_data_untouched >= 128] <- 1 

# image_test_data_untouched <- read.table("val_noheader.csv", sep=',')
# colnames(image_test_data_untouched)[1] <- "Number"
# image_test_data_untouched$Number <- as.factor(image_test_data_untouched$Number)
# image_test_data_untouched[image_test_data_untouched <= 127] <- 0
# image_test_data_untouched[image_test_data_untouched >= 128] <- 1

image_res_data_untouched <- read.table("test.csv", sep=",")
colnames(image_res_data_untouched) <- sprintf("pixel%d", 1:784)
image_res_data_untouched[image_res_data_untouched <= 127] <- 0
image_res_data_untouched[image_res_data_untouched >= 128] <- 1

# image_train_data_stretched <- read.table("train_stretched_noheader.csv", sep=",")
# colnames(image_train_data_stretched)[1] <- "Number"
# colnames(image_train_data_stretched)[2:401] <- sprintf("pixel%d", 1:400)
# image_train_data_stretched$Number <- as.factor(image_train_data_stretched$Number)

# image_test_data_stretched <- read.table("val_stretched_noheader.csv", sep=',')
# colnames(image_test_data_stretched)[1] <- "Number"
# image_test_data_stretched$Number <- as.factor(image_test_data_stretched$Number)

# image_res_data_stretched <- read.table("test_stretched2.csv", sep=",")
# colnames(image_res_data_stretched) <- sprintf("pixel%d", 1:400)

# generate random forest
model_rf_untouched <- randomForest(Number~., data=image_train_data_untouched, maxnodes=15, ntree=30)
# model_rf_stretched <- randomForest(Number~., data=image_train_data_stretched, maxnodes=15, ntree=10)

untouched_pred <- predict(model_rf_untouched, newdata=image_res_data_untouched)
# stretched_pred <- predict(model_rf_stretched, newdata=image_res_data_stretched)
write.csv(untouched_pred, file="try.csv")
# untouched_table <- table(image_test_data_untouched$Number, untouched_pred)
# stretched_table <- table(image_test_data_stretched$Number, stretched_pred)
# 
# untouched_ratio <- sum(diag(untouched_table))/sum(untouched_table)
# stretched_ratio <- sum(diag(stretched_table))/sum(stretched_table)
# 
# print("UNTOUCHED:")
# print(untouched_table)
# print(untouched_ratio)
# 
# print("STRETCHED:")
# print(stretched_table)
# print(stretched_ratio)

