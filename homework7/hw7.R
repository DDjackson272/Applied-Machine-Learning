library(glmnet)
library(dplyr)
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework7")
train_blog <- read.table("./BlogFeedback/blogData_train.csv", sep=",")
test_folder <- list.files("./BlogFeedback/test")

get_all_data <- function(input_file_list, input_file_path){
  temp <- read.table(paste(input_file_path, input_file_list[1], sep=""), sep=",")
  for (i in 2:length(input_file_list)){
    temp_origin <- read.table(paste(input_file_path, input_file_list[i], sep=""), sep=",")
    # print(length(temp_origin))
    temp <- rbind(temp, temp_origin)
  }
  rownames(temp) <- 1:nrow(temp)
  return (temp)
}

test_blog <- get_all_data(test_folder, "./BlogFeedback/test/")

# problem a
y <- as.matrix(train_blog[,281])
x <- as.matrix(train_blog[,1:280])
mod <- cv.glmnet(x,y,family="poisson",alpha=1)
plot(mod)

# problem b
fit1 <- predict(mod, newx=x, "response", s = "lambda.min")
fit2 <- predict(mod, newx=x, "response", s = "lambda.1se")
plot(y, fit1, xlab="True value", ylab="Fitted value", 
     main="Fitted value vs True value")
plot(y, fit2, xlab="True value", ylab="Fitted value", 
     main="Fitted value vs True value")

# problem c
test_x <- as.matrix(test_blog[,1:280])
test_y <- as.matrix(test_blog[,281])
fit_test_1 <- predict(mod, newx=test_x, "response", s="lambda.min")
fit_test_2 <- predict(mod, newx=test_x, "response", s="lambda.1se")
plot(test_y, fit_test_1, xlab="True value", ylab="Fitted value", 
     main="Fitted value vs True value")
plot(test_y, fit_test_2, xlab="True value", ylab="Fitted value", 
     main="Fitted value vs True value")

