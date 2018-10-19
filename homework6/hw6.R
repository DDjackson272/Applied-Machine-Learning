library(matlib)
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework6")
housing_data <- read.table("housing.data")
linearMod <- lm(V14~., data=housing_data)
x_matrix <- as.matrix(housing_data[,1:13])
# plot(linearMod)

# laverage
laverage_threshold <- 0.04
hat <- x_matrix %*% inv((t(x_matrix) %*% x_matrix)) %*% t(x_matrix)
laverage_to_delete <- c()
laverage_count <- 1
for (i in 1:nrow(hat)){
  if (hat[i,][i] > laverage_threshold){
    laverage_to_delete[laverage_count] <- i
    laverage_count <- laverage_count + 1
  }
}
after_laverage <- housing_data[-laverage_to_delete,]
rownames(after_laverage) <- 1:nrow(after_laverage)

# cook's distance
cook_threshold <- 1
linearMod_after_laverage <- lm(V14~., data=after_laverage)
origin_matrix <- linearMod_after_laverage$coefficients[1] + (as.matrix(after_laverage[,1:13]) %*% linearMod_after_laverage$coefficients[2:14])

cal_err <- function(matrix1, matrix2){
  err <- 0
  for (i in 1:nrow(matrix1)){
    err <- err + (matrix1[i,][1] - matrix2[i,][1])^2
  }
  return (err)
}

delete_outliers <- function(input_matrix){
  to_delete <- c()
  count <- 1
  for (i in 1:nrow(input_matrix)){
    temp <- input_matrix[-c(i),]
    if (nrow(temp) == nrow(input_matrix)-1){
      linearMod <- lm(V14~., data=temp)
      recon <- linearMod$coefficients[1] + (as.matrix(input_matrix[,1:13]) %*% linearMod$coefficients[2:14])
      err <- cal_err(recon, origin_matrix)
      print(sprintf("row: %d, mse: %f", i, err))
      if (err > cook_threshold){
        to_delete[count] <- i
        count <- count + 1
      }
    }
  }
  return (input_matrix[-to_delete,])
}

after_cook <- delete_outliers(after_laverage)
rownames(after_cook) <- 1:nrow(after_cook)
# standardized residuals

cal_std_res <- function(input_matrix){
  linear_model <- lm(V14~., data=input_matrix)
  recon <- linear_model$coefficients[1] + (as.matrix(input_matrix[,1:13]) %*% linear_model$coefficients[2:14])
  x_mat <- as.matrix(input_matrix[,1:13])
  h <- x_mat %*% inv((t(x_mat) %*% x_mat)) %*% t(x_mat)
  e <- as.matrix(input_matrix[,14]) - recon
  var <- (t(e) %*% e) / nrow(input_matrix)
  del <- c()
  count <- 1
  for (i in 1:nrow(e)){
    s <- e[i] / ((var * (1-h[i,][i]))^0.5)
    print(s)
    if (s > 2 || s < -2){
      del[count] <- i
      count <- count + 1
    }
  }
  return (input_matrix[-del,])
}

after_std_res <- cal_std_res(after_cook)
rownames(after_std_res) <- 1:nrow(after_std_res)
linearMod_after_std_res <- lm(V14~., data=after_std_res)
plot(linearMod_after_std_res)