
# Take in data
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework4")
# data.batch1 <- read.table("./train_data/data_1.csv", sep=",")
# data.batch2 <- read.table("./train_data/data_2.csv", sep=",")
# data.batch3 <- read.table("./train_data/data_3.csv", sep=",")
# data.batch4 <- read.table("./train_data/data_4.csv", sep=",")
# data.batch5 <- read.table("./train_data/data_5.csv", sep=",")

# ==================== Prepare for calculation ===================
# Define some parameters
pc.number <- 20

# Function that classify data
get_classified <- function(input_data){
  res <- matrix(0, nrow=10, ncol=ncol(input_data)-1)
  for (i in 0:9){
    mat_set <- input_data[input_data[,"V3073"]==i,]
    for (j in 1:(ncol(input_data)-1)){
      res[i+1,][j-1] <- mean(mat_set[,j])
    }
  }
  return (res)
}

# Function that substract and store mean
get_mean <- function(input_mat, mean_of_mat){
  output_mean <- c()
  output_mat <- matrix(0, nrow = nrow(input_mat), ncol = ncol(input_mat))
  for (i in 1:ncol(input_mat)){
    output_mean[i] <- mean(mean_of_mat[,i])
    output_mat[,i] <- input_mat[,i] - mean(mean_of_mat[,i])
  }
  return (list(v1=output_mat, v2=output_mean))
}

# Function that returns error
get_err <- function(input_mat1, input_mat2){
  sum_res <- c()
  for (i in 1:nrow(input_mat1)){
    sum_res[i] <- sum((input_mat1[i,]-input_mat2[i,])*(input_mat1[i,]-input_mat2[i,]))/ncol(input_mat1)
  }
  return (sum_res)
}

# Function that returns reconstructed matrix.
get_res <- function(data_matrix, eigen_matrix, mean_vector){
  x <- matrix(0, nrow = nrow(data_matrix), ncol=ncol(data_matrix))
  zero <- c()
  for (i in 1:ncol(data_matrix)){
    zero[i] <- 0
  }
  
  for (i in 1:nrow(data_matrix)){
    r <- t(eigen_matrix) %*% t(data_matrix)[,i]
    p <- c(r[1:pc.number], zero[(pc.number+1):ncol(data_matrix)])
    x[i,] <- eigen_matrix %*% p + mean_vector
  }
  return (x)
}

# ==================== Actual calculation ===================
data.batch <- rbind(data.batch1, data.batch2, data.batch3, data.batch4,data.batch5)
classified <- get_classified(data.batch)
list_classified <- get_mean(classified, classified)
cov_mat <- cov(list_classified$v1)
eigen_vector <- eigen(cov_mat)$vectors
recon_data <- get_res(list_classified$v1, eigen_vector, list_classified$v2)
err_data <- get_err(classified, recon_data)
print(err_data)
# Method to plot colorful image
# r <- matrix(res[i+1,][1:1024], 32)
# g <- matrix(res[i+1,][1025:2048], 32)
# b <- matrix(res[i+1,][2049:3072], 32)
# col <- rgb(r/255, g/255, b/255)
# dim(col) <- dim(r)
# grid.raster(col, interpolate=FALSE)

# Calculate 20 principle components
