
# Setting working directory and taking in data
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework3")
iris.data <- read.table("./hw3-data/iris.csv", sep=",", header=T)
iris.data1 <- read.table("./hw3-data/dataI.csv", sep=",", header=T)
iris.data2 <- read.table("./hw3-data/dataII.csv", sep=",", header=T)
iris.data3 <- read.table("./hw3-data/dataIII.csv", sep=",", header=T)
iris.data4 <- read.table("./hw3-data/dataIV.csv", sep=",", header=T)
iris.data5 <- read.table("./hw3-data/dataV.csv", sep=",", header=T)

# Setting parameters here
pc.number <- 2
use.noiesless <- FALSE

# Function that returns error
get_err <- function(input_mat1, input_mat2){
  sum_res <- 0
  for (i in 1:nrow(input_mat1)){
    sum_res <- sum_res + sum((input_mat1[i,]-input_mat2[i,])*(input_mat1[i,]-input_mat2[i,]))
  }
  return (sum_res/nrow(input_mat1))
}

# Function that substract and store mean
get_mean <- function(input_mat, output_mean, mean_of_mat){
  for (i in 1:length(input_mat)){
    output_mean[i] <- mean(mean_of_mat[,i])
    input_mat[,i] <- input_mat[,i] - mean(mean_of_mat[,i])
  }
  return (list(v1=input_mat, v2=output_mean))
}

# Function that returns reconstructed matrix.
get_res <- function(input.list, eigen.vector){
  x <- matrix(0, nrow = 150, ncol=4)
  zero <- c(0,0,0,0)
  for (i in 1:150){
    r <- t(eigen.vector) %*% t(input.list$v1[i,])
    if (0 < pc.number && pc.number < 4){
      p <- c(r[1:pc.number], zero[(pc.number+1):4])
    } else if (pc.number == 4) {
      p <- r
    } else if (pc.number == 0) {
      p <- zero
    }
    x[i,] <- eigen.vector %*% p + input.list$v2
  }
  return (x)
}

# subtracting mean
x.mean <- c()
x1.mean <- c()
x2.mean <- c()
x3.mean <- c()
x4.mean <- c()
x5.mean <- c()
list0 <- get_mean(iris.data, x.mean, iris.data)
if (!use.noiesless){
  list1 <- get_mean(iris.data1, x1.mean, iris.data1)
  list2 <- get_mean(iris.data2, x2.mean, iris.data2)
  list3 <- get_mean(iris.data3, x3.mean, iris.data3)
  list4 <- get_mean(iris.data4, x4.mean, iris.data4)
  list5 <- get_mean(iris.data5, x5.mean, iris.data5)
} else {
  list1 <- get_mean(iris.data1, x1.mean, iris.data)
  list2 <- get_mean(iris.data2, x2.mean, iris.data)
  list3 <- get_mean(iris.data3, x3.mean, iris.data)
  list4 <- get_mean(iris.data4, x4.mean, iris.data)
  list5 <- get_mean(iris.data5, x5.mean, iris.data)
}

# calculate covariance matrix
cov_mat <- cov(list0$v1[,1:4])
cov_mat1 <- cov(list1$v1[,1:4])
cov_mat2 <- cov(list2$v1[,1:4])
cov_mat3 <- cov(list3$v1[,1:4])
cov_mat4 <- cov(list4$v1[,1:4])
cov_mat5 <- cov(list5$v1[,1:4])

# calculate eigenvalue and eigenvactor
eigen_vector <- eigen(cov_mat)$vectors
eigen_vector1 <- eigen(cov_mat1)$vectors
eigen_vector2 <- eigen(cov_mat2)$vectors
eigen_vector3 <- eigen(cov_mat3)$vectors
eigen_vector4 <- eigen(cov_mat4)$vectors
eigen_vector5 <- eigen(cov_mat5)$vectors

#generate the final data
if (!use.noiesless){
  x1 <- get_res(list1, eigen_vector1)
  x2 <- get_res(list2, eigen_vector2)
  x3 <- get_res(list3, eigen_vector3)
  x4 <- get_res(list4, eigen_vector4)
  x5 <- get_res(list5, eigen_vector5)
} else {
  x1 <- get_res(list1, eigen_vector)
  x2 <- get_res(list2, eigen_vector)
  x3 <- get_res(list3, eigen_vector)
  x4 <- get_res(list4, eigen_vector)
  x5 <- get_res(list5, eigen_vector)
}


print(pc.number)
print(get_err(x1, iris.data))
print(get_err(x2, iris.data))
print(get_err(x3, iris.data))
print(get_err(x4, iris.data))
print(get_err(x5, iris.data))
