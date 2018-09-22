library(rgl)
library(gsubfn)
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework3")
iris.data <- read.table("./hw3-data/iris.csv", sep=",", header=T)
iris.data1 <- read.table("./hw3-data/dataI.csv", sep=",", header=T)
iris.data2 <- read.table("./hw3-data/dataII.csv", sep=",", header=T)
iris.data3 <- read.table("./hw3-data/dataIII.csv", sep=",", header=T)
iris.data4 <- read.table("./hw3-data/dataIV.csv", sep=",", header=T)
iris.data5 <- read.table("./hw3-data/dataV.csv", sep=",", header=T)

# Function that returns error
get_err <- function(input_mat1, input_mat2){
  sum_res <- 0
  for (i in 1:nrow(input_mat1)){
    sum_res <- sum_res + sum((input_mat1[i,]-input_mat2[i,])*(input_mat1[i,]-input_mat2[i,]))
  }
  return (sum_res/nrow(input_mat1))
}

# Function that substract and store mean
get_mean <- function(input_mat, output_mean){
  for (i in 1:length(input_mat)){
    output_mean[i] <- mean(input_mat[,i])
    input_mat[,i] <- input_mat[,i] - mean(input_mat[,i])
  }
  return (list(v1=input_mat, v2=output_mean))
}

# subtracting mean
x.mean <- c()
x1.mean <- c()
x2.mean <- c()
x3.mean <- c()
x4.mean <- c()
x5.mean <- c()
list0 <- get_mean(iris.data, x.mean)
list1 <- get_mean(iris.data1, x1.mean)
list2 <- get_mean(iris.data2, x2.mean)
list3 <- get_mean(iris.data3, x3.mean)
list4 <- get_mean(iris.data4, x4.mean)
list5 <- get_mean(iris.data5, x5.mean)

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
x1 <- matrix(0, nrow = 150, ncol=4)
x2 <- matrix(0, nrow = 150, ncol=4)
x3 <- matrix(0, nrow = 150, ncol=4)
x4 <- matrix(0, nrow = 150, ncol=4)
x5 <- matrix(0, nrow = 150, ncol=4)
pc.number <- 4
zero <- c(0,0,0,0)
for (i in 1:150){
  r1 <- t(eigen_vector1) %*% t(list1$v1[i,])
  r2 <- t(eigen_vector2) %*% t(list2$v1[i,])
  r3 <- t(eigen_vector3) %*% t(list3$v1[i,])
  r4 <- t(eigen_vector4) %*% t(list4$v1[i,])
  r5 <- t(eigen_vector5) %*% t(list5$v1[i,])
  if (pc.number < 4){
    p1 <- c(r1[1:pc.number], zero[(pc.number+1):4])
    p2 <- c(r2[1:pc.number], zero[(pc.number+1):4])
    p3 <- c(r3[1:pc.number], zero[(pc.number+1):4])
    p4 <- c(r4[1:pc.number], zero[(pc.number+1):4])
    p5 <- c(r5[1:pc.number], zero[(pc.number+1):4]) 
  } else {
    p1 <- r1
    p2 <- r2
    p3 <- r3
    p4 <- r4
    p5 <- r5
  }
  x1[i,] <- eigen_vector1 %*% p1 + list1$v2
  x2[i,] <- eigen_vector2 %*% p2 + list2$v2
  x3[i,] <- eigen_vector3 %*% p3 + list3$v2
  x4[i,] <- eigen_vector4 %*% p4 + list4$v2
  x5[i,] <- eigen_vector5 %*% p5 + list5$v2
}

