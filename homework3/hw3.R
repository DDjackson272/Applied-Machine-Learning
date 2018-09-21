library(rgl)
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework3")
iris.data <- read.table("./hw3-data/iris.csv", sep=",", header=T)
iris.data1 <- read.table("./hw3-data/dataI.csv", sep=",", header=T)
iris.data2 <- read.table("./hw3-data/dataII.csv", sep=",", header=T)
iris.data3 <- read.table("./hw3-data/dataIII.csv", sep=",", header=T)
iris.data4 <- read.table("./hw3-data/dataIV.csv", sep=",", header=T)
iris.data5 <- read.table("./hw3-data/dataV.csv", sep=",", header=T)
# subtracting mean
for (i in 1:length(iris.data)){
  if (is.numeric(iris.data[,i])){
    iris.data[,i] <- iris.data[,i] - mean(iris.data[,i])
    iris.data1[,i] <- iris.data1[,i] - mean(iris.data1[,i])
    iris.data2[,i] <- iris.data2[,i] - mean(iris.data2[,i])
    iris.data3[,i] <- iris.data3[,i] - mean(iris.data3[,i])
    iris.data4[,i] <- iris.data4[,i] - mean(iris.data4[,i])
    iris.data5[,i] <- iris.data5[,i] - mean(iris.data5[,i])
  }
}

# calculate covariance matrix
cov_mat <- cov(iris.data[,1:4])
cov_mat1 <- cov(iris.data1[,1:4])
cov_mat2 <- cov(iris.data2[,1:4])
cov_mat3 <- cov(iris.data3[,1:4])
cov_mat4 <- cov(iris.data4[,1:4])
cov_mat5 <- cov(iris.data5[,1:4])

# calculate eigenvalue and eigenvactor
eigen_vector <- eigen(cov_mat)$vectors
eigen_vector1 <- eigen(cov_mat1)$vectors
eigen_vector2 <- eigen(cov_mat2)$vectors
eigen_vector3 <- eigen(cov_mat3)$vectors
eigen_vector4 <- eigen(cov_mat4)$vectors
eigen_vector5 <- eigen(cov_mat5)$vectors

#generate the feature vector
feat_vectors <- eigen_vector[,1:2]
feat_vectors1 <- eigen_vector1[,1:2]
feat_vectors2 <- eigen_vector2[,1:2]
feat_vectors3 <- eigen_vector3[,1:2]
feat_vectors4 <- eigen_vector4[,1:2]
feat_vectors5 <- eigen_vector5[,1:2]

#generate the final data
final_data <- t(t(feat_vectors) %*% t(iris.data[,1:4]))
final_data1 <- t(t(feat_vectors1) %*% t(iris.data1[,1:4]))
final_data2 <- t(t(feat_vectors2) %*% t(iris.data2[,1:4]))
final_data3 <- t(t(feat_vectors3) %*% t(iris.data3[,1:4]))
final_data4 <- t(t(feat_vectors4) %*% t(iris.data4[,1:4]))
final_data5 <- t(t(feat_vectors5) %*% t(iris.data5[,1:4]))

plot(final_data[,1], final_data[,2])
plot(final_data1[,1], final_data1[,2])
plot(final_data2[,1], final_data2[,2])
plot(final_data3[,1], final_data3[,2])
plot(final_data4[,1], final_data4[,2])
plot(final_data5[,1], final_data5[,2])
