setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework8")
library(jpeg)
library(mvtnorm)
test1RGB <- readJPEG("test1.jpg")
test2RGB <- readJPEG("test2.jpg")
test3RGB <- readJPEG("test3.jpg")
test4RGB <- readJPEG("test4.jpg")

segments_value <- 50
iter_time <- 20
inputRGB <- test3RGB

flatten_data <- function(input_matrix){
  data <- matrix(input_matrix[,,1], ncol=1)
  data <- cbind(data, matrix(input_matrix[,,2], ncol=1))
  data <- cbind(data, matrix(input_matrix[,,3], ncol=1))
  return (data.frame(data))
}

inputData <- flatten_data(inputRGB)
inputRow <- nrow(inputRGB)
inputCol <- ncol(inputRGB)

# mvrnorm(n, mu, Sigma)
# initialize all mu and Sigmas
startPoint = kmeans(inputData, center=segments_value)
mu <- array(startPoint$centers,dim=c(1,3,segments_value))
stds <- array(c(sd(inputData[,1], sd(inputData[,2], sd(inputData[,3])))),dim=c(1,3,segments_value))
weights_r <- array(1/segments_value, dim=c(nrow(inputData), segments_value))
weights_g <- array(1/segments_value, dim=c(nrow(inputData), segments_value))
weights_b <- array(1/segments_value, dim=c(nrow(inputData), segments_value))
priors_r <- array(0, dim=c(nrow(inputData), segments_value))
priors_g <- array(0, dim=c(nrow(inputData), segments_value))
priors_b <- array(0, dim=c(nrow(inputData), segments_value))
flag = TRUE
while(flag){
  mu_old <- mu
  std_old <- stds
  # print(sprintf("%d th iteration inputData:", k))
  # print(inputData[1:5,])
  for (i in 1:segments_value){
    priors_r[,i] <- dnorm(inputData[,1], mu[1,1,i], stds[1,1,i])
    priors_g[,i] <- dnorm(inputData[,2], mu[1,2,i], stds[1,2,i])
    priors_b[,i] <- dnorm(inputData[,3], mu[1,3,i], stds[1,3,i])
  }
  for (i in 1:segments_value){
    weights_r[,i] = priors_r[,i]/rowSums(priors_r)
    weights_g[,i] = priors_g[,i]/rowSums(priors_g)
    weights_b[,i] = priors_b[,i]/rowSums(priors_b)
  }
  for (i in 1:segments_value){
    mu[1,1,i] <- sum(inputData[,1] * weights_r[,i])/sum(weights_r[,i])
    mu[1,2,i] <- sum(inputData[,2] * weights_r[,i])/sum(weights_r[,i])
    mu[1,3,i] <- sum(inputData[,3] * weights_r[,i])/sum(weights_r[,i])
  }
  for (i in 1:segments_value){
    stds[1,1,i] <- sqrt(sum((inputData[,1]-mu[1,1,i])**2 * weights_r[,i])/sum(weights_r[,i]))
    stds[1,2,i] <- sqrt(sum((inputData[,2]-mu[1,2,i])**2 * weights_g[,i])/sum(weights_g[,i]))
    stds[1,3,i] <- sqrt(sum((inputData[,3]-mu[1,3,i])**2 * weights_b[,i])/sum(weights_b[,i]))
  }
  print(abs(sum(mu)-sum(mu_old)))
  if (abs(sum(mu)-sum(mu_old)) < 0.1){
    flag = FALSE
  }
}

# # reconstruct image
for (i in 1:nrow(inputData)){
  res_r <- array(0, dim=c(segments_value))
  res_g <- array(0, dim=c(segments_value))
  res_b <- array(0, dim=c(segments_value))
  for (j in 1:segments_value){
    res_r[j] <- dnorm(inputData[i,1], mu[1,1,j], stds[1,1,j])
    res_g[j] <- dnorm(inputData[i,2], mu[1,2,j], stds[1,2,j])
    res_b[j] <- dnorm(inputData[i,3], mu[1,3,j], stds[1,3,j])
  }
  print(i)
  inputData[i,1] <- mu[1,1,which.max(res_r)]
  inputData[i,2] <- mu[1,2,which.max(res_g)]
  inputData[i,3] <- mu[1,3,which.max(res_b)]
}

r <- array(inputData[,1], dim=c(inputRow, inputCol))
g <- array(inputData[,2], dim=c(inputRow, inputCol))
b <- array(inputData[,3], dim=c(inputRow, inputCol))
recon <- array(0, dim=c(inputRow, inputCol,3))
recon[,,1] <- r
recon[,,2] <- g
recon[,,3] <- b

writeJPEG(recon, "demo.jpg")
