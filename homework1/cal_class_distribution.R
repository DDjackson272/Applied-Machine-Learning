library(Matrix)
g_u <- read.table("b_t.csv", sep=",")
colnames(g_u)[1] = "Number"

res <- matrix(0, nrow=10, ncol=length(g_u)-1)
for (i in 0:9){
  mat_set <- g_u[g_u[,"Number"]==i,]
  for (j in 2:length(g_u)){
    res[i+1,][j-1] <- (1-sum(mat_set[,j])/nrow(mat_set)) * 255
  }
}
for (i in 1:10){
  mat <- matrix(res[i,], nrow=20)
  Mat <- Matrix(mat)
  im <- image(Mat)
  print(im)
}


