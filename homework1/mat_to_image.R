library(Matrix)
mat_to_image <- function(inputdata){
  mat <- matrix(integer(), nrow = 20, ncol=20)
  for (i in 1:20){
    for (j in 1:20){
      index <- sprintf("pixel%d",(i-1)*20+j)
      mat[[i,j]] <- inputdata[[index]]
    }
  }
  mat <- Matrix(mat)
  im <- image(mat)
  print(im)
}

mat_to_image(stretched_res_data[5,])