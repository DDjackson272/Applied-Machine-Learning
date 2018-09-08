library(Matrix)
mat_to_image <- function(inputdata){
  mat <- matrix(integer(), nrow = 20, ncol=20)
  for (i in 1:20){
    for (j in 1:20){
      mat[[i,j]] <- inputdata[[(i-1)*20+j]]
    }
  }
  mat <- Matrix(mat)
  im <- image(mat)
  print(im)
}

resizingFunc <- function(inputdata){
  
  print("Start truncating vector!")
  # Resizing from 28*28 to 20*20
  col_to_delete <- vector()
  count <- 1
  for (i in 1:length(inputdata[1,])){
    row <- floor((i-2)/28)
    col <- (i-2) %% 28
    if ((row <= 3 || row >= 24) || (col <= 3 || col >= 24)){
      col_to_delete[count] <- i
      count <- count + 1
    }
  }
  
  boundingData <- inputdata[-col_to_delete]
  
  print("Start stretching vector!")
  
  # Stretch the image
  max_col <- 1
  min_col <- 20
  max_row <- 1
  min_row <- 20
  for (k in 1:nrow(boundingData)){
    print(k)
    image_1 <- boundingData[k,]
    max_col <- 1
    min_col <- 20
    max_row <- 1
    min_row <- 20
    for (i in 1:20){
      for (j in 1:20){
        if(image_1[[(i-1)*20+j]] == 1){
          if (i < min_row)
            min_row <- i
          if (i > max_row)
            max_row <- i
          if (j < min_col)
            min_col <- j
          if (j > max_col)
            max_col <- j 
        }
      }
    }
    
    row_a <- 20/(max_row-min_row)
    row_b <- 20*min_row/(min_row-max_row)
    col_a <- 20/(max_col-min_col)
    col_b <- 20*min_col/(min_col-max_col)
    
    for (i in 1:20){
      for (j in 1:20){
        if(image_1[[(i-1)*20+j]] == 1){
          end_y <- round(col_a*j+col_b)
          if (end_y < 1 || end_y > 20)
            end_y <- j
          image_1[[(i-1)*20+end_y]] <- 1
          end_x <- round(row_a*i+row_b)
          if (end_x < 1 || end_x > 20)
            end_x <- i
          image_1[[(end_x-1)*20+j]] <- 1
        }
      }
    }
    
    boundingData[k,] <- image_1
    # 
    # colnames(image_1) <- c(1:400)
    # mat_to_image(image_1)
  }
  
  return(boundingData)
}

res_data <- read.table("test.csv", sep=",")
res_data[res_data <= 127] <- 0
res_data[res_data >= 128] <- 1
stretched_res_data <- resizingFunc(res_data)
