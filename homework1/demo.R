start_time <- proc.time()
for (i in 1:nrow(image_train_data)){
  vec <- matrix(integer(), nrow=20, ncol=20)
  image_1 <- image_train_data[i,][2:401]
  colnames(image_1) <- c(1:400)
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
      vec[[i,j]] <- image_1[[(i-1)*20+j]]
    }
  }
  row_a <- 20/(max_row-min_row)
  row_b <- 20*min_row/(min_row-max_row)
  col_a <- 20/(max_col-min_col)
  col_b <- 20*min_col/(min_col-max_col)
  
  for (i in 1:20){
    for (j in 1:20){
      if(vec[[i,j]]==1){
        end_y <- round(col_a*j+col_b)
        if (end_y < 1 || end_y > 20)
          end_y <- j
        vec[[i, end_y]] <- 1
        end_x <- round(row_a*i+row_b)
        if (end_x < 1 || end_x > 20)
          end_x <- i
        vec[[end_x, j]] <- 1
      }
    }
  }
}

total <- proc.time() - start_time
print(total)


