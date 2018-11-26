# setwd("/Users/hengzhe/OneDrive - University of Illinois - Urbana/UIUC/CS498/homework9")
# test_data <- read.table("./test_data.csv", sep=",")
# train_data <- read.table("./train_data.csv", sep=",")
# test_label <- read.table("./test_label.csv", sep=",")
# train_label <- read.table("./train_label.csv", sep=",")
# print("load done!")
# 
# test_data[test_data <= 127] <- -1
# test_data[test_data >= 128] <- 1
# train_data[train_data <= 127] <- -1
# train_data[train_data >= 128] <- 1
# print("binarilized done!")
# 
# training_set_image <- train_data[1:500,]
# 
# set.seed(as.numeric(Sys.time()))
# for (i in 1:500){
#   # 16 is 2% of 784 pixels
#   flipped_pixels = sample(1:784, 16)
#   training_set_image[i,flipped_pixels] <- (-1) * training_set_image[i,flipped_pixels]
# }
# print("flipped 2% of pixels done!")

theta <- 2
recon <- training_set_image

H <- c(rep(0, 784))
X <- training_set_image[1,]
diff_count <- 0
for (i in 1:784){
  if (H[i] != train_data[1,i]){
    diff_count <- diff_count + 1
  }
}
print(diff_count)

for (iter in 1:10){
  for (col in 1:28){
    for (row in 1:28){
      pos = (col-1) * 28 + row
      neighbour <- c(-1,-1,-1,-1)
      count <- 1
      if (col-1 >= 1){
        neighbour[count] <- (col-2)*28 + row
        count <- count + 1
      }
      if (row-1 >= 1){
        neighbour[count] <- (col-1)*28 +row-1
        count <- count + 1
      }
      if (col + 1 <= 28){
        neighbour[count] <- col * 28 + row
        count <- count + 1
      }
      if (row + 1 <= 28){
        neighbour[count] <- (col-1)*28 + row + 1
      }
      p1 <- 0
      p2 <- 0
      for (i in 1:4){
        if (neighbour[i] != -1){
          p1 <- p1 + theta * 1 * X[,neighbour[i]]
          p2 <- p2 + theta * -1 * X[,neighbour[i]]
        }
      }
      if (p1 < p2){
        H[pos] = -1
      } else {
        H[pos] = 1
      }
    }
  }
  diff_count <- 0
  for (i in 1:784){
    if (H[i] != train_data[1,i]){
      diff_count <- diff_count + 1
    }
  }
  print(diff_count) 
}