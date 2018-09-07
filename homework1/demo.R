library(magick)
vec <- matrix(integer(), nrow=20, ncol=20)
image_1 <- image_train_data[1,][2:401]
colnames(image_1) <- c(1:400)
print(image_1)
for (i in 1:20){
  for (j in 1:20){
    vec[[i,j]] <- image_1[[(i-1)*20+j]]
  }
}
image_read(vec)

