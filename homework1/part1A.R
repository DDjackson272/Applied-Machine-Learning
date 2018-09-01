library(caret)
data <- read.table("pima-indians-diabetes.csv",sep = ',')
colnames(data) <- c("feat_a","feat_b","feat_c","feat_d","feat_e","feat_f","feat_g","feat_h","res")

start <- 1
end <- round(nrow(data)/10)
total_acc <- 0
for (i in 1:10){
  eva_data <- data[start:end,]
  ind <- createDataPartition(eva_data$res, times=1, p=0.8, list=F)
  train_data <- eva_data[ind,]
  test_data <- eva_data[-ind,]
  
  p_y <- sum(train_data$res=="TRUE")/nrow(train_data)
  p_n <- sum(train_data$res=="FALSE")/nrow(train_data)
  ind_y <- train_data$res == "TRUE"
  ind_n <- train_data$res == "FALSE"
  
  feat_a_y_mean <- mean(train_data[ind_y,]$feat_a)
  feat_a_n_mean <- mean(train_data[ind_n,]$feat_a)
  feat_a_y_sd <- sd(train_data[ind_y,]$feat_a)
  feat_a_n_sd <- sd(train_data[ind_n,]$feat_a)

  feat_b_y_mean <- mean(train_data[ind_y,]$feat_b)
  feat_b_n_mean <- mean(train_data[ind_n,]$feat_b)
  feat_b_y_sd <- sd(train_data[ind_y,]$feat_b)
  feat_b_n_sd <- sd(train_data[ind_n,]$feat_b)
  
  feat_c_y_mean <- mean(train_data[ind_y,]$feat_c)
  feat_c_n_mean <- mean(train_data[ind_n,]$feat_c)
  feat_c_y_sd <- sd(train_data[ind_y,]$feat_c)
  feat_c_n_sd <- sd(train_data[ind_n,]$feat_c)
  
  feat_d_y_mean <- mean(train_data[ind_y,]$feat_d)
  feat_d_n_mean <- mean(train_data[ind_n,]$feat_d)
  feat_d_y_sd <- sd(train_data[ind_y,]$feat_d)
  feat_d_n_sd <- sd(train_data[ind_n,]$feat_d)
  
  feat_e_y_mean <- mean(train_data[ind_y,]$feat_e)
  feat_e_n_mean <- mean(train_data[ind_n,]$feat_e)
  feat_e_y_sd <- sd(train_data[ind_y,]$feat_e)
  feat_e_n_sd <- sd(train_data[ind_n,]$feat_e)
  
  feat_f_y_mean <- mean(train_data[ind_y,]$feat_f)
  feat_f_n_mean <- mean(train_data[ind_n,]$feat_f)
  feat_f_y_sd <- sd(train_data[ind_y,]$feat_f)
  feat_f_n_sd <- sd(train_data[ind_n,]$feat_f)
  
  feat_g_y_mean <- mean(train_data[ind_y,]$feat_g)
  feat_g_n_mean <- mean(train_data[ind_n,]$feat_g)
  feat_g_y_sd <- sd(train_data[ind_y,]$feat_g)
  feat_g_n_sd <- sd(train_data[ind_n,]$feat_g)
  
  feat_h_y_mean <- mean(train_data[ind_y,]$feat_h)
  feat_h_n_mean <- mean(train_data[ind_n,]$feat_h)
  feat_h_y_sd <- sd(train_data[ind_y,]$feat_h)
  feat_h_n_sd <- sd(train_data[ind_n,]$feat_h)
  pred <- c()
  for (i in 1:nrow(test_data)){
    y <- pnorm(test_data[i,]$feat_a,feat_a_y_mean,feat_a_y_sd,log.p=T)+pnorm(test_data[i,]$feat_b,feat_b_y_mean,feat_b_y_sd,log.p=T)+
      pnorm(test_data[i,]$feat_c,feat_c_y_mean,feat_c_y_sd,log.p=T)+pnorm(test_data[i,]$feat_d,feat_d_y_mean,feat_d_y_sd,log.p=T)+
      pnorm(test_data[i,]$feat_e,feat_e_y_mean,feat_e_y_sd,log.p=T)+pnorm(test_data[i,]$feat_f,feat_f_y_mean,feat_f_y_sd,log.p=T)+
      pnorm(test_data[i,]$feat_g,feat_g_y_mean,feat_g_y_sd,log.p=T)+pnorm(test_data[i,]$feat_h,feat_h_y_mean,feat_h_y_sd,log.p=T)+
      log(p_y)
    n <- pnorm(test_data[i,]$feat_a,feat_a_n_mean,feat_a_n_sd,log.p=T)+pnorm(test_data[i,]$feat_b,feat_b_n_mean,feat_b_n_sd,log.p=T)+
      pnorm(test_data[i,]$feat_c,feat_c_n_mean,feat_c_n_sd,log.p=T)+pnorm(test_data[i,]$feat_d,feat_d_n_mean,feat_d_n_sd,log.p=T)+
      pnorm(test_data[i,]$feat_e,feat_e_n_mean,feat_e_n_sd,log.p=T)+pnorm(test_data[i,]$feat_f,feat_f_n_mean,feat_f_n_sd,log.p=T)+
      pnorm(test_data[i,]$feat_g,feat_g_n_mean,feat_g_n_sd,log.p=T)+pnorm(test_data[i,]$feat_h,feat_h_n_mean,feat_h_n_sd,log.p=T)+
      log(p_n)
    if(y>n){
      pred[i] <- c("TRUE")
    } else {
      pred[i] <- ("FALSE")
    }
  }
  
  nb_table <- table(actual=test_data$res, predict=pred)
  ratio <- sum(diag(nb_table))/sum(nb_table)

  print(nb_table)
  print(ratio)

  total_acc <- total_acc + ratio
  
  start <- start + round(nrow(data)/10)
  end <- end + round(nrow(data)/10)
  if(end > nrow(data))
    end <- nrow(data)
}

print(total_acc/10)



