pred_test = c()
for (i in 1:nrow(test_data)){
  if (sum(a*test_data[i,][1:14])+b > 0)
    pred_test[i] <- 1
  else
    pred_test[i] <- -1
}
