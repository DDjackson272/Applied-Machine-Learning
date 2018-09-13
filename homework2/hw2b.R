pred_test = c()
for (i in 1:nrow(test_data)){
  if (sum(a*test_data[i,][1:4], na.rm=T)+b > 0)
    pred_test[i] <- "<=50K"
  else
    pred_test[i] <- ">50K"
}
write.csv(pred_test, file="res.csv")
print(levels(as.factor(pred_test)))