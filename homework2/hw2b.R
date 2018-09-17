pred_test = c()
example = c()
for (i in 1:nrow(test_data)){
  example[i] <- sprintf("'%d'", i-1)
  if (sum(a*test_data[i,][c(1,3,5,11:13)])+b > 0)
    pred_test[i] <- ">50K"
  else
    pred_test[i] <- "<=50K"
}
csv.data <- data.frame("Example"=example,
                       "Label"=pred_test)
write.csv(csv.data, file="res.csv", row.names = F)
print(levels(as.factor(pred_test)))