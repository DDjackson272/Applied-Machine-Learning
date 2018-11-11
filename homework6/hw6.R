library(matlib)
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework6")
housing_data <- read.table("housing.data")
linearMod <- lm(V14~., data=housing_data)
x_matrix <- as.matrix(housing_data[,1:13])
plot(linearMod)

laverage_threshold <- 0.2
cook_threshold <- 0.5
std_res_threshold <- 3
hat <- hatvalues(linearMod)
cook <- cooks.distance(linearMod)
stdr <- rstandard(linearMod)

del_outliers <- function(input_matrix){
  to_del <- c()
  del_count <- 1
  for (i in 1:nrow(input_matrix)){
    # delete laverage outlier
    laverage_flag = hat[i] > laverage_threshold
    
    # delete cook's distance outlier
    cook_flag = cook[i] > cook_threshold
    
    # delete standardized residuce
    std_res_flag = stdr[i] > std_res_threshold || stdr[i] < -std_res_threshold

    if (laverage_flag || cook_flag || std_res_flag){
      to_del[del_count] = i
      del_count <- del_count + 1
    }
  }
  print(to_del)
  return (input_matrix[-to_del,])
}

after_del_outliers <- del_outliers(housing_data)
write.csv(after_del_outliers, file="del_outliers.csv", row.names=F)
plot(lm(V14~.,data=after_del_outliers))
