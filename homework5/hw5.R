library(factoextra)
library(randomForest)
library(caret)
setwd("/Users/hengzhe/Library/Mobile Documents/com~apple~CloudDocs/UIUC/CS-498-Applied-Machine-Learning/homework5")
brush_teeth <- list.files("./HMP_Dataset/Brush_teeth")
climb_stairs <- list.files("./HMP_Dataset/Climb_stairs")
comb_hair <- list.files("./HMP_Dataset/Comb_hair")
descend_stair <- list.files("./HMP_Dataset/Descend_stairs")
drink_glass <- list.files("./HMP_Dataset/Drink_glass")
eat_meat <- list.files("./HMP_Dataset/Eat_meat")
eat_soup <- list.files("./HMP_Dataset/Eat_soup")
getup_bed <- list.files("./HMP_Dataset/Getup_bed")
liedown_bed <- list.files("./HMP_Dataset/Liedown_bed")
pour_water <- list.files("./HMP_Dataset/Pour_water")
sitdown_chair <- list.files("./HMP_Dataset/Sitdown_chair")
standup_chair <- list.files("./HMP_Dataset/Standup_chair")
use_telephone <- list.files("./HMP_Dataset/Use_telephone")
walk <- list.files("./HMP_Dataset/Walk")

segment_length <- 16
cluster_number <- 480

transform_data <- function(input_matrix, matrix_name){
  for (i in 1:nrow(input_matrix)){
    if (i %% segment_length == 1){
      if (i == segment_length+1){
        # print(temp_col)
        temp_row <- temp_col
      } else if (i > segment_length+1){
        temp_row <- rbind(temp_row, temp_col)
      }
      temp_col <- input_matrix[i,]
    } else {
      temp_col <- cbind(temp_col, input_matrix[i,])
    }
  }
  temp_row <- cbind(temp_row, matrix_name)
  return (temp_row)
}

get_all_data <- function(input_file_list, input_file_path, name){
  temp <- read.table(paste(input_file_path, input_file_list[1], sep=""), sep=" ")
  temp <- transform_data(temp, paste(name, 1, sep=""))
  for (i in 2:length(input_file_list)){
    temp_origin <- read.table(paste(input_file_path, input_file_list[i], sep=""), sep=" ")
    transformed <- transform_data(temp_origin, paste(name, i, sep=""))
    # print(transformed[1:3,][,1:6])
    temp <- rbind(temp, transformed)
  }
  colnames(temp)[1:(3*segment_length)] <- sprintf("V%d", 1:(3*segment_length))
  rownames(temp) <- 1:nrow(temp)
  return (temp)
}

build_feature_matrix <- function(all_data, file_folder, class_name){
  feature_matrix <- matrix(0, nrow = length(file_folder), ncol = cluster_number+1)
  for (i in 1:length(file_folder)){
    search_name <- paste(class_name, sprintf("%d",i), sep="")
    temp_matrix <- all_data[all_data[,"matrix_name"]==search_name,]
    feat <- vector(length=(cluster_number+1))
    for(j in 1:cluster_number){
      feature_matrix[i,][j] <- round((nrow(temp_matrix[temp_matrix[, "res.hk$cluster"]==j,])/nrow(temp_matrix))*100)
    }
    feature_matrix[i,][cluster_number+1] <- class_name
  }
  return (feature_matrix)
}

brush_teeth_all <- get_all_data(brush_teeth, "./HMP_Dataset/Brush_teeth/", "brush_teeth")
climb_stairs_all <- get_all_data(climb_stairs, "./HMP_Dataset/Climb_stairs/", "climb_stair")
comb_hair_all <- get_all_data(comb_hair, "./HMP_Dataset/Comb_hair/", "comb_hair")
descend_stair_all <- get_all_data(descend_stair, "./HMP_Dataset/Descend_stairs/", "descend_stair")
drink_glass_all <- get_all_data(drink_glass, "./HMP_Dataset/Drink_glass/", "drink_glass")
eat_meat_all <- get_all_data(eat_meat, "./HMP_Dataset/Eat_meat/", "eat_meat")
eat_soup_all <- get_all_data(eat_soup, "./HMP_Dataset/Eat_soup/", "eat_soup")
getup_bed_all <- get_all_data(getup_bed, "./HMP_Dataset/Getup_bed/", "getup_bed")
liedown_bed_all <- get_all_data(liedown_bed, "./HMP_Dataset/Liedown_bed/", "liedown_bed")
pour_water_all <- get_all_data(pour_water, "./HMP_Dataset/Pour_water/", "pour_water")
sitdown_chair_all <- get_all_data(sitdown_chair, "./HMP_Dataset/Sitdown_chair/", "sitdown_chair")
standup_chair_all <- get_all_data(standup_chair, "./HMP_Dataset/Standup_chair/", "standup_chair")
use_telephone_all <- get_all_data(use_telephone, "./HMP_Dataset/Use_telephone/", "use_telephone")
walk_all <- get_all_data(walk, "./HMP_Dataset/Walk/", "walk")
all_data <- rbind(brush_teeth_all, climb_stairs_all, comb_hair_all, #3
                  descend_stair_all, drink_glass_all, eat_meat_all, #3
                  eat_soup_all, getup_bed_all, liedown_bed_all, pour_water_all, #4
                  sitdown_chair_all, standup_chair_all, use_telephone_all, walk_all) #4

res.hk <- hkmeans(all_data[,1:(3*segment_length)], cluster_number, iter.max = 100)

all_data <- cbind(all_data, res.hk$cluster)

brush_teeth_mat <- build_feature_matrix(all_data, brush_teeth, "brush_teeth")
climb_stairs_mat <- build_feature_matrix(all_data, climb_stairs, "climb_stair")
comb_hair_mat <- build_feature_matrix(all_data, comb_hair, "comb_hair")
descend_stair_mat <- build_feature_matrix(all_data, descend_stair, "descend_stair")
drink_glass_mat <- build_feature_matrix(all_data, drink_glass, "drink_glass")
eat_meat_mat <- build_feature_matrix(all_data, eat_meat, "eat_meat")
eat_soup_mat <- build_feature_matrix(all_data, eat_soup, "eat_soup")
getup_bed_mat <- build_feature_matrix(all_data, getup_bed, "getup_bed")
liedown_bed_mat <- build_feature_matrix(all_data, liedown_bed, "liedown_bed")
pour_water_mat <- build_feature_matrix(all_data, pour_water, "pour_water")
sitdown_chair_mat <- build_feature_matrix(all_data, sitdown_chair, "sitdown_chair")
standup_chair_mat <- build_feature_matrix(all_data, standup_chair, "standup_chair")
use_telephone_mat <- build_feature_matrix(all_data, use_telephone, "use_telephone")
walk_mat <- build_feature_matrix(all_data, walk, "walk")

all_mat <- rbind(brush_teeth_mat, climb_stairs_mat, comb_hair_mat, descend_stair_mat, #4
                 drink_glass_mat, eat_meat_mat, eat_soup_mat, getup_bed_mat, liedown_bed_mat, #5
                 pour_water_mat, sitdown_chair_mat, standup_chair_mat, use_telephone_mat, walk_mat) #5

df <- data.frame(all_mat)

# write df to a csv file so that we can check
write.csv(df, file="feature_vector.csv", row.names=F)

# training and predicting
colnames(df)[cluster_number+1] <- "action"
ind <- createDataPartition(df$action, times=1, p=0.6, list=F)
train_data <- df[ind,]
test_data <- df[-ind,]

# check if all the input is valid
for (i in 1:cluster_number){
  len = length(levels(train_data[[sprintf("X%d", i)]]))
  if(len > 53)
    print(i)
}

model <- randomForest(action~., data=train_data)
pred <- predict(model, newdata=test_data)
table <- table(actual=test_data$action, predict=pred)
ratio <- sum(diag(table))/sum(table)
print(table)
print(ratio)

