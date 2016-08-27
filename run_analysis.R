# Set working directory that stores the Samsung data "UCI HAR Dataset"
# Sourcing this code creates data frames of step_1 to step_4
## step_1: a data set containing whole data from the training and the test sets 
## step_2: the mean and standard deviation for each measurement. 
## step_3: step_2 with descriptive activity names.
## step_4: the average of each variable for each activity and each subject

makeTable <- function(){
library(dplyr)
setwd("./UCI HAR Dataset")
features<-read.csv("features.txt", sep = " ", header=F, 
                       colClasses = "character")
features[,3] <- gsub("\\()", "", features[,2]) # remove ()
features[,4] <- gsub(",|-", "_", features[,3]) # replace ,|- by _
features[,5] <- gsub("\\)", "", features[,4]) # replace ) by ""
features <- features[,5]

AllData<- data.frame()
for (dataset in c("./test", "./train")){
  setwd(dataset)
  file2read<- grep("subject", dir(), value = T)    
  subj <- readLines(file2read)   # 2947
  file2read<- grep("X_", dir(), value = T)
  X<- readLines(file2read) %>% strsplit(" ") %>% unlist  # 197776 entries 
  file2read<- grep("y_", dir(), value = T)
  y<- readLines(file2read) %>% strsplit(" ")  %>% unlist# 2947 entries 
  subj.y<- cbind(subj,y); 
  X<-X[X!=""] #2947 x 561
  X<- matrix(X,  ncol = 561, byrow = TRUE) 
  subj.y.X<- cbind(subj.y, X); 
  colnames(subj.y.X)<-c("subject", "activity_label", features)
  subj.y.X <- data.frame(subj.y.X); dim(subj.y.X)
  subj.y.X[, 3:563] <- lapply(subj.y.X[, 3:563], as.character)
  subj.y.X[, 3:563] <- lapply(subj.y.X[, 3:563], as.numeric)

  setwd("./Inertial Signals")
  ##body_acc_x
  file2read<- grep("body_acc_x", dir(), value = T)
  body_acc_x <- readLines(file2read)
  body_acc_x <- body_acc_x %>% strsplit(" ") %>% unlist #2947 element
  body_acc_x <- body_acc_x[body_acc_x !=""]
  body_acc_x <- data.frame(matrix(body_acc_x, ncol=128, byrow = T))
  Header <- paste0("body_acc_x_", 1:128)
  colnames(body_acc_x) <- Header
  body_acc_x[] <- lapply(body_acc_x, as.character)
  body_acc_x[] <- lapply(body_acc_x, as.numeric)

  ##body_acc_y
  file2read<- grep("body_acc_y", dir(), value = T)
  body_acc_y <- readLines(file2read)
  body_acc_y <- body_acc_y %>% strsplit(" ") %>% unlist #2947 element
  body_acc_y <- body_acc_y[body_acc_y !=""]
  body_acc_y <- data.frame(matrix(body_acc_y, ncol=128, byrow = T))
  Header <- paste0("body_acc_y_", 1:128)
  colnames(body_acc_y) <- Header
  body_acc_y[] <- lapply(body_acc_y, as.character)
  body_acc_y[] <- lapply(body_acc_y, as.numeric)

  ##body_acc_z
  file2read<- grep("body_acc_z", dir(), value = T)
  body_acc_z <- readLines(file2read)
  body_acc_z <- body_acc_z %>% strsplit(" ") %>% unlist #2947 element
  body_acc_z <- body_acc_z[body_acc_z !=""]
  body_acc_z <- data.frame(matrix(body_acc_z, ncol=128, byrow = T))
  Header <- paste0("body_acc_z_", 1:128)
  colnames(body_acc_z) <- Header
  body_acc_z[] <- lapply(body_acc_z, as.character)
  body_acc_z[] <- lapply(body_acc_z, as.numeric)

  ##body_gyro_x
  file2read<- grep("body_gyro_x", dir(), value = T)
  body_gyro_x <- readLines(file2read)
  body_gyro_x <- body_gyro_x %>% strsplit(" ") %>% unlist #2947 element
  body_gyro_x <- body_gyro_x[body_gyro_x !=""]
  body_gyro_x <- data.frame(matrix(body_gyro_x, ncol=128, byrow = T))
  Header <- paste0("body_gyro_x_", 1:128)
  colnames(body_gyro_x) <- Header
  body_gyro_x[] <- lapply(body_gyro_x, as.character)
  body_gyro_x[] <- lapply(body_gyro_x, as.numeric)

  ##body_gyro_y
  file2read<- grep("body_gyro_y", dir(), value = T)
  body_gyro_y <- readLines(file2read)
  body_gyro_y <- body_gyro_y %>% strsplit(" ") %>% unlist #2947 element
  body_gyro_y <- body_gyro_y[body_gyro_y !=""]
  body_gyro_y <- data.frame(matrix(body_gyro_y,ncol=128, byrow = T))
  Header <- paste0("body_gyro_y_", 1:128)
  colnames(body_gyro_y) <- Header
  body_gyro_y[] <- lapply(body_gyro_y, as.character)
  body_gyro_y[] <- lapply(body_gyro_y, as.numeric)

  ##body_gyro_z
  file2read<- grep("body_gyro_z", dir(), value = T)
  body_gyro_z <- readLines(file2read)
  body_gyro_z <- body_gyro_z %>% strsplit(" ") %>% unlist #2947 element
  body_gyro_z <- body_gyro_z[body_gyro_z !=""]
  body_gyro_z <- data.frame(matrix(body_gyro_z, ncol=128, byrow = T))
  Header <- paste0("body_gyro_z_", 1:128)
  colnames(body_gyro_z) <- Header
  body_gyro_z[] <- lapply(body_gyro_z, as.character)
  body_gyro_z[] <- lapply(body_gyro_z, as.numeric)

  ##total_acc_x
  file2read<- grep("total_acc_x", dir(), value = T)
  total_acc_x <- readLines(file2read)
  total_acc_x <- total_acc_x %>% strsplit(" ") %>% unlist #2947 element
  total_acc_x <- total_acc_x[total_acc_x !=""]
  total_acc_x <- data.frame(matrix(total_acc_x, ncol=128, byrow = T))
  Header <- paste0("total_acc_x_", 1:128)
  colnames(total_acc_x) <- Header
  total_acc_x[] <- lapply(total_acc_x, as.character)
  total_acc_x[] <- lapply(total_acc_x, as.numeric)

  ##total_acc_y
  file2read<- grep("total_acc_y", dir(), value = T)
  total_acc_y <- readLines(file2read)
  total_acc_y <- total_acc_y %>% strsplit(" ") %>% unlist #2947 element
  total_acc_y <- total_acc_y[total_acc_y !=""]
  total_acc_y <- data.frame(matrix(total_acc_y, ncol=128, byrow = T))
  Header <- paste0("total_acc_y_", 1:128)
  colnames(total_acc_y) <- Header
  total_acc_y[] <- lapply(total_acc_y, as.character)
  total_acc_y[] <- lapply(total_acc_y, as.numeric)

  ##total_acc_z
  file2read<- grep("total_acc_z", dir(), value = T)
  total_acc_z <- readLines(file2read)
  total_acc_z <- total_acc_z %>% strsplit(" ") %>% unlist #2947 element
  total_acc_z <- total_acc_z[total_acc_z !=""]
  total_acc_z <- data.frame(matrix(total_acc_z, ncol=128, byrow = T))
  Header <- paste0("total_acc_z_", 1:128)
  colnames(total_acc_z) <- Header
  total_acc_z[] <- lapply(total_acc_z, as.character)
  total_acc_z[] <- lapply(total_acc_z, as.numeric)
  ##combine data frames
  Table<- cbind(subj.y.X, body_acc_x, body_acc_y, body_acc_z,
                body_gyro_x, body_gyro_y, body_gyro_z,
                total_acc_x,total_acc_y, total_acc_z)
  setwd("../"); setwd("../")
  AllData <- rbind(AllData, Table)}
AllData[ ,1:2] <- lapply(AllData[ ,1:2], as.character)
AllData<- AllData %>% arrange(subject, activity_label)
AllData}

step_1 <- makeTable()
## Extract colum containing mean or std variables
extract_mean_std<- function(){
    extract <- step_1 %>% select(subject, 
               activity_label, contains("_mean_"),contains("_std_"))
    extract}
step_2 <- extract_mean_std()

decriptive_activity_label<- function(){
### read activity name
activity<- readLines("activity_labels.txt") %>% strsplit(" ")
### replace wutg descriptive avtivity labels
for (i in 1:6){
step_2$activity_label[step_2$activity_label == activity[[i]][1]]<-
    activity[[i]][2]}
#### as factor
step_2$activity_label<- factor(step_2$activity_label, levels = c(
    activity[[1]][2],activity[[2]][2],activity[[3]][2],activity[[4]][2],
    activity[[5]][2],activity[[6]][2]))
step_2
}
step_3 <- decriptive_activity_label()

tidy_data <- function(){
### mean then std colums in this order
col_nm_mean_std<-grep("_mean_|_std_", colnames(step_3), value = F)
## calculate average of mean and std, for each subject and each activity
tidy<- aggregate(step_3[, col_nm_mean_std], 
                 list(step_3$subject, step_3$activity_label), mean) 
tidy<- tidy %>% arrange(Group.1)
### rename colums
colnames(tidy) <-c("subject", "activity_label", 
                   grep("_mean_|_std_", colnames(step_3), value = T))
colnames(tidy) <- gsub("mean", "average", colnames(tidy) )
colnames(tidy) <- gsub("std", "average_std", colnames(tidy) )
tidy
}
tidy_data_set <- tidy_data()
rm(step_1, step_2, step_3, decriptive_activity_label, extract_mean_std,
   makeTable, tidy_data)
setwd("../")