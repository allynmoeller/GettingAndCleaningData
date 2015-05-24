# Author: amoeller
###############################################################################

#You will be required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, and 
#3) a code book that describes the variables, the data, and any transformations or work that you performed 
#to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
#This repo explains how all of the scripts work and how they are connected. 

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#You should create one R script called run_analysis.R that does the following. 
#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
##############################

##### data prep #####
#set folder names and create if they do not exist
datadir <- "UCI HAR Dataset"
resultsdir <- "results"
if(!file.exists(resultsdir)){
  dir.create(resultsdir)
} 

#read file convert to data.frame
getfiledata <- function (filename,cols = NULL){
  f <- paste(datadir,filename,sep="/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(f,sep="",stringsAsFactors=F)
  } else {
    data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
  }
  data
}

#run and check getfiledata
features <- getfiledata("features.txt")

#read data and build database
getdata <- function(type, features){
  subject <- getfiledata(paste(type,"/","subject_",type,".txt",sep=""),"id")
  y <- getfiledata(paste(type,"/","y_",type,".txt",sep=""),"activity")
  x <- getfiledata(paste(type,"/","X_",type,".txt",sep=""),features$V2)
  return (cbind(subject,y,x))
}

#run and check getdata
test <- getdata("test", features)
train <- getdata("train", features)

#save the resulting data in specified folder
saveresults <- function (data,name){
  print(paste("saving results", name))
  file <- paste(resultsdir, "/", name,".csv" ,sep="")
  write.csv(data,file)
}

##### requirements #####
#1) Merges the training and the test sets to create one data set.
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
mean_and_std <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresults(mean_and_std,"mean_and_std")
#3) Uses descriptive activity names from activities file to name the activities in the data set
activity_labels <- getfiledata("activity_labels.txt")
#4) Appropriately labels the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidy_dataset <- ddply(mean_and_std, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidy_dataset)[-c(1:2)] <- paste(colnames(tidy_dataset)[-c(1:2)], "_mean", sep="")
saveresults(tidy_dataset,"tidy_dataset")
