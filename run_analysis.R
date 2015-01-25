##################################################################################################################
## Coursera Getting and Cleaning Data Project

## Author : Biswajit Mandal

## email : bjitfreelancing@gmail.com

## Date : 01/26/2015

## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
## The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. 
## You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, 
## and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. 
## You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.  
## One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
## Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. 
## The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. 
## A full description is available at the site where the data was obtained: 
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
## Here are the data for the project: 
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

######################################################################################################################
# Setting up workspace
#setwd("./Getting and Cleaning Data/Project");

# Cleaning workspace
rm(list=ls());

# Reading input files
features <- read.table('UCI HAR Dataset/features.txt',header=FALSE);
activityLabels <- read.table('UCI HAR Dataset/activity_labels.txt',header=FALSE);
subjectTrain <- read.table('UCI HAR Dataset/train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain <- read.table('UCI HAR Dataset/train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain <- read.table('UCI HAR Dataset/train/y_train.txt',header=FALSE); #imports y_train.txt
subjectTest = read.table('UCI HAR Dataset/test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('UCI HAR Dataset/test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('UCI HAR Dataset/test/y_test.txt',header=FALSE); #imports y_test.txt

# 1. Merges the training and the test sets to create one data set.

# Assigin column names
colnames(activityLabels)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

# Create the final training set
trgData = cbind(yTrain,subjectTrain,xTrain);

# Create the final test set
testData = cbind(yTest,subjectTest,xTest);


# Combine training and test data to create a final data set
finalData = rbind(trgData,testData);
View(finalData)

colNames  = colnames(finalData); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector for the same
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));

# Subset finalData table based on the logicalVector to keep only desired columns
finalData = finalData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData = merge(finalData,activityLabels,by='activityId',all.x=TRUE);

# Updating the colNames vector to include the new column names after merge
colNames  = colnames(finalData); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# Reassigning the new descriptive column names to the finalData set
colnames(finalData) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityLabel without the activityLabels column
finalDataNoActivityLbl  = finalData[,names(finalData) != 'activityLabels'];

# Summarizing the finalDataNoActivityLabel table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityLbl[,names(finalDataNoActivityLbl) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityLbl$activityId,subjectId = finalDataNoActivityLbl$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityLabels,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t');

