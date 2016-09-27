## Getting and Cleaning Data Course Project
## To avoid being prompted to restart your RStudio session when loading 
## packages, begin with a new RStudio session.
##
## The purpose of this project is to demonstrate the ability to collect,
## work with, and clean a data set. The goal is to prepare tidy data that
## can be used for later analysis.
##
## The datasets being used are taken from the Human Activity Recognition
## Using Smartphones project. A full description of the datasets is available
## at: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+
## Using+Smartphones.
##
## Guidance for this project was taken from the discussion forum post by David 
## Hood: https://thoughfulbloke.wordpress.com/2015/09/09/getting-and-cleaning-
## the-assignment/.

## Begin by creating a directory to hold the data. 
if(!file.exists("data")){
    dir.create("data")
}

## Download the dataset folder and unzip the files. Record the current date.
filename <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(filename, "./data/data.zip", method="curl")
unzip("./data/data.zip", exdir="./data")
datedownloaded <- date()
datedownloaded

## Load all of the required data into R and label the columns. Note, the
## raw inertial signals are not required for this assignment and are
## therefore not loaded.

activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", 
                             header = FALSE)
names(activityLabels) <- c("activityId", "activityName")
features <- read.table("./data/UCI HAR Dataset/features.txt", header = FALSE)
names(features) <- c("featureId", "featureName")
testSubjects <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", 
                             header = FALSE)
names(testSubjects) <- "subjectId"

testX <- read.table("./data/UCI HAR Dataset/test/X_test.txt", header = FALSE)
names(testX) <- features$featureName
testY <- read.table("./data/UCI HAR Dataset/test/y_test.txt", header = FALSE)
names(testY) <- "activityId"
 
trainSubjects <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", 
                          header = FALSE)
names(trainSubjects) <- "subjectId"
 
trainX <- read.table("./data/UCI HAR Dataset/train/X_train.txt", header = FALSE)
names(trainX) <- features$featureName
trainY <- read.table("./data/UCI HAR Dataset/train/y_train.txt", header = FALSE)
names(trainY) <- "activityId"

## Clip all of the data for each group together using column binding
testData <- cbind(testSubjects, testY, testX)
trainData <- cbind(trainSubjects, trainY, trainX)

## Combine the two groups into one dataset using row binding and eliminate
## all columns except subjectId, activityId, and those columns associated 
## with mean or std. Since the guidance was somewhat ambiguous, I chose to
## include all columns that contained mean or std, including meanFreq.
combinedData <- rbind(testData, trainData)
colsToKeep <- c("subjectId", "activityId", "mean", "std")
combinedData <- combinedData[,grepl(paste(colsToKeep, collapse="|"), 
                                    colnames(combinedData))]

## Clean up the column names by removing the hyphens and parens and 
## capitalizing mean and std.
names(combinedData) <- gsub(("-|\\(|\\)"), "", names(combinedData))
names(combinedData) <- gsub("mean", "Mean", names(combinedData))
names(combinedData) <- gsub("std", "Std", names(combinedData))

## Add the activity names to the dataset by merging on the intersect - which
## is the default for merge - then reorder the columns to place the activity
## name at the beginning of the data frame so it is easy to see. 
combinedData <- merge(combinedData, activityLabels)
combinedData <- combinedData[c(2, 82, 3:81)]
View(combinedData)

## Install required packages to reshape the data. If you did not begin in
## a new RStudio session, you may be prompted to restart r and save the 
## environment in order to update loaded packages. Answer 'Yes' if prompted. 
## You may also receive a warning message regarding the version of r plyr was 
## built under. This warning can be ignored.
install.packages("reshape2")
install.packages("plyr")
library(reshape2)
library(plyr)

## Melt the data into a long, narrow dataset. This is the long form as 
## mentioned in the rubric as either long or wide form is acceptable.
meltedData <- melt(combinedData, id=c("subjectId", "activityName"))
View(meltedData)
names(meltedData) <- c("subjectId", "activityName", "featureVariable", "value")

## Finally, recast the data to produce an independent tidy dataset with
## the average of each variable for each activity and each subject.
meanData <- ddply(meltedData, c("subjectId", "activityName", "featureVariable"), 
                  summarise, mean = mean(value))
View(meanData)

## And save the new tidy dataset to a file.
write.table(meanData, "./data/means_data.txt", row.names=FALSE)
