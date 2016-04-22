## Download the data file
filesPath <- getwd()
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

###Unzip DataSet to the data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

## 1. Merges the training and the test sets to create one data set.

## Read test files
testx <- read.table(file.path(filesPath, "test", "X_test.txt"))
testy <- read.table(file.path(filesPath, "test", "y_test.txt"))
testsub <- read.table(file.path(filesPath, "test", "subject_test.txt"))

## Read train files
trainsub <- read.table(file.path(filesPath, "train", "subject_train.txt"))
trainx <- read.table(file.path(filesPath, "train", "X_train.txt"))
trainy <- read.table(file.path(filesPath, "train", "y_train.txt"))

## Merging to get the data table for all data
testdat <- cbind(testsub, testy, testx)
traindat <- cbind(trainsub, trainy, trainx)
totaldat <- rbind(testdat,traindat)

## Set names to variables
featurenum <- read.table(file.path(filesPath,"features.txt"))
names(totaldat) <- c("subjectNum","activityNum",as.character(featurenum$V2))

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## Get the sequence of rows whose variable name includes mean or standard deviation.
returncol <- grep("mean\\(\\)|std\\(\\)", featurenum$V2)
## Extract corresponded rows
totaldat2 <- totaldat[, c(1,2,returncol+2)]

## 3. Uses descriptive activity names to name the activities in the data set.

actnum <- read.table(file.path(filesPath, "activity_labels.txt"))
names(actnum) <- c("actNo","activityName")
## Merge data with activity label which is a primary key
mergedat <- merge(totaldat2, actnum, by.x = "activityNum", by.y = "actNo", all=T)
mergedat <- mergedat[,c(69,2:68)]

## 4. Appropriately labels the data set with descriptive variable names.

## Plan for descriptive labeling
## 1) prefix 't' should be replaced by 'time'
## 2) 'Acc' should be replaced by 'Accelerometer'
## 3) 'Gyro' should be replaced by 'Gyroscope'
## 4) prefix 'f' should be replaced by 'frequency'
## 5) 'Mag' should be replaced by 'Magnitude'
## 6) 'BodyBody' should be replaced by 'Body'

names(mergedat) <- gsub("^t", "time", names(mergedat))
names(mergedat) <- gsub("^f", "frequency", names(mergedat))
names(mergedat) <- gsub("Acc", "Accelerometer", names(mergedat))
names(mergedat) <- gsub("Gyro", "Gyroscope", names(mergedat))
names(mergedat) <- gsub("Mag", "Magnitude", names(mergedat))
names(mergedat) <- gsub("BodyBody", "Body", names(mergedat))

## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

library(dplyr)
tidy <- aggregate(. ~subjectNum + activityName, mergedat, mean)
tidy <- tidy[order(tidy$subjectNum, tidy$activityName),]
write.table(tidy, file = "tidydata.txt", row.name=FALSE)
