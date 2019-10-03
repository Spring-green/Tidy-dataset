#Getting and Cleaning Data Course Project
#Date: Oct 2, 2019

library(dplyr)


# Downloading the data and Checking if the data directory exist
if (!file.exists(filename)){
  fileURL <- 'http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
  download.file(fileURL, "Coursera_DS3_Final.zip", method="auto")
}  


if (!file.exists("UCI HAR Dataset")) { 
  unzip("Coursera_DS3_Final.zip") 
}

### Reading data into R and assign each data to variables

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")



### Creating run_analysis.R

### 1. Merging the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
XY_merge <- cbind(Subject, Y, X)

### 2. Extracting only the measurements on the mean and standard deviation for each measurement.
TidyData <- XY_merge %>%
            select(subject, code, contains("mean"), contains("std"))

### 3. Using descriptive activity names to name the activities in the data set
TidyData$code <- activities[TidyData$code, 2]


### 4. Creating appropriately labels the data set with descriptive variable names.
names(TidyData)[2] = "activity"
names(TidyData)<-gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData)<-gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData)<-gsub("BodyBody", "Body", names(TidyData))
names(TidyData)<-gsub("Mag", "Magnitude", names(TidyData))
names(TidyData)<-gsub("^t", "Time", names(TidyData))
names(TidyData)<-gsub("^f", "Frequency", names(TidyData))
names(TidyData)<-gsub("tBody", "TimeBody", names(TidyData))
names(TidyData)<-gsub("-mean()", "Mean", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-std()", "STD", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("-freq()", "Frequency", names(TidyData), ignore.case = TRUE)
names(TidyData)<-gsub("angle", "Angle", names(TidyData))
names(TidyData)<-gsub("gravity", "Gravity", names(TidyData))


### 5. creating a second, independent tidy data set with the average of each variable for each activity and each subject from step4
MyData<- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(MyData, "MyData.txt", row.name=FALSE)



