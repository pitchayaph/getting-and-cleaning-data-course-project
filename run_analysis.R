# OVERVIEW
# The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.
# result output is in a file named "tidy_data.txt".

library(dplyr)

# Get data

# download zip file 
Urlfile <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
Filename <- "UCI HAR Dataset.zip"

if (!file.exists(Filename)) {
  download.file(Urlfile, Filename, mode = "wb")
}

# unzip data file if not exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(Filename)
}

# Read data

# Training data
trainingSubj <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingVal <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingAct <- read.table(file.path(dataPath, "train", "y_train.txt"))

# Testing data
testSubj <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testVal <- read.table(file.path(dataPath, "test", "X_test.txt"))
testAct <- read.table(file.path(dataPath, "test", "y_test.txt"))

# read features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
 
# read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


# Step 1 - Merge data


# concatenate these 2 tables to make a single data table
humanAct <- rbind(
  cbind(trainingSubj, trainingVal, trainingAct),
  cbind(testSubj, testVal, testAct)
)

# remove the unuse tables
rm(trainingSubj, trainingVal, trainingAct, 
   testSubj, testVal, testAct)

# assign column names
colnames(humanAct) <- c("subject", features[, 2], "activity")


# Step 2 - Extract only the measurements on the mean and standard deviation
#          for each measurement

# pick columns
Keepcol <- grepl("subject|activity|mean|std", colnames(humanAct))
humanAct <- humanAct[, Keepcol]


# Step 3 - Use descriptive activity names to name the activities

# replace activity values with named factor levels
humanAct$activity <- factor(humanAct$activity, levels = activities[, 1], labels = activities[, 2])


# Step 4 - Appropriately label the data set with descriptive variable names

# get column names & remove unwanted characters
humanActCols <- colnames(humanAct)
humanActCols <- gsub("[\\(\\)-]", "", humanActCols)

# expand abbreviations and clean up names

humanActCols <- gsub("mean", "Mean", humanActCols)
humanActCols <- gsub("std", "StandardDeviation", humanActCols)
humanActCols <- gsub("Freq", "Frequency", humanActCols)
humanActCols <- gsub("^f", "frequencyDomain", humanActCols)
humanActCols <- gsub("^t", "timeDomain", humanActCols)
humanActCols <- gsub("Gyro", "Gyroscope", humanActCols)
humanActCols <- gsub("Mag", "Magnitude", humanActCols)
humanActCols <- gsub("Acc", "Accelerometer", humanActCols)

humanActCols <- gsub("BodyBody", "Body", humanActCols)

# use new labels as column names
colnames(humanAct) <- humanActCols


# Step 5 - Create a second tidy set with the avg of each variable for each activity and each subject

# group by subject and activity and summarise using mean
humanMeans <- humanAct %>% group_by(subject, activity) %>% summarise_each(list(mean))

# output to file "tidy_data.txt"
write.table(humanMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)
			
			
			
################Done Yay!####################