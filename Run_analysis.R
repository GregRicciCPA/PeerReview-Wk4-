# Getting and Cleaning Data Course Project
# GR - April 11, 2019
# file: run_analysis.R

# Load the dplyr library
# library(data.table)
# library(devtools)
# library(codebook)
library(dplyr)

# Download the UCI dataset in txt format, Load into R
# Read data - x test, y test, subject test
# Read in the data and identify column names
features <- read.table("./features.txt", header=FALSE, col.names = c("n","functions"))
activities <- read.table("./activity_labels.txt", header=FALSE, col.names = c("code", "activity"))
subject_test <- read.table("./subject_test.txt", header=FALSE, col.names = "subject")
x_test <- read.table("./X_test.txt", header = FALSE, col.names = features$functions)
y_test <- read.table("./y_test.txt", header=FALSE, col.names = "code")
subject_train <- read.table("./subject_train.txt", header=FALSE, col.names = "subject")
x_train <- read.table("./X_train.txt", header=FALSE, col.names = features$functions)
y_train <- read.table("./y_train.txt", header=FALSE, col.names = "code")

# Merge the datasets into a single dataframe called testdf
# Combine testdf and traindf into running_data
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, Y, X)

# Apply the measurement labels as column names running_data
TidyData <- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))

# create column names
TidyData$code <- activities[TidyData$code, 2]

# Remove parentheses, hyphens and bad stuff from column names, fix 
# repeating word(s) in columns, create descriptive variable names
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

# Group the running data by subject and activity, then
# calculate the mean of every measurement
FinalData <- TidyData %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean))

# Write running_data to text file and data checks
write.table(FinalData, "FinalData.txt", row.name=FALSE)
Str(FinalData)
Summarise(FinalData)

