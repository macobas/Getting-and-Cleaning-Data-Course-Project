############################################################
#  Getting and Cleaning Data - Course Project
#  Author: Your Name
#  Script: run_analysis.R
#  Description: Creates a tidy dataset from UCI HAR data
############################################################

# Load required package
library(dplyr)

# 1. Download and unzip dataset if not already done
filename <- "dataset.zip"
dataset_folder <- "UCI HAR Dataset"

if (!file.exists(dataset_folder)) {
        if (!file.exists(filename)) {
                url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
                download.file(url, destfile = filename, method = "curl")
        }
        unzip(filename)
}

# 2. Read data
features <- read.table(file.path(dataset_folder, "features.txt"), col.names = c("index", "feature"))
activity_labels <- read.table(file.path(dataset_folder, "activity_labels.txt"), col.names = c("code", "activity"))

# Training data
subject_train <- read.table(file.path(dataset_folder, "train", "subject_train.txt"), col.names = "subject")
x_train <- read.table(file.path(dataset_folder, "train", "X_train.txt"), col.names = features$feature)
y_train <- read.table(file.path(dataset_folder, "train", "y_train.txt"), col.names = "code")

# Test data
subject_test <- read.table(file.path(dataset_folder, "test", "subject_test.txt"), col.names = "subject")
x_test <- read.table(file.path(dataset_folder, "test", "X_test.txt"), col.names = features$feature)
y_test <- read.table(file.path(dataset_folder, "test", "y_test.txt"), col.names = "code")

# 3. Merge training and test sets
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
merged_data <- cbind(Subject, Y, X)

# 4. Extract only measurements on the mean and standard deviation
tidy_data <- merged_data %>%
        select(subject, code, contains("mean"), contains("std"))

# 5. Use descriptive activity names
tidy_data$code <- activity_labels[tidy_data$code, 2]

# 6. Appropriately label dataset with descriptive variable names
names(tidy_data)[2] <- "activity"

names(tidy_data) <- gsub("^t", "Time", names(tidy_data))
names(tidy_data) <- gsub("^f", "Frequency", names(tidy_data))
names(tidy_data) <- gsub("Acc", "Accelerometer", names(tidy_data))
names(tidy_data) <- gsub("Gyro", "Gyroscope", names(tidy_data))
names(tidy_data) <- gsub("Mag", "Magnitude", names(tidy_data))
names(tidy_data) <- gsub("BodyBody", "Body", names(tidy_data))
names(tidy_data) <- gsub("-mean\\(\\)", "Mean", names(tidy_data))
names(tidy_data) <- gsub("-std\\(\\)", "Std", names(tidy_data))
names(tidy_data) <- gsub("[-()]", "", names(tidy_data))

# 7. Create independent tidy dataset with average of each variable for each subject and activity
final_data <- tidy_data %>%
        group_by(subject, activity) %>%
        summarise_all(mean)

# 8. Write output to file
write.table(final_data, "tidy_data.txt", row.name = FALSE)

cat("Tidy dataset created successfully: tidy_data.txt\n")

