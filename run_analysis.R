library (dplyr)

# download zip file containing data if it hasn't already been downloaded
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

data_Path<-"C:\\Users\\giannetto_sa\\Documents\\data science\\GandC - Prj\\UCI HAR Dataset"
############################
############################
# 1st Step - Reading data ##                                                   
############################
############################

# training data
trainingSubjects <- read.table(file.path(data_Path, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(data_Path, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dat_aPath, "train", "y_train.txt"))

# test data
testSubjects <- read.table(file.path(data_Path, "test", "subject_test.txt"))
testValues <- read.table(file.path(data_Path, "test", "X_test.txt"))
testActivity <- read.table(file.path(data_Path, "test", "y_test.txt"))

# read features
# no convertion of text fields to factors
features <- read.table(file.path(data_Path, "features.txt"), as.is = TRUE)

# read activity labels
activities <- read.table(file.path(data_Path, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")
###########################################
###########################################
# 2nd Step - Merge training and test sets #
###########################################
###########################################
 
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assigning column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

##############################################################################
# 3rd Step - Extracting only mean and standard dev. for each measurement     #
##############################################################################

# calculating columns of data set to keep based on column name
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))

# getting data in these columns only
humanActivity <- humanActivity[, columnsToKeep]

####################################################################################
# 4th Step - Use descriptive activity names to name the activities in the data set #
####################################################################################
# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])

##############################################################################
# 5th Step - Appropriately label the data set with descriptive variable names
##############################################################################

# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

####################################################################################################
# 6th Step - Second tidy set with the average of each variable for each activity and each subject ##
####################################################################################################

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "C:\\Users\\giannetto_sa\\Documents\\data science\\GandC - Prj\\tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
