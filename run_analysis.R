#Getting and Cleaning Data Course Project

#########################################################################
#1. Merges the training and the test sets to create one data set

#read data
testData <- read.table("./data/test/X_test.txt")
trainData <- read.table("./data/train/X_train.txt")
#check data dimension
dim(testData) #2947 x 561
dim(trainData) #7352 x 561

#read data subject
testSubject <- read.table("./data/test/subject_test.txt")
trainSubject <- read.table("./data/train/subject_train.txt")
#check subject dimension
dim(testSubject) # 2947 x 1
dim(trainSubject) # 7352 x 1

#read data label
testLabel <- read.table("./data/test/Y_test.txt")
trainLabel <- read.table("./data/train/Y_train.txt")
#check label dimension
dim(testLabel) # 2947 x 1
dim(trainLabel) #7352 x 1

#merge train and test data
mergeData <- rbind(testData, trainData)
dim(mergeData) #10299 x 561
#merge train and test subject
mergeSubject <- rbind(testSubject, trainSubject)
dim(mergeSubject) # 10299 x 1
#merge train and test label
mergeLabel <- rbind(testLabel, trainLabel)
dim(mergeLabel) # 10299 x 1

#########################################################################
#2. Extracts only the measurements on the mean and standard deviation 
#   for each measurement

#read features
features <- read.table("./data/features.txt")
dim(features) #561 x 2
names(features)

#grab index for measurement on mean -> mean() and std deviation -> std() only
meanStd <- grep("mean\\(\\)|std\\(\\)", features[,2])
length(meanStd) #66 out of 561 features

#subset for mean and std deviation 
mergeData <- mergeData[,meanStd]
dim(mergeData) #10299 x 66

#assign features name to data set
#remove "()" from names
#change "-" to "_"
names(mergeData) <- gsub("\\(\\)", "", features[meanStd, 2])
names(mergeData) <- gsub("-", "_", names(mergeData))
names(mergeData)

#########################################################################
#3. Uses descriptive activity names to name the activities in the data set

#read activities
activities <- read.table("./data/activity_labels.txt")
activities[,2] <- tolower(activities[,2]) #reformat activities name to lower case
activities
#assign activity names to data set
activityLabel <- activities[mergeLabel[,1],2]
mergeLabel[,1] <- activityLabel
names(mergeLabel) <- "activity"
head(mergeLabel)
names(mergeLabel)

#########################################################################
#4. Appropriately labels the data set with descriptive variable names

#merge subject, label and data into single dataset
names(mergeSubject) <- "subject"
cleanData <- cbind(mergeSubject, mergeLabel, mergeData)
names(cleanData)
dim(cleanData) # 10299 x 68

#write out the clean dataset
write.table(cleanData, "./data/cleanData.txt")

#########################################################################
#5. Creates a second, independent tidy data set with the 
#   average of each variable for each activity and each subject

library(dplyr)
#groupby data by subject and activity
groupData <- group_by(cleanData, subject, activity ) 
summarise(groupData, n=n())

#compute mean for each variable by group(activity, subject)
meanData <- summarise_each(groupData, funs(mean))
dim(meanData) # 180 x 68

# write out the dataset
write.table(meanData, "./data/meanData.txt")



