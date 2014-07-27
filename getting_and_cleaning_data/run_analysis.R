#set working directory
setwd("C:/Users/Gabriel/SkyDrive/coursera/DataScienceTrack/GettingandCleaningData/week3/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")

#open training and test sets
training <- read.table("./train/X_train.txt")
test <- read.table("./test/X_test.txt")
#open the activity numbers
y_train <- read.table("./train/y_train.txt")
y_test <- read.table("./test/y_test.txt")
#open variable and activity names
variables <- read.table("features.txt")
activities <- read.table("activity_labels.txt")


#add the y-hats/class variables/activity indicators to the data
training$activity <- y_train$V1
test$activity <- y_test$V1

########################
#######################
#Merges the training and the test sets to create one data set.
#actually they will be appended because the variables are the same.
######################
######################
data.merged <- rbind(test,training)

#remove old variables to keep things from getting cluttered up
rm(training, test, y_test, y_train)


########################
#######################
#Appropriately labels the data set with descriptive variable names. 
#yes, i'm doing step 4 first b/c it will make getting the mean and stddev easier
######################
######################
#both columns and variable names are in order so they can just be added
#fyi: I took out the activityname b/c it already has a descriptive name
names(data.merged)[1:ncol(data.merged)-1] <- as.character(variables$V2)

#sanity check... now we can easily look up these variable in the features_info.txt file
names(data.merged)


########################
#######################
#Extracts only the measurements on the mean and standard 
########################
#######################
#use grep to extract the correct variables
data.merged <- data.merged[,grep("mean\\(\\)|std\\(\\)|activity",names(data.merged))]

#sanity check
names(data.merged)

########################
#######################
#Uses descriptive activity names to name the activities in the data set
######################
######################
data.merged$activityname <- activities$V2[match(x = data.merged$activity, table = activities$V1)]

#drop the old name
data.merged$activity <- NULL


##################
##################
#Creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject. 
####################
####################

write.table(data.merged,"tidy_data.txt",row.names = F)