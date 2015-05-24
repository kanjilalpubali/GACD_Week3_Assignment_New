#Set Working Directory
setwd("F:/PK/Coursera - Data Science Specialization/Course 3 - Getting and Cleaning Data/Week 3/GACD Week3 Assignment")

#All datasets related to train, test, activity, label have been saved in the Working Directory

#Question1: Merges the training and the test sets to create one data set

#Read Different Datasets

train <- read.table("X_train.txt")          #Read Train Dataset
test <- read.table("X_test.txt")            #Read Test Dataset

train_labels <- read.table("y_train.txt")   #Read Train Labels Dataset
test_labels <- read.table("y_test.txt")     #Read Test Labels Dataset

train_subjects <- read.table("subject_train.txt") #Read Train Subjects Dataset
test_subjects <- read.table("subject_test.txt")   #Read Test Subjects Dataset

activity_labels <- read.table("activity_labels.txt") #Read Activity Labels Dataset 
features <- read.table("features.txt")      #Read Features Dataset

#Keep Only 2nd Column from Features Dataset
features_new <- features[,2]

#Name Train & Test Datasets using Features Data
names(train) <- features_new
names(test)  <- features_new

#Name Train & Test Subject Datasets
names(train_subjects) <-"Subjects"
names(test_subjects) <-"Subjects"

#Name Train & Test Labels Datasets
names(train_labels) <-"Activity"
names(test_labels) <-"Activity"

#Create Train Dataset with All Required Variables
train1 <- cbind(train_subjects,train_labels,train)
dim(train1) 
#[1] 7352  563

#Create Test Dataset with All Required Variables
test1 <- cbind(test_subjects,test_labels,test)
dim(test1) 
#[1] 2947  563

#Stack Train & Test Datasets to Create Final Dataset with All Required Variables
test_train_all <- rbind(train1,test1)

dim(test_train_all) 
#[1] 10299   563
head(test_train_all,3)

#Question2: Extracts only the measurements on the mean and standard deviation for each measurement.

#Create New Dataset with Only Variables related Mean or SD
test_train_reqd <- test_train_all[,c(1,2,grep("mean|std",tolower(names(test_train_all))))]

dim(test_train_reqd)
#[1] 10299    88
head(test_train_reqd,3)

#Question3: Uses descriptive activity names to name the activities in the data set

#Create Column Names for Activity Labels Dataset
names(activity_labels)<-c("Activity","Activity Description")

#Merge Activity Description with Main Dataset using Activity as Merge ID Variable
test_train_reqd2 <- merge(x=test_train_reqd, y=activity_labels, by="Activity", all.y = TRUE)

dim(test_train_reqd2)
#[1] 10299    89
head(test_train_reqd2,3)

#Question4: Appropriately labels the data set with descriptive variable names

#Initialize reshape2 Library for Using Melt Functions
library(reshape2)

#Melt Data
test_train_reqd3 <- melt(test_train_reqd2,
                         id=c("Subjects","Activity Description"),
                         measure.vars=c(names(test_train_reqd2[,3:(ncol(test_train_reqd2)-1)])))

#Create Appropriate Labels
names(test_train_reqd3)<-c("Subjects","ActivityDescription","Features","Measurements")

#Checks
test_train_reqd4 <- test_train_reqd3[order(test_train_reqd3$Subjects,test_train_reqd3$ActivityDescription,test_train_reqd3$Features),]
head(test_train_reqd4,3)

#Question5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject

#Initialize Library plyr
library(plyr)

#Create Tidy Data with Average Measurement of Each Feature/Activity/Subject
Tidy_Data <- ddply(test_train_reqd4, names(test_train_reqd4)[1:3], summarize, Average=mean(Measurements))

#Checks
dim(Tidy_Data)
#[1] 15480     4
head(Tidy_Data)

#Write Table
write.table(Tidy_Data, "./Tidy_Data.txt", sep="\t",row.name=FALSE)