#run_analysis.R
#Samer Carcar  2/15/2015

library(dplyr)

# read the test sets
#test_body_acc_x <- read.table("test/Inertial Signals/body_acc_x_test.txt")
#test_body_acc_y <- read.table("test/Inertial Signals/body_acc_y_test.txt")
#test_body_acc_z <- read.table("test/Inertial Signals/body_acc_z_test.txt")
#test_body_gyro_x <- read.table("test/Inertial Signals/body_gyro_x_test.txt")
#test_body_gyro_y <- read.table("test/Inertial Signals/body_gyro_y_test.txt")
#test_body_gyro_z <- read.table("test/Inertial Signals/body_gyro_z_test.txt")
#test_total_acc_x <- read.table("test/Inertial Signals/total_acc_x_test.txt")
#test_total_acc_y <- read.table("test/Inertial Signals/total_acc_x_test.txt")
#test_total_acc_z <- read.table("test/Inertial Signals/total_acc_x_test.txt")

test_subject<-read.table("test/subject_test.txt")
test_X<-read.table("test/X_test.txt")
test_y<-read.table("test/y_test.txt")

# read the training sets
#train_body_acc_x <- read.table("train/Inertial Signals/body_acc_x_train.txt")
#train_body_acc_y <- read.table("train/Inertial Signals/body_acc_y_train.txt")
#train_body_acc_z <- read.table("train/Inertial Signals/body_acc_z_train.txt")
#train_body_gyro_x <- read.table("train/Inertial Signals/body_gyro_x_train.txt")
#train_body_gyro_y <- read.table("train/Inertial Signals/body_gyro_y_train.txt")
#train_body_gyro_z <- read.table("train/Inertial Signals/body_gyro_z_train.txt")
#train_total_acc_x <- read.table("train/Inertial Signals/total_acc_x_train.txt")
#train_total_acc_y <- read.table("train/Inertial Signals/total_acc_y_train.txt")
#train_total_acc_z <- read.table("train/Inertial Signals/total_acc_z_train.txt")

train_subject<-read.table("train/subject_train.txt")
train_X<-read.table("train/X_train.txt")
train_y<-read.table("train/y_train.txt")

#combine test data: subject, y_test, X_test
test_data<- cbind(test_subject, test_y, test_X)

#combine train data
train_data<- cbind(train_subject, train_y, train_X)

#create column headings
features<- read.table("features.txt")
features2<- as.character(features[,2])
col_headings<- c("subject","activity",features2)

#rename headings for test_data and train_data to col_headings
names(test_data)<-col_headings
names(train_data)<-col_headings

#merge the training and test sets to generate 1 data set
data<-rbind(test_data, train_data)

#extract mean() and std() from the set
#columns to extract
cols_extract<-grep("subject|activity|mean\\(\\)|std\\(\\)", names(data))
data2<-data[,cols_extract]

#use descriptive names for the activity column
activity_data<-as.character(data2[,"activity"])
for (i in 1:length(activity_data)) {
    if (activity_data[i] == "1") activity_data[i]="WALKING"
    else if (activity_data[i] == "2") activity_data[i]="WALKING UPSTAIRS"
    else if (activity_data[i] == "3") activity_data[i]="WALKING DOWNSTAIRS"
    else if (activity_data[i] == "4") activity_data[i]="SITTING"
    else if (activity_data[i] == "5") activity_data[i]="STANDING"
    else activity_data[i]="LAYING"
}

data2[,"activity"]<-activity_data

#convert all column to numeric
for (i in 3:length(names(data2))) {
    data2[,i]<-suppressWarnings(as.numeric(data2[,i]))
}

#create a second tidy set with the average of each variable for each activity 
#and each subject

tidy_data<-group_by(data2,subject,activity)
y<-summarise_each(tidy_data,funs(mean))
write.table(y,file="tidy_data.txt",row.names=TRUE)
