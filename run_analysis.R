library(data.table)
library(dplyr)
#####  First Assignment: Merges the training and the test sets to create one data set   #####  
dir.create("Assigment_HCR") # Creating  a working directory
setwd("Assigment_HCR")      # Setting a working directory
f_Url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(f_Url,"Assigment_HCR/Dataset.zip") #download zip file from provided source
unzip("./Assigment_HCR/dataset.zip",exdir="./Assigment_HCR") #Decompressing zip file

df_names <-read.table("UCI HAR Dataset/features.txt", sep="") #Save names of 561 vars


df_X_train <- read.table("UCI HAR Dataset/train/x_train.txt", sep="") # Reading X train observations
df_y_train <- read.table("UCI HAR Dataset/train/y_train.txt", sep="")# Reading y train observations
df_subject_train <- read.table("UCI HAR Dataset/Train/subject_train.txt", sep="") # Reading subject train observations


df_X_test <- read.table("UCI HAR Dataset/test/X_test.txt", sep="") # Reading X test observations
df_y_test <- read.table("UCI HAR Dataset/test/y_test.txt", sep="") # Reading Y test observations
df_subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", sep="") # Reading subject train observations

df_test_HC <-cbind.data.frame(df_subject_test,df_y_test,df_X_test) #Binding  columns of three test data frames
names(df_test_HC)[1]<-"Subject"  # Assigning a name to column
names(df_test_HC)[2]<-"Activity" #Assigning a name to column
names(df_test_HC)[3:563]<-as.character(df_names[,2]) #Assigning names to measure columns

df_train_HC <-cbind.data.frame(df_subject_train,df_y_train,df_X_train) #Binding columns of three train data frames
names(df_train_HC)[1]<-"Subject" # Assigning a name to column of data frame
names(df_train_HC)[2]<-"Activity" # Assigning a name to column of data frame
names(df_train_HC)[3:563]<-as.character(df_names[,2]) #Assigning names to measure columns of data frame


df_combined_HC<-rbind.data.frame(df_test_HC,df_train_HC) #Binding columns of thow data frames (train and test)


#####  Second Assignment   
##### Extracts only the measurements on the mean and standard deviation for each measurement. 
df_extracted_mean_std<-df_combined_HC[grepl("Subject| Activity | mean\\(\\)| std\\(\\)",names(df_combined_HC)), ] 
# Extracting columns of means and standard deviations


#####  Third Assignment   #####  
#####  Uses descriptive activity names to name the activities in the data set
df_activities <-read.table("UCI HAR Dataset/activity_labels.txt", sep="",stringsAsFactors =FALSE) # Saving names of activities
names(df_activities)[1]<-"Id"   # Assigning a name to column of data frame
names(df_activities)[2]<-"Description"# Assigning a name to column of data frame
df_extracted_mean_std$ActivityDescription <-rep(NA,nrow(df_extracted_mean_std)) #Creating a new column filled with NA

for (i in 1:nrow(df_activities)) {                       
  df_extracted_mean_std$ActivityDescription[df_extracted_mean_std$Activity==i]<-as.character(df_activities[i,2])
  # Assigning name activity according with number
}


#####  Fourth Assignment   #####  
#####  Appropriately labels the data set with descriptive variable names.

names(df_extracted_mean_std) <- gsub("\\,", "_", names(df_extracted_mean_std)) #Replacing character "," for "_" on column names
names(df_extracted_mean_std) <- gsub("\\(", "", names(df_extracted_mean_std))  #Replacing character "(" for "" on column names
names(df_extracted_mean_std) <- gsub("\\)", "", names(df_extracted_mean_std))  #Replacing character ")" for "" on column names
names(df_extracted_mean_std) <-gsub("-","_",names(df_extracted_mean_std))      #Replacing character "-" for "_" on column names

##### fifth Assignment
##### From the data set in step 4, creates a second, independent tidy data set with the average of each variable 
##### for each activity and each subject.


df_extracted_mean_std$ActivityDescription <-as.factor(df_extracted_mean_std$ActivityDescription) #Converting a column to factor 
df_extracted_mean_std$Subject <-as.factor(df_extracted_mean_std$Subject) #Converting a column to factor 

V_id_vars <-c("ActivityDescription","Subject") # Defining Id variables that would be melted
v_measured_var <- names(df_combined_HC)[2:563] # Defining measures  that would be melted
df_melted_HC <-melt(df_extracted_mean_std,V_id_vars,measure.vars=v_measured_var) #Creating a data frame with combination of id var and measure melted
df_tidy <-dcast(df_melted_HC,ActivityDescription + Subject ~ variable, mean)
write.table(df_tidy,"Tidy.txt" , row.names = FALSE) # writing result on a external file