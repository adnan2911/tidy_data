library(dplyr)
library(R.utils)

#Read all training and the test sets
ytestd <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt",sep="", header = F)
xtestd <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt",sep="", header = F)
stestd <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt",sep="", header = F)

ytraind <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",sep="", header = F)
xtraind <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",sep="", header = F)
straind <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",sep="", header = F)

features_doc <- read.table("getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",sep = "",header = F)
mean_std_vector <- grep("([Mm]ean|std)",features_doc$V2) + 2 
mean_std_vector <- insert(mean_std_vector,ats=1,values = c(1,2)) #create vector of indexes from features document. The indexes will be used to replace later
features_names  <- grep("([Mm]ean|std)",features_doc$V2,value = TRUE) #create vector with strings names instead


#STEP 1 #Merges the training and the test sets to create one data set.
testmerged <- cbind(stestd,ytestd,xtestd)
trainmerged <- cbind(straind,ytraind,xtraind)
all_merged <- rbind(testmerged,trainmerged)
colnames(all_merged)[1] <- "Subject"
colnames(all_merged)[2] <- "Activity"

#STEP 2 Extracts only the measurements on the mean and standard deviation for each measurement.
extracted <- select(all_merged,mean_std_vector)

#STEP 3 Uses descriptive activity names to name the activities in the data set
lookuptable <- c("1"="WALKING","2"="WALKING_UPSTAIRS","3"="WALKING_DOWNSTAIRS", "4"= "SITTING", "5"="STANDING" , "6"="LAYING")
extracted$Activity <- lookuptable[extracted$Activity]

#STEP 4  Appropriately labels the data set with descriptive variable names.
colnames(extracted)[3:88]<- features_names

#STEP 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
final_df <- extracted %>%
        group_by(Subject,Activity) %>%
        summarise_each(list(mean))

write.table(final_df,file="tidy_data.txt",row.names = F)