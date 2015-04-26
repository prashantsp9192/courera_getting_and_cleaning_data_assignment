# courera_getting_and_cleaning_data_assignment
This is the assignment for "Getting and Cleaning Data course" offered by John Hpkins School

Before running the code, please ensure the extracted zip file containing "UCI HAR Dataset" folder is in your working directory

## The code below reads the files from the working directory

X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt")



## The code blow is for binding together the X_test,y_test and subject_test files into a single DF called test_data

test_data<-cbind(X_test,y_test,subject_test)
names(test_data)[ncol(test_data)-1]<-"activity"
names(test_data)[ncol(test_data)]<-"subject"


## The code blow is for binding together the X_train,y_train and subject_train files into a single DF called train_data

train_data<-cbind(X_train,y_train,subject_train)
names(train_data)[ncol(train_data)-1]<-"activity"
names(train_data)[ncol(train_data)]<-"subject"


## Merging together the training set and the test set
merged_data<-rbind(test_data,train_data)
names<-as.character(features[,2])
names<-c(names,"activity","subject")
names(merged_data)<-names


## defining the regex pattern to extract relevant column names  i.e. one containing mean() and sd() in their names
pattern=".*mean()|std().*"
indices<-grep(pattern,features[,2])


## subsetting the merged data to contain only the relevant columns i.e. one containing mean() and sd() in their names

merged_data_subset<-merged_data[,indices]
merged_data_subset<-cbind(merged_data_subset,"activity"=merged_data$activity)
merged_data_subset<-cbind(merged_data_subset,"subject"=merged_data$subject)



## defining a hash map called activity_value_map that converts the activity number(key) into the corresponding activity_name(value)

activity_name(value)
activity_value_map<-c()
activity_value_map<-activity_labels[,2]
names(activity_value_map)<-activity_labels[,1]
activity_value_map<-as.character(activity_value_map)


## Uses descriptive activity names to name the activities in the data set

temp_vector<-c()
for(i in 1:nrow(merged_data_subset))
{
        temp_vector<-c(temp_vector,activity_value_map[merged_data_subset$activity[i]])
}
merged_data_subset$activity<-temp_vector



## From the above data set creating a second, independent tidy data set with the average of each variable for each activity and each subject.

data_split_per_subject<-split(merged_data_subset,merged_data_subset$subject)
final_append_activity<-c()
final_append_subject<-c()
final_data<-data.frame()
for(i in 1:length(data_split_per_subject))
{
        subject=data_split_per_subject[[i]]$subject[1]
        temp1=split(data_split_per_subject[[i]],data_split_per_subject[[i]]$activity)
        for(j in 1:length(temp1))
        {
                activity<-temp1[[j]]$activity[1]
                temp_df<-temp1[[j]][,1:79]
                col_means<-colMeans(temp_df)
                final_append_activity<-c(final_append_activity,activity)
                final_append_subject<-c(final_append_subject,subject)
                final_data<-rbind(final_data,col_means)
        }
}
final_data<-cbind(final_data,final_append_activity,final_append_subject)
names(final_data)<-names(merged_data_subset)

## coverting the tidy data set obtained above into .txt format using write.table()

write.table(final_data,"tidy_data_set_containing_averages.txt",row.names=FALSE)

# End of code 
