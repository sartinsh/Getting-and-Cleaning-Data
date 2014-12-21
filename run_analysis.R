library(reshape2)
library(plyr)

#Reading in data and col names
test_set<-read.table("./test/X_test.txt")
subjects_test<-read.table("./test/subject_test.txt")
activities_test<-read.table("./test/y_test.txt")
train_set<-read.table("./train/X_train.txt")
subjects_train<-read.table("./train/subject_train.txt")
activities_train<-read.table("./train/y_train.txt")
column_names<-read.table("features.txt", stringsAsFactors=FALSE)
activity_labels<-read.table("activity_labels.txt", stringsAsFactors=FALSE)

#combining train and test data
data_set<-rbind(train_set,test_set)
subjects<-rbind(subjects_train,subjects_test)
activities<-rbind(activities_train,activities_test)

#adding column names
names(data_set)<-column_names[,2]

#looking if column name contains "mean()" or "std()" and creating vector of indices
extract_columns<-grep("mean()",column_names[,2],fixed=TRUE)
extract_columns<-append(extract_columns,grep("std()",column_names[,2],fixed=TRUE))

#extracting mean and standard deviation for each measurement
par_data_set<-data_set[,extract_columns]

names(par_data_set)

#adding subject id and activity code
par_data_set<-cbind(subjects,activities,par_data_set)

#changing column names
names(par_data_set)[c(1,2)]<-c("subject_id","activity")

#substituting activity codes with activity names
for (i in 1:length(activity_labels$V1))
{
  par_data_set$activity[which(par_data_set$activity %in% activity_labels$V1[i])] <- activity_labels$V2[i]
}

#splitting dataset by subject
split_data_set<-split(par_data_set,par_data_set$subject_id )

#melting each split
split_data_set_Melt<-lapply(split_data_set,function (x) {melt(x,id=c("subject_id","activity"))})

#calculating average values for each attribute
split_data_set_Cast<-lapply(split_data_set_Melt, function (x){dcast(x, activity ~ variable, mean)})

#combining list of data frames back in to single data frame
split_data_set <- ldply(split_data_set_Cast, data.frame, .id="subject_id")

#fixing column names
names(split_data_set)<-names(par_data_set)

#writing cleaned data set to file
write.table(split_data_set, file="tidy_data.txt", row.name=FALSE)