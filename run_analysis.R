library('dplyr')


download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",destfile="activity.zip")

#unzip the file,
unzip("activity.zip")



setwd("H:/Trainings/coursera/DataScience_2021")
data_folder <- paste0(getwd(),"/UCI HAR Dataset")
activity_labels <- read.delim(paste0(data_folder,"/activity_labels.txt"),header = FALSE,sep = " ",col.names = c("activityid","activity_label"),colClasses = c("integer","character"))
dim(activity_labels)
head(activity_labels)


features <- read.delim(paste0(data_folder,"/features.txt"),header = FALSE,sep = " ",col.names = c("featureid","feature_label"),colClasses = c("integer","character"))
dim(features)
head(features)

for (t in c("train","test")) {
        print(paste0("reading datasets --- " ,t))
        #read in subject 
        assign(paste0("subject_",t), read.table(paste(data_folder,"/",t,"/subject_",t,".txt",sep=""),header = FALSE, col.names=c("subjectid"),colClasses = c("integer")))
        #read in X
        assign(paste0("X_",t), read.table(paste(data_folder,"/",t,"/X_",t,".txt",sep=""),header = FALSE, ,colClasses = c("double")))
        #read in y 
        assign(paste0("y_",t), read.table(paste(data_folder,"/",t,"/y_",t,".txt",sep=""),header = FALSE, col.names=c("y"),colClasses = c("integer")))
} 

# Before merge, create a flag to remember which (test or train) the data is from
train <- cbind(subject_train,y_train, X_train) %>% mutate(data_use='train')
test <- cbind(subject_test,y_test, X_test) %>% mutate(data_use='test')


#combine train and test dataset into one and call it ana_data
ana_data <- rbind(train, test)

#clean up the unused datasets
rm(list=ls(pattern='train|test'))

#check variable names of combined dataset and do a basic summary
names(ana_data)
str(ana_data)

#find features on mean and std
mean_std_features <- features[grep('mean[()]|std[()]',features$feature_label)  ,] 

#create variable names using feature id and feature descriptions with parenthesises removed 
#variable name for each feature will be 
# F[featureid]_[feature_desc]m example: f1_tBodyAcc-mean-X

mean_std_features$varname <- paste('f',mean_std_features$featureid, '_', gsub("[()]","",mean_std_features$feature_label),sep = '')


#subset only columns on mean and std for each subject and y 
ana_data_rq <- ana_data[,names(ana_data) %in% c(paste0('V',mean_std_features$featureid),'subjectid','y')]
names(ana_data_rq)

#assign new meaningfule names to all feature variables and rename y to activityid  
names(ana_data_rq) <- c("subjectid", "activityid",mean_std_features$varname )
names(ana_data_rq)

#add descriptions for activities 
ana_data_rq <- inner_join(ana_data_rq,activity_labels,by=c("activityid"="activityid"))

#variable names and their descriptions
print(paste0("variable ", mean_std_features$varname,'--- feature ',mean_std_features$featureid,':',mean_std_features$feature_label))


grpby_subj_act <- group_by(ana_data_rq,subjectid,activity_label)
avg_by_subj_act<-summarise(grpby_subj_act,across(starts_with("f"),mean,.names="mean_{.col}"))

head(avg_by_subj_act)
#save as a csv file 
write.table(avg_by_subj_act,file=paste0(data_folder,'/avg_by_subj_activitiy.txt'),row.names=FALSE,col.names=TRUE,sep=",")
