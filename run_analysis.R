
#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names.
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject


#Merges the training and the test sets to create one data set.

#read data
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityLabel = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt

#assign column names
colnames(activityLabel) = c('activityID', 'activityLabel')
colnames(subjectTrain) = "subjectID"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityID"

#creating Training dataset
TrainingData = cbind(yTrain, subjectTrain, xTrain)

##The same process of creating test dataset
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

#assign column names
colnames(subjectTest) = "subjectID"
colnames(xTest) = features[,2]
colnames(yTest) = "activityID"

#creating Test dataset
TestData = cbind(yTest, subjectTest, xTest)

##merge two data sets to one
MergedData = rbind(TrainingData, TestData)

#Extracts only the measurements on the mean and standard deviation for each measurement.

install.packages("dplyr")
library(dplyr)

valid_column_names <- make.names(names=names(MergedData), unique=TRUE, allow_ = TRUE)
names(MergedData) <- valid_column_names
head(MergedData)

SelectedData <- select(MergedData, activityID, subjectID, contains("mean"), contains("std"))
head(SelectedData, 10)

#Use descriptive activity names to name the activities in the data set
SelectedData = left_join(SelectedData,activityLabel,by='activityID')
head(SelectedData)

# Cleaning up the variable names
colnames = colnames(SelectedData)
for (i in 1:length(colnames))
{
  colnames[i] = gsub("std","StdDev",colnames[i])
  colnames[i] = gsub("mean","Mean",colnames[i])
  colnames[i] = gsub("^(t)","time",colnames[i])
  colnames[i] = gsub("^(f)","freq",colnames[i])
  colnames[i] = gsub("([Gg]ravity)","Gravity",colnames[i])
  colnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colnames[i])
  colnames[i] = gsub("[Gg]yro","Gyro",colnames[i])
  colnames[i] = gsub("AccMag","AccMagnitude",colnames[i])
  colnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colnames[i])
  colnames[i] = gsub("JerkMag","JerkMagnitude",colnames[i])
  colnames[i] = gsub("GyroMag","GyroMagnitude",colnames[i])
}
colnames(SelectedData) = colnames


#Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidySelectedData <- SelectedData %>%
  group_by(activityID, subjectID, activityLabel) %>%
  select(c(3:88)) %>%
  summarise_each(funs(mean)) 

head(tidySelectedData)
  
# Export the tidyData set 
write.table(tidySelectedData, './tidySelectedData.txt',row.names=TRUE,sep='\t')
  