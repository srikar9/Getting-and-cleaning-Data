#Merges the training and the test sets to create one data set.
##Extracts only the measurements on the mean and standard deviation for each measurement.
###Uses descriptive activity names to name the activities in the data set
####Appropriately labels the data set with descriptive variable names.
#####creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#1. Merge the training and the test sets to create one data set.

# setting the working directory.

setwd("E:/Srikar's/Coursera Video Lectures/GCD/UCI HAR Dataset")

## Reading data from files

features = read.table('./features.txt',header=FALSE);
activityType = read.table('./activity_labels.txt',header=FALSE);
subjectTrain = read.table('./train/subject_train.txt',header=FALSE);
xTrain = read.table('./train/x_train.txt',header=FALSE);
yTrain = read.table('./train/y_train.txt',header=FALSE);

## Assigning column names to the imported data

colnames(activityType) = c('activityId','activityType');
colnames(subjectTrain) = "subjectId";
colnames(xTrain) = features[,2];
colnames(yTrain) = "activityId";

## Creating the final training set by merging yTrain, subjectTrain and xTrain

trainingData <- cbind(yTrain,subjectTrain,xTrain)

## Reading the test data

subjectTest = read.table('./test/subject_test.txt',header=FALSE);
xTest = read.table('./test/x_test.txt',header=FALSE);
yTest = read.table('./test/y_test.txt',header=FALSE);

## Assigning column names to the imported test data

colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";

## create the final test set by merging the xTest,yTest and subjectTest data

testData <- cbind(yTest,subjectTest,xTest)

## Combine the training and test data to create a final data set

finalData <- rbind(trainingData,testData)

## Creating a vector for column names from the finalData, which will be used to select the desires mean()&stdev() columns

colNames <- colnames(finalData)

#2.Extracts only the measurements on the mean and standard deviation for each measurement.

## Create a logicalVector that contains TRUE values for the Id, mean() & stdev()columns and FALSE for others

logicalVector <- (grepl("activity..",colNames)| grepl("subject..",colNames)|grepl("-mean..",colNames)& 
                    !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

## subset the finalData table based on logicalVector to keep only desired columns

finalData <- finalData[logicalVector==TRUE]

#3. Uses descriptive activity names to name the activities in the data set

## Merge the finalData set with the activityType table to include the descriptive activity names
finalData <- merge(finalData,activityType,by='activityId',all.x=TRUE)

## Updating the colNames vector to include the new column after merge

colNames <- colnames(finalData)

#4. Appropriately labels the data set with descriptive variable names.

# cleaning up the variable names

for(i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

## Reassigning the new descriptive column names to the finalData set
colNmaes <- colnames(finalData)

#5.creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Create a new table, finalDataNoActivityType without the activityType column

finalDataNoActivityType <- finalData[,names(finalData)!='activityType']

## Summarizing the finaldataNoActivityType table to include just the mean of each variable for each activity and each subject.

tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) !=c('activityId','subjectId')],
                      by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)


## Export the tidyData set

write.table(tidyData, './tidyData.txt',row.
