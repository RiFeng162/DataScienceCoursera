# This script is used to generate the tidy data set from the raw data
library(reshape2)
options(stringsAsFactors = FALSE)
# Download data
originDir = getwd()
path = 'data/projectData'
if (!file.exists(path)) {
    url = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
    download.file(url, destfile = path,method = 'curl')
}

# Read the datasets into R
setwd(path)
activity_labels = read.table('activity_labels.txt') 
names(activityIndx) = c('activityIndex','activity')
for (j in c('test','train')) {
    for (i in dir(j)) {
        file_path = paste0(j,'/',i)
        if (!dir.exists(file_path)) {
            file_value = read.table(file_path)
            file_name = strsplit(i,'\\.')[[1]][1]
            assign(file_name,file_value)
        }
    }
} # read the data in diretory test and train, except the inertial dataset
feature = read.table('features.txt')

# Merge the datasets and name the variables
names(subject_train) = 'subjectIndex'
names(subject_test) = 'subjectIndex'
names(y_train) = 'activityIndex'
names(y_test) = 'activityIndex'
trainDat = cbind(subject_train,y_train,X_train)
testDat = cbind(subject_test,y_test,X_test)
compltDat = rbind(trainDat,testDat)
names(compltDat)[3:length(names(compltDat))] = feature[,2]

# Extract mean and std measurements
meanVariables = grep(pattern = 'mean[^Freq]',x = names(compltDat),value = TRUE)
stdVariables = grep(pattern = 'std',x = names(compltDat),value = TRUE)

# Uses descriptive activity names to name the activities in the data set
namedDat = merge(compltDat,activityIndx,by.x = 'activityIndex', by.y = 'Index',all.x = TRUE)
namedDat = namedDat[,c(1,2,564,3:563)] # reorder the variables
namedDat$activityIndex = NULL # remove the activity labels column

#creates a second, independent tidy data set with the average of each variable for each activity and each subject.
groupedMean = aggregate(.~ subjectIndex + activity, namedDat, mean)
meltDat = melt(groupedMean,id.vars = c("subjectIndex","activity"),
               value.name = "meanValue")
tidyDat = meltDat[order(meltDat$subjectIndex,meltDat$activity),]
names(tidyDat)[names(tidyDat)=='variable'] = "measurement"
row.names(tidyDat) = NULL
write.table(tidyDat,"tidyData.txt")
setwd(originDir)


