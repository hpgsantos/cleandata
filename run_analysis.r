
if(!require(plyr)) install.packages('plyr',dependencies=TRUE)


url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
fname <- "data.zip"
f <- file.path(fname)
download.file(url, f)

unzip(fname) 


path <-"./UCI HAR Dataset"
pathTrain  <- paste(path, "train/",sep="/")
pathTest <- paste(path, "test/",sep="/")


## function to validade filtering by regex pattern
filterByRegex<- function(x, bind = "all") {
   
    pattern <- ".*-(mean|std).*"
    
    if(bind == "mean")
       pattern <- ".*-(mean).*"
    else if(bind == "std")
       pattern <- ".*-(std).*"
    
    ## grepel returns TRUE/FALSE instead position
    return(grepl(pattern,x))
}

## create all datasets needed
dfFeatures <- read.table(paste(path,"features.txt",sep="/"), stringsAsFactors=FALSE)
dfActivities <- read.table(paste(path,"activity_labels.txt",sep="/"), stringsAsFactors=FALSE)
dfTrain <- read.table(paste(pathTrain,"X_train.txt",sep="/"), sep="")
dfTest <- read.table(paste(pathTest,"X_test.txt",sep="/"), sep="")


## matching filtered features and returnind a subset of selected ones
dfFeatures.select <- subset(dfFeatures, filterByRegex(dfFeatures[,2]))
dfFeatures.select$type <- ifelse(filterByRegex(dfFeatures.select[,2],"mean"),"M","S")

## Assig 02 Extracts only the measurements on the mean and standard deviation for each measurement
## Assign 03  Using descriptive activity names to name the activities in the data set,removing non alphanumeric chars
dfFeatures.select$name <- gsub("[^[:alnum:] ]","",dfFeatures.select[,2])
rownames(dfFeatures.select) <- NULL

##cleaned features named to compose tidy data 
## Assig 04 Appropriately labels the data set with descriptive variable names.
featuresName<- as.vector(dfFeatures.select$name)

## generete train dataset with the features selected
## Assig 02 Extracts only the measurements on the mean and standard deviation for each measurement
dfTrain <- dfTrain[,dfFeatures.select[,1]]
dfTrainLabel <- read.table(paste(pathTrain,"y_train.txt",sep="/"), sep="")
dfTrainSubject <- read.table(paste(pathTrain,"subject_train.txt",sep="/"), sep="")
dfTrain <- cbind(dfTrainSubject,dfTrainLabel,dfTrain)

## generete test dataset with the features selected
dfTest <- dfTest[,dfFeatures.select[,1]]
dfTestLabel <- read.table(paste(pathTest,"y_test.txt",sep="/"), sep="")
dfTestSubject <- read.table(paste(pathTest,"subject_test.txt",sep="/"), sep="")
dfTest <- cbind(dfTestSubject,dfTestLabel,dfTest)


##make a uninion between datasets
dfMerged <- rbind(dfTrain,dfTest)

colnames(dfMerged) <- c("subject","activity",featuresName)
dfMerged$activity<- factor(dfMerged$activity, levels = c(1,2,3,4,5,6),labels = c("WALKING","WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

## From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
dfTidy <- aggregate(. ~ subject + activity, dfMerged, mean)
dfTidy <- dfTidy[order(dfTidy$subject,dfTidy$activity),]
rownames(dfTidy) <- NULL
write.table(dfTidy, "tidy_data.txt", row.name=FALSE )

