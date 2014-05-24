run_analysis<- function () {
  #
  ## Read the data set relating to test: All data is read including the signals
  #
  s1 <- read.table("./UCI HAR Dataset/test/subject_test.txt")
  X1data <- read.table("./UCI HAR Dataset/test/X_test.txt")
  y1data <- read.table("./UCI HAR Dataset/test/y_test.txt")
  testData <- cbind(s1,y1data,X1data)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/body_acc_x_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/body_acc_y_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/body_acc_z_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/body_gyro_x_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/body_gyro_y_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/body_gyro_z_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/total_acc_x_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/total_acc_y_test.txt")
  testData <- cbind(testData,temp)
  temp <- read.table("./UCI HAR Dataset/test/Inertial\ Signals/total_acc_z_test.txt")
  testData <- cbind(testData,temp)
  #
  ## Read the data set relating to train: All data is read including the signals
  #
  s1 <- read.table("./UCI HAR Dataset/train/subject_train.txt")
  X1data <- read.table("./UCI HAR Dataset/train/X_train.txt")
  y1data <- read.table("./UCI HAR Dataset/train/y_train.txt")
  trainData <- cbind(s1,y1data,X1data)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/body_acc_x_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/body_acc_y_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/body_acc_z_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/body_gyro_x_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/body_gyro_y_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/body_gyro_z_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/total_acc_x_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/total_acc_y_train.txt")
  trainData <- cbind(trainData,temp)
  temp <- read.table("./UCI HAR Dataset/train/Inertial\ Signals/total_acc_z_train.txt")
  trainData <- cbind(trainData,temp)
  #
  ## Added a new column to identify if the row comes from test or train
  #
  testData$testortrain <- c(rep("test",2947))
  trainData$testortrain <- c(rep("train",7352))
  #
  ## Combine the train and test data
  #
  allData <- rbind(trainData,testData)
  #
  ## Work in processing the column names
  #
  featureNames <- read.table("./UCI HAR Dataset/features.txt",colClasses=c("numeric","character"))
  featureNames <- featureNames[,2]
  featureNames <- lapply(featureNames,function(x){tolower(gsub("-|,","",x))})
  featureNames <- gsub("\\(","",featureNames)
  featureNames <- gsub("\\)","",featureNames)
  #
  ## Work in creating column names for the signal data
  #
  valueNames <- lapply(1:128,function(x) paste("bodyaccx",x,sep=""))
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("bodyaccy",x,sep="")))
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("bodyaccz",x,sep="")))
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("bodygyrx",x,sep="")))
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("bodygyry",x,sep=""))) 
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("bodygyrz",x,sep="")))
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("totalaccx",x,sep="")))
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("totalaccy",x,sep="")))
  valueNames <- c(valueNames, lapply(1:128,function(x) paste("totalaccz",x,sep="")))
  #
  ## Assign column names to all the data that has been collected
  #
  colnames(allData) <- c("subjectno","activity",featureNames,valueNames,"testortrail")
  #
  ## Give meaningful names to the activities
  #
  allData$activity[allData$activity==1] <- "WALKING"
  allData$activity[allData$activity==2] <- "WALKING_UPSTAIRS"
  allData$activity[allData$activity==3] <- "WALKING_DOWNSTAIRS"
  allData$activity[allData$activity==4] <- "SITTING"
  allData$activity[allData$activity==5] <- "STANDING"
  allData$activity[allData$activity==6] <- "LAYING"
  #
  ## Collect only columns that has the word mean or std as a partial match
  #
  result <- allData[,c(1,2,grep("mean|std",colnames(allData)))]
  #
  ## Perform the necessary aggregation and give meaningful names
  #
  result2 <- aggregate(result[,3:88],by=list(result$activity,result$subjectno),FUN=mean)
  colnames(result2)[1:2] <- c("activity","subjectno")
  #
  ## Write the results to a file
  #
  write.table(result2,file="./tidydataoutput.txt",sep=",")
}