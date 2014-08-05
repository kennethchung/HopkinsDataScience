

test1<-function(){
  filename = "./UCI HAR Dataset/test/X_test.txt"
  testDataset <- read.table(filename);
  print(ncol(testDataset))
  print(nrow(testDataset))
  
  filename = "./UCI HAR Dataset/test/y_test.txt"
  testLabels <- read.table(filename);
  print(ncol(testLabels))
  print(nrow(testLabels))
  
  
  filename = "./UCI HAR Dataset/test/subject_test.txt"
  testSubjects <- read.table(filename);
  print(unique(testSubjects[,1]))
  print(ncol(testSubjects))
  print(nrow(testSubjects))
  
  
  filename = "./UCI HAR Dataset/train/X_train.txt"
  trainDataset <- read.table(filename);
  print(ncol(trainDataset))
  print(nrow(trainDataset))
  
  filename = "./UCI HAR Dataset/train/y_train.txt"
  trainLabels <- read.table(filename);
  print(ncol(trainLabels))
  print(nrow(trainLabels))
  
  
  filename = "./UCI HAR Dataset/train/subject_train.txt"
  trainSubjects <- read.table(filename);
  print(unique(trainSubjects[,1]))
  print(ncol(trainSubjects))
  print(nrow(trainSubjects))
  
}


