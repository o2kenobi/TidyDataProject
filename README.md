Project assignment for Week4 Cleaning Data
------------------------------------------

\*from zfile="getdata\_projectfiles\_UCI HAR Dataset.zip"
Create run\_analysis.R to
1. merges the traning and the test sets to create 1 dataset
2. extracts only the measurements on the mean & sd for each measurement
3. Uses descriptive activity names to name the activities in the dataset
4. Appropriately labels the dataset with descriptive var names.
5. From the dataset in step 4, creates a second, independent tidy dataset with the average of each variable for each activity and each subject.

**First**, With the following steps, process the 2 separate datasets for Train and test groups, respectively.

**Step a.** Assign data paths

``` r
dirhd<-"UCI HAR Dataset/"
dir.trn<-paste(dirhd,"train/",sep="")
dir.test<-paste(dirhd,"test/",sep="")
```

**Step b.** Get info about data

``` r
acties <- read.table(paste(dirhd,"activity_labels.txt",sep=""))
ftures <- read.table(paste(dirhd,"features.txt",sep=""))
str(acties)      #<--6 activities
```

    ## 'data.frame':    6 obs. of  2 variables:
    ##  $ V1: int  1 2 3 4 5 6
    ##  $ V2: Factor w/ 6 levels "LAYING","SITTING",..: 4 6 5 2 3 1

**Step c.** Get and ready Train data set

``` r
sbj1 <- read.table(paste(dir.trn,"/subject_train.txt",sep=""))
xs1  <- read.table(paste(dir.trn,"X_train.txt",sep=""))
ys1  <- read.table(paste(dir.trn,"y_train.txt",sep=""))
str(sbj1)                #<-- 21 subjects, labels in 1-30
```

    ## 'data.frame':    7352 obs. of  1 variable:
    ##  $ V1: int  1 1 1 1 1 1 1 1 1 1 ...

``` r
length(unique(sbj1$V1))  #[1] 21
```

    ## [1] 21

``` r
dim(xs1)
```

    ## [1] 7352  561

``` r
str(ys1)
```

    ## 'data.frame':    7352 obs. of  1 variable:
    ##  $ V1: int  5 5 5 5 5 5 5 5 5 5 ...

``` r
unique(ys1)              #<-- activity label by no.
```

    ##     V1
    ## 1    5
    ## 28   4
    ## 52   6
    ## 79   1
    ## 126  3
    ## 151  2

**Step d.** use features to name columns and save activities for another column to use

``` r
names(xs1) <-ftures$V2
ys1$act <-as.character(rep("o",dim(ys1)[1]))
for (i in 1:6) ys1$act[ys1$V1==i]=tolower(as.character(acties[i,2]))
```

**Step e.** Get and ready Test data set

``` r
sbj2 <- read.table(paste(dir.test,"/subject_test.txt",sep=""))
xs2  <- read.table(paste(dir.test,"X_test.txt",sep=""))
ys2  <- read.table(paste(dir.test,"y_test.txt",sep=""))
names(xs2) <-ftures$V2
ys2$act <-as.character(rep("o",dim(ys2)[1]))
for (i in 1:6) ys2$act[ys2$V1==i]=tolower(as.character(acties[i,2]))
```

**Step f.** Load dplyr library for data frame manipulation

``` r
library(dplyr)
```

    ## Warning: package 'dplyr' was built under R version 3.4.4

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

### Below are answers to the requests of the project assigment

**Request 2.** Extract data for only mean() and std() & add subject and activity columns

``` r
imn<-grep("mean...[XYZ]",ftures$V2)  #pick out the "mean" features for x,y,z
isd<-grep("std...[XYZ]",ftures$V2)   #pick out the "std"  features
trnset           <-xs1[,c(imn,isd)]  #<--create train dataset
trnset$subject   <- sbj1[,1]
trnset$activity  <- ys1[,2]          #use meaningful activity names (Request 3)
testset          <-xs2[,c(imn,isd)]  #<--create test dataset
testset$subject  <- sbj2[,1]
testset$activity <- ys2[,2]          #use meaningful activity names (Request 3)
```

**Request 1.** Merge train and test datasets
\#bothsets&lt;-merge(trnset,testset,all=T) \#&lt;--merged based on same vars. SLOW!!

``` r
bothsets<-rbind(trnset,testset)
```

**Request 3.** Uses descriptive activity names to name the activities in the dataset already used activities names**(Already done in Answer to Request 2)**

``` r
unique(bothsets$activity)
```

    ## [1] "standing"           "sitting"            "laying"            
    ## [4] "walking"            "walking_downstairs" "walking_upstairs"

**Request 4.** Appropriately labels the dataset with descriptive var names. Used measurement names with mean/std and X/Y/Z from the original datasets. **(Already done in Steps d & e)**

Plus,"subject" and "activity" columns and labeled were added

``` r
names(bothsets)
```

    ##  [1] "tBodyAcc-mean()-X"      "tBodyAcc-mean()-Y"     
    ##  [3] "tBodyAcc-mean()-Z"      "tGravityAcc-mean()-X"  
    ##  [5] "tGravityAcc-mean()-Y"   "tGravityAcc-mean()-Z"  
    ##  [7] "tBodyAccJerk-mean()-X"  "tBodyAccJerk-mean()-Y" 
    ##  [9] "tBodyAccJerk-mean()-Z"  "tBodyGyro-mean()-X"    
    ## [11] "tBodyGyro-mean()-Y"     "tBodyGyro-mean()-Z"    
    ## [13] "tBodyGyroJerk-mean()-X" "tBodyGyroJerk-mean()-Y"
    ## [15] "tBodyGyroJerk-mean()-Z" "fBodyAcc-mean()-X"     
    ## [17] "fBodyAcc-mean()-Y"      "fBodyAcc-mean()-Z"     
    ## [19] "fBodyAccJerk-mean()-X"  "fBodyAccJerk-mean()-Y" 
    ## [21] "fBodyAccJerk-mean()-Z"  "fBodyGyro-mean()-X"    
    ## [23] "fBodyGyro-mean()-Y"     "fBodyGyro-mean()-Z"    
    ## [25] "tBodyAcc-std()-X"       "tBodyAcc-std()-Y"      
    ## [27] "tBodyAcc-std()-Z"       "tGravityAcc-std()-X"   
    ## [29] "tGravityAcc-std()-Y"    "tGravityAcc-std()-Z"   
    ## [31] "tBodyAccJerk-std()-X"   "tBodyAccJerk-std()-Y"  
    ## [33] "tBodyAccJerk-std()-Z"   "tBodyGyro-std()-X"     
    ## [35] "tBodyGyro-std()-Y"      "tBodyGyro-std()-Z"     
    ## [37] "tBodyGyroJerk-std()-X"  "tBodyGyroJerk-std()-Y" 
    ## [39] "tBodyGyroJerk-std()-Z"  "fBodyAcc-std()-X"      
    ## [41] "fBodyAcc-std()-Y"       "fBodyAcc-std()-Z"      
    ## [43] "fBodyAccJerk-std()-X"   "fBodyAccJerk-std()-Y"  
    ## [45] "fBodyAccJerk-std()-Z"   "fBodyGyro-std()-X"     
    ## [47] "fBodyGyro-std()-Y"      "fBodyGyro-std()-Z"     
    ## [49] "subject"                "activity"

**Request 5.** From the dataset in step 4, creates a second, independent tidy dataset with the average of each variable for each activity and each subject.

``` r
cnames<-names(bothsets)
jmn<-grep("mean...[XYZ]",cnames)  #pick out the "mean" features for x,y,z
tidymeans<-data.frame()
activity<-unique(bothsets$activity)
for (sbj in 1:6) {
   for (act in activity){
      c<-filter(bothsets,subject==sbj,activity==act)[,-(49:50)]
      tidymeans<-rbind(tidymeans,c(apply(c,2,mean)))
   }
}
tidymeans$subject  <-rep(1:6,each=6)
tidymeans$activity <-rep(activity,6)
names(tidymeans)<-cnames
```

**Finally**, save new tidy dataset to a file

``` r
write.table(tidymeans,file="tidymeans.txt",row.name=F) 
```

### Codebook

This is a re-arranged dataset merged from the Human Activity Recognition Using Smartphones Dataset, containing the mean values of the variables (as column names of the merged data) for each subject and each activity.

==================================================================
Human Activity Recognition Using Smartphones Dataset
Adopted from Version 1.0
==================================================================
The experiments have been carried out with a group of 30 volunteers that were randomly separted into 2 groups (training and test) for the purpose of analysis. Each person performed six activities (standing, sitting, laying, walking, walking\_downstairs, walking\_upstairs) wearing a smartphone on the waist. Using its embedded accelerometer and gyroscope, 3-axial linear acceleration and 3-axial angular velocity were captured at a constant rate of 50Hz.

For more information about this dataset contact: <activityrecognition@smartlab.ws>

#### Columns in the tidy dataset

The data contains the mean values of the variables labeled for the columns of the data, together with a "subject" colomn and an "activity" column that contain the information of the experiment subjects (1-30) and the activities conducted in the experiment, respectively.

The data variables cover the estimated mean and standard deviation (std) of the features measurement. And the list of the variables, including "subject" and "activity", in the data is:
1. "tBodyAcc-mean()-X"
2. "tBodyAcc-mean()-Y"
3. "tBodyAcc-mean()-Z"
4. "tGravityAcc-mean()-X"
5. "tGravityAcc-mean()-Y"
6. "tGravityAcc-mean()-Z"
7. "tBodyAccJerk-mean()-X"
8. "tBodyAccJerk-mean()-Y"
9. "tBodyAccJerk-mean()-Z"
10 "tBodyGyro-mean()-X"
11 "tBodyGyro-mean()-Y"
12 "tBodyGyro-mean()-Z"
13 "tBodyGyroJerk-mean()-X"
14 "tBodyGyroJerk-mean()-Y"
15 "tBodyGyroJerk-mean()-Z"
16 "fBodyAcc-mean()-X"
17 "fBodyAcc-mean()-Y"
18 "fBodyAcc-mean()-Z"
19 "fBodyAccJerk-mean()-X"
20 "fBodyAccJerk-mean()-Y"
21 "fBodyAccJerk-mean()-Z"
22 "fBodyGyro-mean()-X"
23 "fBodyGyro-mean()-Y"
24 "fBodyGyro-mean()-Z"
25 "tBodyAcc-std()-X"
26 "tBodyAcc-std()-Y"
27 "tBodyAcc-std()-Z"
28 "tGravityAcc-std()-X"
29 "tGravityAcc-std()-Y"
30 "tGravityAcc-std()-Z"
31 "tBodyAccJerk-std()-X"
32 "tBodyAccJerk-std()-Y"
33 "tBodyAccJerk-std()-Z"
34 "tBodyGyro-std()-X"
35 "tBodyGyro-std()-Y"
36 "tBodyGyro-std()-Z"
37 "tBodyGyroJerk-std()-X"
38 "tBodyGyroJerk-std()-Y"
39 "tBodyGyroJerk-std()-Z" 40 "fBodyAcc-std()-X"
41 "fBodyAcc-std()-Y"
42 "fBodyAcc-std()-Z"
43 "fBodyAccJerk-std()-X"
44 "fBodyAccJerk-std()-Y"
45 "fBodyAccJerk-std()-Z"
46 "fBodyGyro-std()-X"
47 "fBodyGyro-std()-Y"
48 "fBodyGyro-std()-Z"
49 "subject" - experiment participants, labeled from 1 to 30
50 "activity" - activities conducted in the experiemnt, including "standing", "sitting","laying","walking","walking\_downstairs", and "walking\_upstairs"

**Feature Selection**
=====================

The variables' names in this re-arranged tidy dataset reflect features of the experiment data.

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for each pattern:
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:

gravityMean
tBodyAccMean
tBodyAccJerkMean
tBodyGyroMean
tBodyGyroJerkMean

Notes:
======

-   Features are normalized and bounded within \[-1,1\].

The set of variables that were estimated from these signals are:

mean(): Mean value
std(): Standard deviation
