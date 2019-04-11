##Project assignment for Cleaning Data week4
#zfile="getdata_projectfiles_UCI HAR Dataset.zip"
#dfiles<-unzip(zfile,exdir="data")
dirhd<-"UCI HAR Dataset/"
dir.trn<-paste(dirhd,"train/",sep="")
dir.test<-paste(dirhd,"test/",sep="")

#---data info
acties <- read.table(paste(dirhd,"activity_labels.txt",sep=""))
ftures <- read.table(paste(dirhd,"features.txt",sep=""))
#str(acties)      #<--6 activities
#str(ftures)     #<--561 features

#----0.a Get and ready Train data set
sbj1 <- read.table(paste(dir.trn,"/subject_train.txt",sep=""))
xs1  <- read.table(paste(dir.trn,"X_train.txt",sep=""))
ys1  <- read.table(paste(dir.trn,"y_train.txt",sep=""))
#str(sbj1)                   #<-- 21 subjects, labels in 1-30
#length(unique(sbj1$V1))     #[1] 21
#str(xs1)
#str(ys1) 
#unique(ys1)                 #<-- activity label by no.
names(xs1) <-ftures$V2
ys1$act <-as.character(rep("o",dim(ys1)[1]))
for (i in 1:6) ys1$act[ys1$V1==i]=tolower(as.character(acties[i,2]))

#----0.b Get and ready Test data set
sbj2 <- read.table(paste(dir.test,"/subject_test.txt",sep=""))
xs2  <- read.table(paste(dir.test,"X_test.txt",sep=""))
ys2  <- read.table(paste(dir.test,"y_test.txt",sep=""))
names(xs2) <-ftures$V2
ys2$act <-as.character(rep("o",dim(ys2)[1]))
for (i in 1:6) ys2$act[ys2$V1==i]=tolower(as.character(acties[i,2]))

library(dplyr)

#----2. Extract data for only mean() and std() 
#       & add subject and activity columns
imn<-grep("mean...[XYZ]",ftures$V2)  #pick out the "mean" features for x,y,z
isd<-grep("std...[XYZ]",ftures$V2)   #pick out the "std"  features
trnset           <-xs1[,c(imn,isd)]  #<--create train dataset
trnset$subject   <- sbj1[,1]
trnset$activity  <- ys1[,2]          #   use meaningful activity names (3. of the requirements)
testset          <-xs2[,c(imn,isd)]  #<--create test dataset
testset$subject  <- sbj2[,1]
testset$activity <- ys2[,2]          #   use meaningful activity names (3. of the requirements)

#----1. merge train and test datasets
#bothsets<-merge(trnset,testset,all=T)  #<--merged based on same vars. SLOW!!
bothsets<-rbind(trnset,testset)

#----3. Uses descriptive activity names to name the activities in the dataset
#       already used activities names
unique(bothsets$activity)

#----4. Appropriately labels the dataset with descriptive var names.
#       Used measurement names with mean/std and X/Y/Z from the original datasets
#       Added "subject" and "activity" columns and labeled as such
names(bothsets)

#----5. From the dataset in step 4, creates a second, independent tidy dataset
#       with the average of each variable for each activity and each subject.
cnames<-names(bothsets)
jmn<-grep("mean...[XYZ]",cnames)  #pick out the "mean" features for x,y,z
tidymeans<-data.frame()
activity<-unique(bothsets$activity)
for (sbj in 1:6) {
   for (act in activity){
      c<-filter(bothsets,subject==sbj, activity==act)[,-(49:50)]
      tidymeans<-rbind(tidymeans,c(apply(c,2,mean)))
   }
}
tidymeans$subject  <-rep(1:6,each=6)
tidymeans$activity <-rep(activity,6)
names(tidymeans)<-cnames

#--save new tidy dataset
write.table(tidymeans,file="tidymeans.txt",row.name=F) 

# Create run_analysis.R to
# 1. merges the traning and the test sets to create 1 dataset
# 2. extracts only the measurements on the mean & sd for each measurement
# 3. Uses descriptive activity names to name the activities in the dataset
# 4. Appropriately labels the dataset with descriptive var names.
# 5. From the dataset in step 4, creates a second, independent tidy dataset
#    with the average of each variable for each activity and each subject.
