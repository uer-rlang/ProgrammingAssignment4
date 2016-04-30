# ProgrammingAssignment4
Assignment for week 4 of Coursera "Getting and Cleaning Data" course.


# ==========================
# 1. Setting environment
## 1.1 Setting working directory
```{r}
mOldWorkDir = getwd()
mNewWorkDir = paste(mOldWorkDir, 'assignment4', sep = '/')
if ( !dir.exists( mNewWorkDir ) ) {
  dir.create( mNewWorkDir )
}
setwd( mNewWorkDir )
```

## 1.2 Downloading and unzipping archive with source dataset
```{r}
download.file( 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile = 'dataset.zip' )
unzip( 'dataset.zip' )
```

## 1.3 Setting paths to source files
```{r}
mBasePath             <- paste( mNewWorkDir, 'UCI HAR Dataset',         sep = '/' )
mPathToActivityLabels <- paste( mBasePath,   'activity_labels.txt',     sep = '/' )
mPathToFeatures       <- paste( mBasePath,   'features.txt',            sep = '/' )
mPathToTestX          <- paste( mBasePath,   'test/X_test.txt',         sep = '/' )
mPathToTestY          <- paste( mBasePath,   'test/y_test.txt',         sep = '/' )
mPathToTestSubject    <- paste( mBasePath,   'test/subject_test.txt',   sep = '/' )
mPathToTrainX         <- paste( mBasePath,   'train/X_train.txt',       sep = '/' )
mPathToTrainY         <- paste( mBasePath,   'train/y_train.txt',       sep = '/' )
mPathToTrainSubject   <- paste( mBasePath,   'train/subject_train.txt', sep = '/' )
library(dplyr)
```

# ==========================
# 2. Reading catalog of activity lables and their ids from source file
```{r}
mActivityLables <- read.table( mPathToActivityLabels,
                               sep = '',
                               header = FALSE,
                               col.names = c( "id", "activity_label" ),
                               stringsAsFactors = FALSE )
```

# ==========================
# 3. Reading catalog of features from source file
```{r}
mFeatures <- read.table( mPathToFeatures,
                         sep = '',
                         header = FALSE,
                         col.names = c( "id", "feature_name" ),
                         stringsAsFactors = FALSE )
```
Filter only those features that are means of stds (i.e. have 'mean()' or 'std()' substrings in their names)
```{r}
mFeatures <- filter( mFeatures, grepl( '(mean|std)\\(\\)', mFeatures$feature_name ) )
```
Make features human readable
```{r}
mFeatures$feature_name <- gsub('Acc', 'Accelerator', mFeatures$feature_name )
mFeatures$feature_name <- gsub('Gyro', 'Gyroscope', mFeatures$feature_name )
mFeatures$feature_name <- gsub('Mag', 'Magnitude', mFeatures$feature_name )
mFeatures$feature_name <- gsub('^t', 'time', mFeatures$feature_name )
mFeatures$feature_name <- gsub('^f', 'frequency', mFeatures$feature_name )
mFeatures$feature_name <- gsub('mean\\(\\)', 'Mean', mFeatures$feature_name )
mFeatures$feature_name <- gsub('std\\(\\)', 'Std', mFeatures$feature_name )
mFeatures$feature_name <- gsub('-', '', mFeatures$feature_name )
```

# ==========================
# 4.  Reading testing data
## 4.1 Reading catalog of subjects of test dataset from source file
```{r}
mSubjectTest <- read.table( mPathToTestSubject,
                            sep = '',
                            header = FALSE,
                            col.names = c( "subject_id" ) )
```
Replace numeric subject ids with more human readable ids
```{r}
mSubjectTest$subject_id <- as.character( mSubjectTest$subject_id )
mSubjectTest$subject_id <- paste('volunteer_', mSubjectTest$subject_id, sep = '' )
```


## 4.2 Reading testing measurements from source file
Select only those columns that we are interested in. Also set columns names
```{r}
mXTest <- read.csv( mPathToTestX, sep = '', header = FALSE )
mXTest <- select(mXTest, mFeatures$id)
colnames(mXTest) <- mFeatures$feature_name
```

## 4.3 Reading testing activity ids from source file
```{r}
mYTest <- read.csv( mPathToTestY, sep = '',
                    header = FALSE,
                    col.names = c( 'activity_id' ) )
```
To provide human-readable activity lables instead of testing activity ids we join testing activity ids with with catalog of activity lables and select only names of activities as a result column
```{r}
mYTest <- mYTest %>%
          merge( mActivityLables, by.x = 'activity_id', by.y ='id' ) %>%
          select(activity_label)
```

## 4.4 Creating testing dataset
```{r}
mTestDF <- cbind(mXTest, mYTest, mSubjectTest)
```

## 4.5 Remove objects we do not need
```{r}
remove( mXTest, mYTest, mSubjectTest )
```


# ==========================
# 5.  Reading training data (very similar to reading testing data)
## 5.1 Reading catalog of subjects of train dataset from source file
```{r}
mSubjectTrain <- read.table( mPathToTrainSubject,
                             sep = '',
                             header = FALSE,
                             col.names = c( "subject_id" ) )
```
Replace numeric subject ids with more human readable ids
```{r}
mSubjectTrain$subject_id <- as.character( mSubjectTrain$subject_id )
mSubjectTrain$subject_id <- paste('volunteer_', mSubjectTrain$subject_id, sep = '' )
```

## 5.2 Reading training measurements from source file
Select only those columns that we are interested in. Also set columns names
```{r}
mXTrain <- read.csv( mPathToTrainX, sep = '', header = FALSE )
mXTrain <- select( mXTrain, mFeatures$id )
colnames( mXTrain ) <- mFeatures$feature_name
```

## 5.3 Reading training activity ids from source file
```{r}
mYTrain <- read.csv( mPathToTrainY,
                     sep = '',
                     header = FALSE,
                     col.names = c('activity_id') )
```
To provide human-readable activity lables instead of activity ids we join training activity ids with with catalog of activity lables and select only names of activities as a result column
```{r}
mYTrain <- mYTrain %>%
           merge( mActivityLables, by.x = 'activity_id', by.y ='id' ) %>%
           select( activity_label )
```

## 5.4 Creating training dataset
```{r}
mTrainDF <- cbind( mXTrain, mYTrain, mSubjectTrain )
```

## 5.5 Remove objects we do not need
```{r}
remove( mXTrain, mYTrain, mSubjectTrain )
remove( mFeatures, mActivityLables )
```


# ==========================
# 6. Create merged dataset
```{r}
resultDataSet <- rbind( mTestDF, mTrainDF )
remove( mTestDF, mTrainDF )
```

# ==========================
# 7. Create tidy dataset
```{r}
resultDataSetGrouped <- resultDataSet %>%
                        group_by( activity_label, subject_id ) %>%
                        summarise_each( funs(mean) )
```

Save tidy dataset to file
```{r}
write.table( resultDataSetGrouped, file = 'groupedds.txt', row.name=FALSE ) 
```

# ==========================
# 8. Restore environment
```{r}
setwd( mOldWorkDir )
remove( mBasePath, mPathToFeatures, mPathToActivityLabels )
remove( mPathToTrainX, mPathToTrainY, mPathToTrainSubject )
remove( mPathToTestX, mPathToTestY, mPathToTestSubject )
remove( mOldWorkDir, mNewWorkDir )
```
