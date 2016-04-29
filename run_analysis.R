
# ==========================
# 1. Setting environment
# 1.1 Setting working directory
mOldWorkDir = getwd();
mNewWorkDir = paste(mOldWorkDir, 'assignment4', sep = '/')
if ( !dir.exists( mNewWorkDir ) ) {
  dir.create( mNewWorkDir )
}
setwd( mNewWorkDir )

# 1.2 Downloading and unzipping archive with source dataset
download.file( 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip', destfile = 'dataset.zip' )
unzip( 'dataset.zip' )

# 1.3 Setting paths to source files
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

# ==========================
# 2. Reading catalog of activity lables and their ids from source file
mActivityLables <- read.table( mPathToActivityLabels,
                               sep = '',
                               header = FALSE,
                               col.names = c( "id", "activity_label" ),
                               stringsAsFactors = FALSE )

# ==========================
# 3. Reading catalog of features from source file
mFeatures <- read.table( mPathToFeatures,
                         sep = '',
                         header = FALSE,
                         col.names = c( "id", "feature_name" ),
                         stringsAsFactors = FALSE )
#   Filter only those features that are means of stds
#   (i.e. have 'mean()' or 'std()' substrings in their names)
mMeanAndStdIndex = grepl( '(mean|std)\\(\\)', mFeatures$feature_name )
#   Add 7 spesial features at the end that are also means
mMeanAndStdIndex[555:561] <- TRUE
mFeatures <- filter( mFeatures, mMeanAndStdIndex )
#   Make features human readable
mFeatures$feature_name <- gsub('Acc', 'Accelerator', mFeatures$feature_name )
mFeatures$feature_name <- gsub('Gyro', 'Gyroscope', mFeatures$feature_name )
mFeatures$feature_name <- gsub('Mag', 'Magnitude', mFeatures$feature_name )
mFeatures$feature_name <- gsub('^t', 'time', mFeatures$feature_name )
mFeatures$feature_name <- gsub('^f', 'frequency', mFeatures$feature_name )
remove(mMeanAndStdIndex)



# ==========================
# 4.  Reading testing data
# 4.1 Reading catalog of subjects of test dataset from source file
mSubjectTest <- read.table( mPathToTestSubject,
                            sep = '',
                            header = FALSE,
                            col.names = c( "subject_id" ) )
#    Replace numeric subject ids with more human readable ids
mSubjectTest$subject_id <- as.character( mSubjectTest$subject_id )
mSubjectTest$subject_id <- paste('volunteer_', mSubjectTest$subject_id, sep = '' )


# 4.2 Reading testing measurements from source file and select only those
#     columns that we are interested in. Also set columns names
mXTest <- read.csv( mPathToTestX, sep = '', header = FALSE )
mXTest <- select(mXTest, mFeatures$id)
colnames(mXTest) <- mFeatures$feature_name

# 4.3 Reading testing activity ids from source file
mYTest <- read.csv( mPathToTestY, sep = '',
                    header = FALSE,
                    col.names = c( 'activity_id' ) )
#     To provide human-readable activity lables instead of testing activity ids
#     we join testing activity ids with with catalog of activity lables
#     and select only names of activities as a result column
mYTest <- mYTest %>%
          merge( mActivityLables, by.x = 'activity_id', by.y ='id' ) %>%
          select(activity_label)

# 4.4 At this point we can form a testing dataset by adding
#     testing activities column and testing subject_ids column
#     to testing measurements dataset
mTestDF <- cbind(mXTest, mYTest, mSubjectTest)

# 4.5 To finish processing of testing dataset
#     we remove objects we do not need any more
remove( mXTest, mYTest, mSubjectTest )


# ==========================
# 5.  Reading training data (very similar to reading testing data)
# 5.1 Reading catalog of subjects of train dataset from source file
mSubjectTrain <- read.table( mPathToTrainSubject,
                             sep = '',
                             header = FALSE,
                             col.names = c( "subject_id" ) )
#    Replace numeric subject ids with more human readable ids
mSubjectTrain$subject_id <- as.character( mSubjectTrain$subject_id )
mSubjectTrain$subject_id <- paste('volunteer_', mSubjectTrain$subject_id, sep = '' )

# 5.2 Reading training measurements from source file and select only those
#     columns that we are interested in. Also set columns names
mXTrain <- read.csv( mPathToTrainX, sep = '', header = FALSE )
mXTrain <- select( mXTrain, mFeatures$id )
colnames( mXTrain ) <- mFeatures$feature_name

# 5.3 Reading training activity ids from source file
mYTrain <- read.csv( mPathToTrainY,
                     sep = '',
                     header = FALSE,
                     col.names = c('activity_id') )
#     To provide human-readable activity lables instead of activity ids
#     we join training activity ids with with catalog of activity lables
#     and select only names of activities as a result column
mYTrain <- mYTrain %>%
           merge( mActivityLables, by.x = 'activity_id', by.y ='id' ) %>%
           select( activity_label )

# 5.4 At this point we can form a training dataset by adding
#     training activities column and training subject_ids column
#     to training measurements dataset
mTrainDF <- cbind( mXTrain, mYTrain, mSubjectTrain )

# 5.5 To finish processing of training dataset
#     we remove objects we do not need any more
remove( mXTrain, mYTrain, mSubjectTrain )
remove( mFeatures, mActivityLables )


# ==========================
# 6. Create resulting dataset by binding rows
#    of test and train datasets
resultDataSet <- rbind( mTestDF, mTrainDF )
remove( mTestDF, mTrainDF )

# ==========================
# 7. Create additional resulting dataset with
#    means of all measures by groups of subject ids and activity labels
resultDataSetGrouped <- resultDataSet %>%
                        group_by( activity_label, subject_id ) %>%
                        summarise_each( funs(mean) )

#    save result grouped dataset to file
write.table( resultDataSetGrouped, file = 'groupedds.txt', row.name=FALSE ) 

# ==========================
# 8. Restore environment
setwd( mOldWorkDir )
remove( mBasePath, mPathToFeatures, mPathToActivityLabels )
remove( mPathToTrainX, mPathToTrainY, mPathToTrainSubject )
remove( mPathToTestX, mPathToTestY, mPathToTestSubject )
remove( mOldWorkDir, mNewWorkDir )
