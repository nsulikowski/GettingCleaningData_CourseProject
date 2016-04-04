library(dplyr)

#############################################################################
# 1 - Merges the training and the test sets to create one data set ("data")
#############################################################################

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", stringsAsFactors = FALSE, col.names = c("index","name"))
features <- read.table("./UCI HAR Dataset/features.txt", stringsAsFactors = FALSE, col.names = c("columnindex","name"))

#----------test_data -----------
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", header=FALSE, col.names = c("subject"))
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", header=FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", stringsAsFactors = FALSE, col.names = c("activity_index"))

test_data <- cbind( 
  subject = subject_test$subject, 
  activity_index = y_test$activity_index, 
  X_test
)

#----------build train_data -----------
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", header=FALSE, col.names = c("subject"))
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", header=FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", stringsAsFactors = FALSE, col.names = c("activity_index"))

train_data <- cbind( 
  subject = subject_train$subject, 
  activity_index = y_train$activity_index, 
  X_train
)

test_train_data <- rbind(test_data, train_data)

###############################################################################################
# 2 - Extracts only the measurements on the mean and standard deviation for each measurement. 
# 4 - Appropriately labels the data set with descriptive variable names. 
###############################################################################################
mean_std_columns <- grep("^.*-(mean()|std()).*$", features$name)
mean_std_column_names <- gsub("[()]","", gsub("-","_", features$name[mean_std_columns]))

data = test_train_data[, c(1,2, mean_std_columns+2) ]
names(data) <- c( 
  head(names(test_train_data),2), 
  mean_std_column_names
)

###############################################################################################
# 3 - Uses descriptive activity names to name the activities in the data set
###############################################################################################
data = cbind( 
  subject = data$subject,
  activity_name = activity_labels$name[ data$activity_index ],
  select(data, -(subject:activity_index))
) 
data <- tbl_df(data)

###############################################################################################
# 4 - From the data set in step 4, creates a second, independent tidy data set with the average 
# of each variable for each activity and each subject.
###############################################################################################
average_by_activity_subject <- data %>% 
  group_by(subject, activity_name) %>%
  summarize_each(funs(mean))
  
write.table(average_by_activity_subject, file="average_by_activity_subject.csv", row.name=FALSE ) 


