### Project Description
 
  The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis

### Collection of the raw data
  Human Activity Recognition Using Smartphones Dataset was used

### Notes on the original (raw) data
 - 'README.txt'

 - 'features_info.txt': Shows information about the variables used on the feature vector.

 - 'features.txt': List of all features.

 - 'activity_labels.txt': Links the class labels with their activity name.

 - 'train/X_train.txt': Training set.

 - 'train/y_train.txt': Training labels.

 - 'test/X_test.txt': Test set.

 - 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

 - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

 - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

 - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

 - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

### Creating the tidy datafile
 - Merges the training and the test sets to create one data set.
 - Extracts only the measurements on the mean and standard deviation for each measurement. 
 - Uses descriptive activity names to name the activities in the data set
 - Appropriately labels the data set with descriptive variable names. 
 - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

### Description of the variables in the tidydata.txt file
 - 180 observations of  68 variables
 - subjectNum represents subject ID number
 - activityName reperesents the name of activity 
 - Rest of the variables represent average of each variable for each activity and each subject

### Notes on variables
- subjectNum: Int from 1 to 30
- activityName: Factors with 6 levels; (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING)
