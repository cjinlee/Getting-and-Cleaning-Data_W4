###Background Information###

  The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

  The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 
  
####For each record it is provided ####
  - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
  - Triaxial Angular velocity from the gyroscope.
  - A 561-feature vector with time and frequency domain variables.
  - Its activity label.
  - An identifier of the subject who carried out the experiment.


###run_analysis.R does the following###

  #####1. Merges the training and the test sets to create one data set.#####
  
    Read train and test files first, and then merge them to get the data table for all data by using rbind(), cbind()


  #####2. Extracts only the measurements on the mean and standard deviation for each measurement.#####
  
    First, get the sequence of rows whose variable name includes mean or standard deviation by using grep()

    Then, extract corresponded rows


  #####3. Uses descriptive activity names to name the activities in the data set
  
    Merge data with activity label which is a primary key by using merge()


  #####4. Appropriately labels the data set with descriptive variable names.##### 
  
    Followings are plan for descriptive labeling by using gsub()

      - prefix 't' should be replaced by 'time'
      
      - 'Acc' should be replaced by 'Accelerometer'
      
      - 'Gyro' should be replaced by 'Gyroscope'
      
      - prefix 'f' should be replaced by 'frequency'
      
      - 'Mag' should be replaced by 'Magnitude'
      
      - 'BodyBody' should be replaced by 'Body'


  #####5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.#####
  
    tidy data set is exported to .txt file by write.table()

    'dplyr' package was used
    
    with aggregate()


####tidydata.txt includes following data.####

  1. Subject ID
  
  2. Descriptive activity names
  
  3. The average of each variable for each activity and each subject.
  


Further details are included in CodeBook.md

