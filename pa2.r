install.packages("C50")
library(C50)

colLabels <- c("duration", "protocol_type", "service", "flag", "src_bytes", "dst_bytes", "land", 
       "wrong_fragment", "urgent", "hot", "num_failed_logins", "logged_in", "num_compromised",
       "root_shell", "su_attempted", "num_root", "num_file_creations", "num_shells", 
       "num_access_files", "num_outbound_cmds", "is_host_login", "is_guest_login", "count",
       "srv_count", "serror_rate", "srv_serror_rate", "rerror_rate", "srv_rerror_rate",
       "same_srv_rate", "diff_srv_rate", "srv_diff_host_rate", "dst_host_count", 
       "dst_host_srv_count", "dst_host_same_srv_rate", "dst_host_diff_srv_rate", 
       "dst_host_same_src_port_rate", "dst_host_srv_diff_host_rate", "dst_host_serror_rate",
       "dst_host_srv_serror_rate", "dst_host_rerror_rate", "dst_host_srv_rerror_rate")

# reading the csv file. storing the data in a dataframe. 
trainData = read.csv("kddcup_data_10_percent.csv", header = FALSE)
colnames(trainData) <- colLabels

# x is the dataframe of predictors. y is the factor vector
x <- trainData[, 1:41]
y <- trainData[, 42]

# trains the C5.0 decision tree to generate the node "treeModel"
treeModel <- C50::C5.0(x, y)
summary(treeModel)

# getting the training_accuracy
predictedLabels <- predict(treeModel, x, type = "class")
trainingAccuracy <- sum(as.character(predictedLabels) == as.character(y)) / length(y)
trainingAccuracy

# generates the rules from the same training data
treeModel2 <- C50::C5.0(x, y, rules = TRUE)
summary(treeModel2) 

# predicts the labels of the TCP records found in kddcup_testdata_unlabeled_10_percent.csv and report the accuracy
testData = read.csv("kddcup_testdata_unlabeled_10_percent.csv", header = FALSE)
colnames(testData) <- colLabels

#predictedLabels2 are the predicted values of the unlabeled test data
predictedLabels2 <- predict(treeModel, testData, type = "prob")

# true labels of the test data. This is not originally included in the zip file
# file was downloaded from http://kdd.ics.uci.edu/ to get the accuracy of the predicted labels.
testDataCorrected <- read.csv("corrected.csv", header = FALSE)
colnames(testDataCorrected) <- colLabels
testDataCorrected <- testDataCorrected[,42]

# comparing the labels of the predicted values and the actual values. 
# accuracy = number of matching labels / number of instances
testAccuracy <- sum(as.character(predictedLabels2) == as.character(testDataCorrected)) / length(testDataCorrected)
testAccuracy
