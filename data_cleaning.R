# Read the data.
mHealth.subject1 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject1.log", sep="", header = TRUE)
mHealth.subject2 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject2.log", sep="", header = TRUE)
mHealth.subject3 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject3.log", sep="", header = TRUE)
mHealth.subject4 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject4.log", sep="", header = TRUE)
mHealth.subject5 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject5.log", sep="", header = TRUE)
mHealth.subject6 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject6.log", sep="", header = TRUE)
mHealth.subject7 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject7.log", sep="", header = TRUE)
mHealth.subject8 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject8.log", sep="", header = TRUE)
mHealth.subject9 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject9.log", sep="", header = TRUE)
mHealth.subject10 <- read.csv("D:/msds696/MHEALTHDATASET/MHEALTHDATASET/mHealth_subject10.log", sep="", header = TRUE)

# Create a "subject_id" column.
mHealth.subject1$subject_id <- rep("1", length(mHealth.subject1$accel_chest_x))
mHealth.subject2$subject_id <- rep("2", length(mHealth.subject2$accel_chest_x))
mHealth.subject3$subject_id <- rep("3", length(mHealth.subject3$accel_chest_x))
mHealth.subject4$subject_id <- rep("4", length(mHealth.subject4$accel_chest_x))
mHealth.subject5$subject_id <- rep("5", length(mHealth.subject5$accel_chest_x))
mHealth.subject6$subject_id <- rep("6", length(mHealth.subject6$accel_chest_x))
mHealth.subject7$subject_id <- rep("7", length(mHealth.subject7$accel_chest_x))
mHealth.subject8$subject_id <- rep("8", length(mHealth.subject8$accel_chest_x))
mHealth.subject9$subject_id <- rep("9", length(mHealth.subject9$accel_chest_x))
mHealth.subject10$subject_id <- rep("10", length(mHealth.subject10$accel_chest_x))

# Split off the unknown label data.
mHealth.subject1.test.unknown <- mHealth.subject1[mHealth.subject1$label=="0",]
mHealth.subject2.test.unknown <- mHealth.subject2[mHealth.subject2$label=="0",]
mHealth.subject3.test.unknown <- mHealth.subject3[mHealth.subject3$label=="0",]
mHealth.subject4.test.unknown <- mHealth.subject4[mHealth.subject4$label=="0",]
mHealth.subject5.test.unknown <- mHealth.subject5[mHealth.subject5$label=="0",]
mHealth.subject6.test.unknown <- mHealth.subject6[mHealth.subject6$label=="0",]
mHealth.subject7.test.unknown <- mHealth.subject7[mHealth.subject7$label=="0",]
mHealth.subject8.test.unknown <- mHealth.subject8[mHealth.subject8$label=="0",]
mHealth.subject9.test.unknown <- mHealth.subject9[mHealth.subject9$label=="0",]
mHealth.subject10.test.unknown <- mHealth.subject10[mHealth.subject10$label=="0",]

# Split off the known label data.
mHealth.subject1.known.labels <- mHealth.subject1[mHealth.subject1$label!="0",]
mHealth.subject2.known.labels <- mHealth.subject2[mHealth.subject2$label!="0",]
mHealth.subject3.known.labels <- mHealth.subject3[mHealth.subject3$label!="0",]
mHealth.subject4.known.labels <- mHealth.subject4[mHealth.subject4$label!="0",]
mHealth.subject5.known.labels <- mHealth.subject5[mHealth.subject5$label!="0",]
mHealth.subject6.known.labels <- mHealth.subject6[mHealth.subject6$label!="0",]
mHealth.subject7.known.labels <- mHealth.subject7[mHealth.subject7$label!="0",]
mHealth.subject8.known.labels <- mHealth.subject8[mHealth.subject8$label!="0",]
mHealth.subject9.known.labels <- mHealth.subject9[mHealth.subject9$label!="0",]
mHealth.subject10.known.labels <- mHealth.subject10[mHealth.subject10$label!="0",]

# Create a single data set.
mHealth <- rbind(mHealth.subject1, mHealth.subject2)
mHealth <- rbind(mHealth, mHealth.subject3)
mHealth <- rbind(mHealth, mHealth.subject4)
mHealth <- rbind(mHealth, mHealth.subject5)
mHealth <- rbind(mHealth, mHealth.subject6)
mHealth <- rbind(mHealth, mHealth.subject7)
mHealth <- rbind(mHealth, mHealth.subject8)
mHealth <- rbind(mHealth, mHealth.subject9)
mHealth <- rbind(mHealth, mHealth.subject10)

dim(mHealth)
str(mHealth)

# Labels to be used for stratified sampling.
mHealth.test.unknown.labels <- mHealth[mHealth$label=="0",]
mHealth.known.labels <- mHealth[mHealth$label!="0",]

dim(mHealth.test.unknown.labels)
dim(mHealth.known.labels)

# Stratified sampling package.
library(splitstackshape)

# Seed the random number generator.
set.seed(123)

# Stratified sample for our training set.
mHealth.train <- stratified(mHealth.known.labels, c("subject_id", "label"), 100)
unique(mHealth.train$label)
# [1]  1  2  3  4  5  6  7  8  9 10 11 12
unique(mHealth.train$subject_id)
# [1] "1"  "10" "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" 
 

# Stratified sample for our test set. 
mHealth.test <- stratified(mHealth.known.labels, c("subject_id", "label"), 100)
unique(mHealth.test$label)
# [1]  1  2  3  4  5  6  7  8  9 10 11 12
unique(mHealth.test$subject_id)
# [1] "1"  "10" "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9" 
 
# Stratified sample dimensions. 
dim(mHealth.train)
[1] 12000    25
dim(mHealth.test)
[1] 12000    25
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
