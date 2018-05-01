# Author: Matthew Goralczyk

# Correlations by activity
corrgram(mHealth.train, main="Corrgram - All Activities")
corrgram(mHealth.train[mHealth.train$label=="1",], main="Corrgram - Activity 1")
corrgram(mHealth.train[mHealth.train$label=="2",], main="Corrgram - Activity 2")
corrgram(mHealth.train[mHealth.train$label=="3",], main="Corrgram - Activity 3")
corrgram(mHealth.train[mHealth.train$label=="4",], main="Corrgram - Activity 4")
corrgram(mHealth.train[mHealth.train$label=="5",], main="Corrgram - Activity 5")
corrgram(mHealth.train[mHealth.train$label=="6",], main="Corrgram - Activity 6")
corrgram(mHealth.train[mHealth.train$label=="7",], main="Corrgram - Activity 7")
corrgram(mHealth.train[mHealth.train$label=="8",], main="Corrgram - Activity 8")
corrgram(mHealth.train[mHealth.train$label=="9",], main="Corrgram - Activity 9")
corrgram(mHealth.train[mHealth.train$label=="10",], main="Corrgram - Activity 10")
corrgram(mHealth.train[mHealth.train$label=="11",], main="Corrgram - Activity 11")
corrgram(mHealth.train[mHealth.train$label=="12",], main="Corrgram - Activity 12")

# Standing still break down by subject.
mHealth.train.label.1 <- mHealth.train[mHealth.train$label=="1"]
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="1"], main="Corrgram - Standing Still - Subject 1")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="2"], main="Corrgram - Standing Still - Subject 2")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="3"], main="Corrgram - Standing Still - Subject 3")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="4"], main="Corrgram - Standing Still - Subject 4")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="5"], main="Corrgram - Standing Still - Subject 5")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="6"], main="Corrgram - Standing Still - Subject 6")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="7"], main="Corrgram - Standing Still - Subject 7")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="8"], main="Corrgram - Standing Still - Subject 8")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="9"], main="Corrgram - Standing Still - Subject 9")
corrgram(mHealth.train.label.1[mHealth.train.label.1$subject_id=="10"], main="Corrgram - Standing Still - Subject 10")

# Cycling break down by subject.
mHealth.train.label.9 <- mHealth.train[mHealth.train$label=="9"]
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="1"], main="Corrgram - Cycling - Subject 1")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="2"], main="Corrgram - Cycling - Subject 2")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="3"], main="Corrgram - Cycling - Subject 3")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="4"], main="Corrgram - Cycling - Subject 4")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="5"], main="Corrgram - Cycling - Subject 5")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="6"], main="Corrgram - Cycling - Subject 6")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="7"], main="Corrgram - Cycling - Subject 7")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="8"], main="Corrgram - Cycling - Subject 8")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="9"], main="Corrgram - Cycling - Subject 9")
corrgram(mHealth.train.label.9[mHealth.train.label.1$subject_id=="10"], main="Corrgram - Cycling - Subject 10")

# Histrograms of the features broken down by type of exercise (label) as well as the participant.
library(lattice)

histogram(~ ec_1 | subject_id, data=mHealth.train)
histogram(~ ec_2 | subject_id, data=mHealth.train)
histogram(~ ec_1 | label, data=mHealth.train)
histogram(~ ec_2 | label, data=mHealth.train)

histogram(~ accel_chest_x | subject_id, data=mHealth.train)
histogram(~ accel_chest_y | subject_id, data=mHealth.train)
histogram(~ accel_chest_z | subject_id, data=mHealth.train)
histogram(~ accel_chest_x | label, data=mHealth.train)
histogram(~ accel_chest_y | label, data=mHealth.train)
histogram(~ accel_chest_z | label, data=mHealth.train)

histogram(~ accel_l_ankle_x| subject_id, data=mHealth.train)
histogram(~ accel_l_ankle_y| subject_id, data=mHealth.train)
histogram(~ accel_l_ankle_z| subject_id, data=mHealth.train)
histogram(~ accel_l_ankle_x| label, data=mHealth.train)
histogram(~ accel_l_ankle_y| label, data=mHealth.train)
histogram(~ accel_l_ankle_z| label, data=mHealth.train)

histogram(~ gyro_l_ankle_x | subject_id, data=mHealth.train)
histogram(~ gyro_l_ankle_y | subject_id, data=mHealth.train)
histogram(~ gyro_l_ankle_z | subject_id, data=mHealth.train)
histogram(~ gyro_l_ankle_x | label, data=mHealth.train)
histogram(~ gyro_l_ankle_y | label, data=mHealth.train)
histogram(~ gyro_l_ankle_z | label, data=mHealth.train)

histogram(~ mag_l_ankle_x | subject_id, data=mHealth.train)
histogram(~ mag_l_ankle_y | subject_id, data=mHealth.train)
histogram(~ mag_l_ankle_z | subject_id, data=mHealth.train)
histogram(~ mag_l_ankle_x | label, data=mHealth.train)
histogram(~ mag_l_ankle_y | label, data=mHealth.train)
histogram(~ mag_l_ankle_z | label, data=mHealth.train)

histogram(~ accel_r_arm_z | subject_id, data=mHealth.train)
histogram(~ accel_r_arm_y | subject_id, data=mHealth.train)
histogram(~ accel_r_arm_z | subject_id, data=mHealth.train)
histogram(~ accel_r_arm_z | label, data=mHealth.train)
histogram(~ accel_r_arm_y | label, data=mHealth.train)
histogram(~ accel_r_arm_z | label, data=mHealth.train)

histogram(~ gyro_r_arm_x | subject_id, data=mHealth.train)
histogram(~ gyro_r_arm_y | subject_id, data=mHealth.train)
histogram(~ gyro_r_arm_z | subject_id, data=mHealth.train)
histogram(~ gyro_r_arm_x | label, data=mHealth.train)
histogram(~ gyro_r_arm_y | label, data=mHealth.train)
histogram(~ gyro_r_arm_z | label, data=mHealth.train)

histogram(~ mag_r_arm_x | subject_id, data=mHealth.train)
histogram(~ mag_r_arm_y | subject_id, data=mHealth.train)
histogram(~ mag_r_arm_z | subject_id, data=mHealth.train)
histogram(~ mag_r_arm_x | label, data=mHealth.train)
histogram(~ mag_r_arm_y | label, data=mHealth.train)
histogram(~ mag_r_arm_z | label, data=mHealth.train)

library(plyr)

# Heatmaps comparing the different exercises verus the different features.
groupColumns <- ("label")
dataColumns <- c("accel_chest_x", "accel_chest_y", "accel_chest_z")
label.v.mag.arm <- ddply(mHealth.train, groupColumns, function(x) colMeans(x[dataColumns]))
row.names(label.v.mag.arm) <- label.v.mag.arm$label
label.v.mag.arm$label <- NULL
heatmap(as.matrix(label.v.mag.arm), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "yellow", "orange", "red"))(256), scale="column", margins=c(5,10))

groupColumns <- ("label")
dataColumns <- c("accel_l_ankle_x", "accel_l_ankle_y", "accel_l_ankle_z")
label.v.mag.arm <- ddply(mHealth.train, groupColumns, function(x) colMeans(x[dataColumns]))
row.names(label.v.mag.arm) <- label.v.mag.arm$label
label.v.mag.arm$label <- NULL
heatmap(as.matrix(label.v.mag.arm), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "yellow", "orange", "red"))(256), scale="column", margins=c(5,10))

groupColumns <- ("label")
dataColumns <- c("gyro_l_ankle_x", "gyro_l_ankle_y", "gyro_l_ankle_z")
label.v.mag.arm <- ddply(mHealth.train, groupColumns, function(x) colMeans(x[dataColumns]))
row.names(label.v.mag.arm) <- label.v.mag.arm$label
label.v.mag.arm$label <- NULL
heatmap(as.matrix(label.v.mag.arm), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "yellow", "orange", "red"))(256), scale="column", margins=c(5,10))

groupColumns <- ("label")
dataColumns <- c("mag_l_ankle_x", "mag_l_ankle_y", "mag_l_ankle_z")
label.v.mag.arm <- ddply(mHealth.train, groupColumns, function(x) colMeans(x[dataColumns]))
row.names(label.v.mag.arm) <- label.v.mag.arm$label
label.v.mag.arm$label <- NULL
heatmap(as.matrix(label.v.mag.arm), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "yellow", "orange", "red"))(256), scale="column", margins=c(5,10))

groupColumns <- ("label")
dataColumns <- c("accel_r_arm_x", "accel_r_arm_y", "accel_r_arm_z")
label.v.mag.arm <- ddply(mHealth.train, groupColumns, function(x) colMeans(x[dataColumns]))
row.names(label.v.mag.arm) <- label.v.mag.arm$label
label.v.mag.arm$label <- NULL
heatmap(as.matrix(label.v.mag.arm), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "yellow", "orange", "red"))(256), scale="column", margins=c(5,10))

groupColumns <- ("label")
dataColumns <- c("gyro_r_arm_x", "gyro_r_arm_y", "gyro_r_arm_z")
label.v.mag.arm <- ddply(mHealth.train, groupColumns, function(x) colMeans(x[dataColumns]))
row.names(label.v.mag.arm) <- label.v.mag.arm$label
label.v.mag.arm$label <- NULL
heatmap(as.matrix(label.v.mag.arm), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "yellow", "orange", "red"))(256), scale="column", margins=c(5,10))

groupColumns <- ("label")
dataColumns <- c("mag_r_arm_x", "mag_r_arm_y", "mag_r_arm_z")
label.v.mag.arm <- ddply(mHealth.train, groupColumns, function(x) colMeans(x[dataColumns]))
row.names(label.v.mag.arm) <- label.v.mag.arm$label
label.v.mag.arm$label <- NULL
heatmap(as.matrix(label.v.mag.arm), Rowv=NA, Colv=NA, col=colorRampPalette(c("white", "yellow", "orange", "red"))(256), scale="column", margins=c(5,10))

library(plot3D)

# 3D plots of the three-dimensional features.
scatter3D(mHealth.train$accel_chest_x, mHealth.train$accel_chest_y, mHealth.train$accel_chest_z, theta=25, phi=20, pch=18, bty="u", colkey=TRUE, side=1)
scatter3D(mHealth.train$accel_l_ankle_x, mHealth.train$accel_l_ankle_y, mHealth.train$accel_l_ankle_z, theta=250, phi=20, pch=18, bty="u", colkey=TRUE, side=1)
scatter3D(mHealth.train$gyro_l_ankle_x, mHealth.train$gyro_l_ankle_y, mHealth.train$gyro_l_ankle_z, theta=270, phi=25, pch=18, bty="u", colkey=TRUE, side=1)
scatter3D(mHealth.train$mag_l_ankle_x, mHealth.train$mag_l_ankle_y, mHealth.train$mag_l_ankle_z, theta=20, phi=25, pch=18, bty="u", colkey=TRUE, side=1)
scatter3D(mHealth.train$accel_r_arm_x, mHealth.train$accel_r_arm_y, mHealth.train$accel_r_arm_z, theta=40, phi=20, pch=18, bty="u", colkey=TRUE, side=1)
scatter3D(mHealth.train$gyro_r_arm_x, mHealth.train$gyro_r_arm_y, mHealth.train$gyro_r_arm_z, theta=150, phi=20, pch=18, bty="u", colkey=TRUE, side=1)
scatter3D(mHealth.train$mag_r_arm_x, mHealth.train$mag_r_arm_y, mHealth.train$mag_r_arm_z, theta=15, phi=20, pch=18, bty="u", colkey=TRUE, side=1)

# Gyro right arm, x vs y
qplot(gyro_r_arm_x, gyro_r_arm_y, colour = mHealth.train$label, data = mHealth.train)
# Gyro right arm, x vs z
qplot(gyro_r_arm_x, gyro_r_arm_z, colour = mHealth.train$label, data = mHealth.train)
# Gyro right arm, y vs z
qplot(gyro_r_arm_y, gyro_r_arm_z, colour = mHealth.train$label, data = mHealth.train)

# Gyro vs magnetometer and accelerometer
qplot(gyro_r_arm_x, accel_r_arm_x, colour = mHealth.train$label, data = mHealth.train)
qplot(gyro_r_arm_z, mag_r_arm_z, colour = mHealth.train$label, data = mHealth.train)
