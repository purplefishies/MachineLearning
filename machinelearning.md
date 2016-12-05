---
title: "Identifying Performance of Dumbbell lifts from personal fitness sensors"
author: "Jimi Damon"
date: "12/04/2016"
output: html_document
hitheme: tomorrow
mode: selfcontained
highlighter: highlight.js
framework: io2012
url:
  assets: ../../assets
  lib: ../../librariesNew
widgets: mathjax
---

# Background of the data

One thing that people regularly do is quantify how much of a
particular activity they do, but they rarely quantify how well they do
it. In this project, your goal will be to use data from accelerometers
on the belt, forearm, arm, and dumbell of 6 participants. They were
asked to perform barbell lifts correctly and incorrectly in 5
different ways.  

The classifications for the various ways of lifing dumbbell was as
follows:  Class 'A' corresponded to exactly following the specification for
lifting dumbbell, class 'B' indicated throwing the elbows to the
front, class 'C' indicated only lifting the dumbbell halfway, class
'D' indicated lowering the dumbbell only halfway and class 'E'
indicated throwing the hips to the front while performing a lift.

# Reading the data

I downloaded the training and testing datasets from the URL
https://d396qusza40orc.cloudfront.net/predmachlearn, and saved them to
my local directory. The files / data sets were called
"pml-training.csv" and "pml-testing.csv".


```r
library(ggplot2)
library(caret)
library(randomForest); 
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
```

Exploring the data I found the dimensions of the training data to be 

```
## [1] 19622   160
```


# Data cleaning

I began examining the data and discovered that there were numerous 
columns that contained NA values ( i.e. kurtosis_roll_belt ) . In addition, there were a number of
variables as well that had the same value repeated for the length of
the data frame ( i.e. new_window ). So, I decided that my first method for cleaning the
data was to first remove the zero variance elements and then to remove
the columns that corresponded to the timestamps and indices that had
nothing to do with the training or method of lifting.


```r
nzv_cols <- nearZeroVar( training)
modtraining <- training[,-nzv_cols]
oknames <- names(modtraining)[colSums(is.na(modtraining)) == 0]
nnames <- oknames[grep(".*(num_window|X|timestamp|user_name).*",oknames,invert=TRUE)]
nnames
```

```
##  [1] "roll_belt"            "pitch_belt"           "yaw_belt"            
##  [4] "total_accel_belt"     "gyros_belt_x"         "gyros_belt_y"        
##  [7] "gyros_belt_z"         "accel_belt_x"         "accel_belt_y"        
## [10] "accel_belt_z"         "magnet_belt_x"        "magnet_belt_y"       
## [13] "magnet_belt_z"        "roll_arm"             "pitch_arm"           
## [16] "yaw_arm"              "total_accel_arm"      "gyros_arm_x"         
## [19] "gyros_arm_y"          "gyros_arm_z"          "accel_arm_x"         
## [22] "accel_arm_y"          "accel_arm_z"          "magnet_arm_x"        
## [25] "magnet_arm_y"         "magnet_arm_z"         "roll_dumbbell"       
## [28] "pitch_dumbbell"       "yaw_dumbbell"         "total_accel_dumbbell"
## [31] "gyros_dumbbell_x"     "gyros_dumbbell_y"     "gyros_dumbbell_z"    
## [34] "accel_dumbbell_x"     "accel_dumbbell_y"     "accel_dumbbell_z"    
## [37] "magnet_dumbbell_x"    "magnet_dumbbell_y"    "magnet_dumbbell_z"   
## [40] "roll_forearm"         "pitch_forearm"        "yaw_forearm"         
## [43] "total_accel_forearm"  "gyros_forearm_x"      "gyros_forearm_y"     
## [46] "gyros_forearm_z"      "accel_forearm_x"      "accel_forearm_y"     
## [49] "accel_forearm_z"      "magnet_forearm_x"     "magnet_forearm_y"    
## [52] "magnet_forearm_z"     "classe"
```

# Formula creation

Now I was left with a new variable nnames that contained the filtered
list of names.  I then created a formula for these fields only.

```r
form <- as.formula(paste("classe ~ ",paste(nnames[-length(nnames)],collapse=" + ")))
form
```

```
## classe ~ roll_belt + pitch_belt + yaw_belt + total_accel_belt + 
##     gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x + 
##     accel_belt_y + accel_belt_z + magnet_belt_x + magnet_belt_y + 
##     magnet_belt_z + roll_arm + pitch_arm + yaw_arm + total_accel_arm + 
##     gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + accel_arm_y + 
##     accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z + 
##     roll_dumbbell + pitch_dumbbell + yaw_dumbbell + total_accel_dumbbell + 
##     gyros_dumbbell_x + gyros_dumbbell_y + gyros_dumbbell_z + 
##     accel_dumbbell_x + accel_dumbbell_y + accel_dumbbell_z + 
##     magnet_dumbbell_x + magnet_dumbbell_y + magnet_dumbbell_z + 
##     roll_forearm + pitch_forearm + yaw_forearm + total_accel_forearm + 
##     gyros_forearm_x + gyros_forearm_y + gyros_forearm_z + accel_forearm_x + 
##     accel_forearm_y + accel_forearm_z + magnet_forearm_x + magnet_forearm_y + 
##     magnet_forearm_z
```

# Data segmentation


```r
set.seed(69)
dataTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
train <- trainData[dataTrain,]
valid <- trainData[-dataTrain,]
```

# Training and cross-validation

I went with a random forest approach to my data model. The reason for this was due to the perceived non-linear aspect of the data in question. I decided to use 5-fold cross-validation for my model due to the fact that it corresponds to a shorter simulation cycle and during my analysis I was runing into severe problems with simulation times balooning.  The training control was set up as follows:


```r
control <- trainControl(method = "cv", number = 5)
```

In addition, nodata transformations were applied to the data as they are considered less
important in non-linear model fitting like random forests. I ran the training method on the "train" subset of the original training data.



```r
set.seed(42)
fit_rf <- train( form, data=train, method="rf", trControl=control ,
                do.trace=TRUE )
```


```
## Random Forest 
## 
## 13737 samples
##    52 predictors
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 10989, 10990, 10990, 10989, 10990 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9893717  0.9865536  0.001881289  0.002378652
##   27    0.9898814  0.9871987  0.001967012  0.002488592
##   52    0.9841306  0.9799222  0.003752686  0.004747967
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

# Out of sample calculation


```r
predata <- predict(mymodel,valid)
confmatrix <- confusionMatrix(valid$classe, predata )
(accuracy <- confmatrix$overall[1])
```

```
##  Accuracy 
## 0.9983008
```
So, for accuracy of 0.991, the out of sample error rate is around .009, or less than 1%.

# Predicting 20 different test cases.

Now I applied this model to the testing data that was provided by the website


```r
(predict(mymodel, testing))
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

