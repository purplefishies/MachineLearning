rule1 <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 2.7] <- "spam"
  prediction[x < 2.40] <- "nonspam"
  prediction[(x >= 2.40 & x <= 2.45)] <- "spam"
  prediction[(x > 2.45 & x <= 2.70)] <- "nonspam"
  return(prediction)
}


# Kernel densitites of spam



ggplot( data=smallSpam) +
    geom_density( aes(x=capitalAve,color=type,linetype=type))


library('ROCR')


# Creating folds

set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,initialWindow=20,
                          horizon=10)
names(folds)

#
#Create time slices


cut2(training$wage, g=3 )
# cutWage
# [ 20.1, 91.7) [ 91.7,118.9) [118.9,318.3] 
#           704           725           673 


# Wages


inTrain <- createDataPartition(y=Wage$wage,
                              p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[‐inTrain,]
dim(training); dim(testing)

library(Hmisc)
cutWage <- cut2(training$wage,g=3)
table(cutWage)

p1 <- qplot(cutWage,age, data=training,fill=cutWage,
      geom=c("boxplot"))
p1



#
# Standardizing 
#

# Preprocessing 
set.seed(32343)
modelFit <‐ train(type ~.,data=training,
                  preProcess=c("center","scale"),method="glm")
modelFit



#Boxcox transformation


imputing data





library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                              p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]




Principle Component ANalysis

- combine predictors if necessary to capture the most data possible
- Reduce noise


QAP(Statistical Benefit of PCA,
    Find a new set of multivariates that are uncorrelated and explain as much variance as possible )

QAP(Benefit of using less variables in PCA,
    Data compression )


ALso SVD, singular value decomposition, matrix solutions






