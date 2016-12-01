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


Decision Trees

QAP(Downside of Decision Tree problems,
    LIST(tendency to overfit / esp without pruning,
         Have high training variance: samples from the same population can
         produce very different trees,
         learn non-monotone relationships with generalized additive
         models
         )
    )


Advanced Methods

- Reduce training variance with bagging / random forrests
- 
- 

calcAUC <- function(predcol, outcol ) {
    perf <- performance( predicton(predcol, outcol==pos ),'auc' )
    as.numeric( perf@y.values )
}


QAP(AUC,LIST(Area under curve , performance of measure ))



# Covariate creation

library(ISLR); library(caret); data(Wage);
inTrain <- createDataPartition(y=Wage$wage,
                              p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

# Dummy variable creation

dummies <- dummyVars(wage ~ jobclass,data=training)
head(predict(dummies,newdata=training))


# Remove zero covariates
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv




# Ggplot with spline


n <- 10
d <- data.frame(x = 1:n, y = rnorm(n))
ggplot(d,aes(x,y)) + geom_point() + geom_line(data=data.frame(spline(d, n=n*10)))


#
#
n <- 10
d <- data.frame(x = 1:n, y = rnorm(n))

fm <- lm( y ~ bs(x,df=4),data=d)
ggplot(d,aes(x,y)) + geom_point() + geom_line(data=data.frame(x=xx,y=predict(fm,xx)))

ggplot(d,aes(x,y)) + geom_point() + geom_line(data=data.frame(x=xx,y=predict(lm( y ~ bs(x,df=4),data=d),xx)))


# back to the training of spam
x <- seq(from=min(training$age),to=max(training$age),by=0.1)
xx <- data.frame(age=x)
fm <- lm(wage ~ bs(age,df=3),data=training)
predict(fm,xx)

tmpdat <- data.frame(x=x,y=predict(fm,xx))
qplot(training$age,training$wage )  + geom_line(data=tmpdat)
# qplot(training$age,training$wage) + geom_line( data=data.frame(x=xx,y=predict(fm,xx)))
Need to ggplot the points first
