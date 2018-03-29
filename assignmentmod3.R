sapply(iris[1:4],var)
range(sapply(iris[1:4],var))
iris.stand <- as.data.frame(scale(iris[,1:4]))
sapply(iris.stand,sd)

pca <- prcomp(iris.stand, center=T,scale=T)
pca
summary(pca)
plot(pca)
abline(h=1)
summary(pca)

cols <- character(nrow(iris))
cols[] <- "black"
cols[iris$Species %in% c("setosa","vertosa")] <- "black"
cols[iris$Species == "virginica"] <- "red"
pairs(pca$x[,1:3], col=cols)

pca <- prcomp(iris[,2:4],center=T,scale. =T, rank.=3)
summary(pca)
pairs(pca$x[,1:2], col=cols)


# look at the full example iris data set (it's pre-loaded in your R)
iris
# first six records
head( iris )
# convert the `Species` column to numeric, so you get 1, 2, 3
as.numeric( iris$Species )
# now actually store that result back on the data frame
iris$Species <- as.numeric( iris$Species )

A <- 3
myClass <- rep(0,dim(iris)[1])
myClass[which(iris$Class==A)] <- 1



require(dismo) # for n-fold calculation
folds <- kfold(dat2, k=10, by=dat2$Class)
source("C:\\Users\\Linda\\R Projects\\UC1levelFunction012818.R")
set.seed(88)








# follow previous wine example to calculate LR using 10-fold cross validation
###############################################


# 10-fold stratified 10% CV 
require(dismo) # for n-fold calculation
folds <- kfold(iris, k=10, by=iris$Class)
source("C:\\Users\\Linda\\R Projects\\UC1levelFunction012818.R")
set.seed(88)
#Make a variable to collect the ground truth and one to collect the predicted p(Class1)
tstGT <- 99
tstprediction <- 99
# generate a for() control structure to loop through the n-CV folds


for(i in 1:10){
  #get training and test data
  wtrain <- iris[folds!=i,]
  wtest <- iris[folds==i,]
  
  # Perform PCA on wtrain, keep 3PCs
  pca <- prcomp(wtrain,center=T,scale. =T, rank.=2)
 
   # Add Class information 
  pca.trn <- data.frame(Class=wtrain$Class, pca$class)
  pca.trn.split <- split(pca.trn, pca.trn$Class)
  C1 <- UC1(pca.trn.split[[2]],c(2:4))
  mn1 <- apply(pca.trn.split[[2]][,2:4],2,mean)
  C0 <- UC1(pca.trn.split[[1]],c(2:4))
  mn0 <- apply(pca.trn.split[[1]][,2:4],2,mean)
 
   # This is one-level data, so calculate LR for all test data
  
  # Project wtest into pca space to get scores
  tst <- predict(pca, wtest)     #[,1:3]
  tst <- data.frame(Class=wtest$Class, tst)
  
  for(j in 1:dim(tst)[1]){
    n<- (det(C1))^-0.5*exp(-0.5*(as.matrix(tst[j,2:4] - mn1))%*%solve(C1)%*%t(as.matrix(tst[j,2:4] - mn1)))
    d<- (det(C0))^-0.5*exp(-0.5*(as.matrix(tst[j,2:4] - mn0))%*%solve(C0)%*%t(as.matrix(tst[j,2:4] - mn0)))
    
    tstprediction <- c(tstprediction , n/d ) 
    tstGT <- c(tstGT, tst$Class[j])
  }
  
}
tstGT <- tstGT[-1]
tstprediction <- tstprediction[-1]
# The predictions are probability that a sample belongs to wClass ==1
# Convert the predictions to log10(prediction/(1-prediction)), which is the LLR
tstprediction <- log10(tstprediction)

#ROCR Curve
library(ROCR)
ROCRpred <- prediction(tstprediction, tstGT)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf,downsampling=1, colorize = TRUE, print.cutoffs.at = c(-4,-2,-1,0,1,2,4),text.adj=c(-.5,.5), lwd = 4, cex.axis=2, cex.lab=1.25, cex=2, lty=2)
abline(0,1, lty=1,lwd=1,col="red")
abline(1,-1,lty=3,lwd=1,col="blue")

auc.ROCperf = performance(ROCRpred, measure = "auc")
auc <- c(unlist(auc.ROCperf@y.values))

auc

