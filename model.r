
merged <- read.csv("merged.csv", header = TRUE)
topbigram <- read.csv("bigrams.csv", header = TRUE)

# building the model #
speaking <- merged[,10]
idscore <- merged[,8]
name_party <- merged[,9]
modeldata <- cbind(idscore,name_party)

# create new columns for bigrams and name the columns #
zeromatrix <- matrix(0,nrow=nrow(modeldata),ncol=500)
modeldata <- cbind(modeldata,zeromatrix)
topbigramname <- t(matrix(topbigram[,1]))


for (i in 1:500) {
  if (i==1) { names <- c(toString(topbigramname[i])) }
  else { names <- c(names,toString(topbigramname[i])) }
}

colnames(modeldata) <- c("IdeologyScore","Name_Party",names)

# check whether the speaker has said those bigrams ＃

for (i in 1:nrow(modeldata)) {
  for (j in 1:500) { 
    if (grepl(names[j],toString(speaking[i]),ignore.case = TRUE, perl = FALSE,fixed = FALSE, useBytes = FALSE) == TRUE) 
      
    { modeldata[i,j] <- 1}
    else 
    { modeldata[i,j] <- 0 }
  }
  
}

# create the Ridge model #

library(glmnet)
library(ISLR)

grid=10^seq(10,-2,length=100)

modeldata <- data.frame(modeldata)

x=model.matrix(IdeologyScore~.,data=modeldata)[,-1]
y=modeldata$IdeologyScore

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,]

# split samples into a training set and a test set #
set.seed(1)
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]

# fit a ridge regression model on the training set, and evaluate its MSE on the test set, using λ = 4 #
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)
# -> test MSE #

# check large lambda and lambda=0 #
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)
lm(y~x,subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

# cross-validation using cv.glmnet() #
set.seed(1)
cv.out=cv.glmnet(x[train ,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
# bestlambda=0.04307892 #

# therefore #
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2)

# refit our ridge regression model on the full data set #
out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:20,]
