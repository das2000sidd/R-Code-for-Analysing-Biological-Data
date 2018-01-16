train.data = read.table(file='train_data.txt',header = FALSE,sep = ',') 
library(data.table)
summary(train.data)
setnames(train.data,old=c('V1','V28','V29'),new=c('SubId','UPDRS','CLassification'))
train.data$UPDRS = NULL
## UPDRS is for regression
train.data$CLassification = as.factor(train.data$CLassification)
train.data$SubId = NULL
train.data$V26 = NULL ## posing problems
library(andrews)
x=rep(1:26,40)
train.data.new = cbind(x,train.data)

train.data.new$x = as.factor(train.data.new$x)
plot(train.data.new[,-1])
andrews(df=train.data.new,clr=1,main='Andrews Plot for Voice Samples Measured',type = 2)
cor.matrix.train = cor(train.data[,1:25])
length(which(cor.matrix.train>0.6))/length(cor.matrix.train) ## 17%
length(which(cor.matrix.train < -0.6))/length(cor.matrix.train) ## 6%
library(psych)
cortest.bartlett(cor.matrix.train,n=nrow(train.data))

na.check = function(x){
  length(which(is.na(x)==TRUE))/length(x)*100
}
train.na.check = apply(train.data,2,na.check)
length(which(train.na.check==0))/length(train.na.check) ## no missing data
##train.data.cmpl = train.data[complete.cases(train.data),]
train.data.scaled=scale(train.data[,1:25],center=T,scale=T)
train.data.scaled.pca<-prcomp(train.data[,1:25],retx = TRUE,center = TRUE,scale. = TRUE) ## svd of data matrix
biplot(train.data.scaled.pca) ## MUST DO THIS
attributes(train.data.scaled.pca)
plot(train.data.scaled.pca) ## first two enough for explaining variance as per plot
train.data.scaled.pca$rotation
variances = train.data.scaled.pca$sdev^2
sum(variances[1:7])/sum(variances) ## 90% variation explained by first 7 principal components
PCA.scores<-train.data.scaled%*%train.data.scaled.pca$rotation[,1:7]##exact same values







andrews(df=PCA.scores.new,clr=1,main='Andrews Plot for Voice Samples Measured',type = 2)



library(psych)
## ACTUAL CLASSIFICATION LABEL
PCA.scores.df = as.data.frame(PCA.scores)
PCA.scores.df = cbind(PCA.scores.df,train.data$CLassification)

plot(PCA.scores.df[,1],PCA.scores.df[,2],col=PCA.scores.df[,8])## not good enough for separation
pairs.panels(PCA.scores.df[,-8],gap=0,bg=c('red','green')[PCA.scores.df$`train.data$CLassification`],pch=21) ## all uncorrelated
## above plot shows not good separation
distance.matrix = dist(PCA.scores.df[,-8])
kc<-kmeans(distance.matrix,2,50) ## not good clsuter identification

attributes(kc)
kc$cluster
kc$size ## gives 2 clusters, does not do well in identifying cluster sizes
summary(train.data$CLassification)
kc$tot.withinss ## 27063450
kc$betweenss ## 30483899
kc$iter
library(MASS)
linear.disc = lda(PCA.scores.df$`train.data$CLassification`~.,PCA.scores.df,prior=c(1,1)/2)
linear.disc## 2 classes, so one linear discriminants function
linear.disc$prior
linear.disc$counts
linear.disc$scaling
prop = linear.disc$svd^2/sum(linear.disc$svd^2)## amount of between group variance explained
p1 = predict(linear.disc,PCA.scores.df)
ldahist(data=p1$x[,1],g=PCA.scores.df$`train.data$CLassification`) ## BAD GROUPING

p2 = predict(linear.disc,PCA.scores.df)$class
tab = table(Predicted=p2,Actual = PCA.scores.df$`train.data$CLassification`)
tab
lda.train.accuracy = sum(diag(tab))/sum(tab)*100 ## not good with 64% accuracy
lda.train.accuracy = round(lda.train.accuracy,1)


test.data = read.table(file='test_data.txt',header = FALSE,sep = ',') 
summary(test.data$V28)
test.data$V28 = as.factor(test.data$V28)
setnames(test.data,old=c('V1','V28'),new=c('SubId','CLassification'))

## UPDRS is for regression
test.data$CLassification = as.factor(test.data$CLassification)
summary(test.data$CLassification)
test.data$SubId = NULL
test.data$V26 = NULL ## posing problems
test.na.check = apply(test.data,2,na.check)
length(which(test.na.check==0))/length(test.na.check) 
test.data.scaled=scale(test.data[,1:25],center=T,scale=T)
test.data.scaled.pca<-prcomp(test.data[,1:25],retx = TRUE,center = TRUE,scale. = TRUE)
attributes(test.data.scaled.pca)
test.data.PCA.scores<-test.data.scaled[,1:25]%*%test.data.scaled.pca$rotation[,1:7]
pairs.panels(test.data.PCA.scores[,-8],gap=0,bg=c('red','green')[test.data$CLassification],pch=21) ## all uncorrelated

PCA.scores.With.class.test = cbind(test.data.PCA.scores,test.data$CLassification)
test.data.PCA.scores.df = as.data.frame(test.data.PCA.scores)
lda.test.pred = predict(linear.disc,test.data.PCA.scores.df)$class
lda.test.conf = table(Predicted=lda.test.pred,Actual =test.data$CLassification)
lda.test.conf
lda.test.accu = lda.test.conf[1,1]/sum(lda.test.conf)*100 ## 54% accuracy
lda.test.accu = round(lda.test.accu,1)

## Building a logistic regression model
logitmodel<-glm(PCA.scores.df$`train.data$CLassification`~.,family=binomial("logit"),PCA.scores.df,control = list(maxit = 50))
summary(logitmodel)
confint(logitmodel)
logistic.pred.train = predict(logitmodel,PCA.scores.df[,1:7],type = 'response')
logistic.pred.train.class = ifelse(logistic.pred.train>0.5,1,0)
conf.mat.logistic.train = table(PCA.scores.df$`train.data$CLassification`,logistic.pred.train.class)
conf.mat.logistic.train
logistic.train.accu = sum(diag(conf.mat.logistic.train))/sum(conf.mat.logistic.train)*100
logistic.train.accu = round(logistic.train.accu,1)
logistic.pred.test = predict(logitmodel,test.data.PCA.scores.df,type = 'response')


logistic.pred.test.class = ifelse(logistic.pred.test >=0.5,1,0)
logistic.conf.mat=table(logistic.pred.test.class,test.data$CLassification)
logistic.reg.test.accu=sum(diag(logistic.conf.mat))/sum(logistic.conf.mat)*100
logistic.reg.test.accu = round(logistic.reg.test.accu,1)

drop1(logitmodel,test="Chi") ## PC1 could be dropped
PCA.scores.df.no.col1 = PCA.scores.df[,-1]
logitmodel2<-glm(PCA.scores.df.no.col1$`train.data$CLassification`~.,family=binomial("logit"),PCA.scores.df.no.col1,control = list(maxit = 50))
logistic.pred.2.train = predict(logitmodel2,PCA.scores.df[,1:7],type = 'response')
logistic.pred.2.train.class = ifelse(logistic.pred.2.train>0.5,1,0)
conf.mat.logistic.pred.2.train = table(train.data$CLassification,logistic.pred.2.train.class)
conf.mat.logistic.pred.2.train
logistic.train.2.accu = sum(diag(conf.mat.logistic.pred.2.train))/sum(conf.mat.logistic.pred.2.train)*100
logistic.train.2.accu = round(logistic.train.accu,1)
logistic.pred.2 = predict(logitmodel2,test.data.PCA.scores.df[,-1],type = 'response')

logistic.pred.2.class = ifelse(logistic.pred.2 >=0.5,1,0)
logistic.conf.mat.2=table(test.data$CLassification,logistic.pred.2.class)
logistic.test.accu.2 =logistic.conf.mat.2[1,1]/sum(logistic.conf.mat.2)*100 ## 54% percent accuracy
logistic.test.accu.2 = round(logistic.test.accu.2,1)
exp(confint(logitmodel)) ## weak effects
exp(confint(logitmodel2)) ## weak effects
## Building a regression tree

library(tree)
fit.tree <- tree(PCA.scores.df$`train.data$CLassification`~., data = PCA.scores.df)
plot(fit.tree)
text(fit.tree,srt=0,adj=1)
tree.train.pred = predict(fit.tree,PCA.scores.df,type='class')
conf.mat.train.tree=table(tree.train.pred,PCA.scores.df$`train.data$CLassification`)
conf.mat.train.tree
tree.train.pred.accu = sum(diag(conf.mat.train.tree))/sum(conf.mat.train.tree)*100 ## 35% accuracy
tree.train.pred.accu = round(tree.train.pred.accu,1)

tree.test.pred = predict(fit.tree,test.data.PCA.scores.df,type='class')
conf.mat.test.tree=table(tree.test.pred,test.data$CLassification)
conf.mat.test.tree
tree.test.pred.accu = conf.mat.test.tree[2,1]/sum(conf.mat.test.tree)*100 ## 35% accuracy
tree.test.pred.accu = round(tree.test.pred.accu,1)

cv.fit.tree = cv.tree(fit.tree,K=10)
plot(cv.fit.tree,type='b')
pruned.tree.new <- prune.tree(fit.tree,best=4)
plot(pruned.tree.new)
text(pruned.tree.new,srt=0,adj=1)

pruned.tree.train.pred = predict(pruned.tree.new,PCA.scores.df,type='class')
conf.mat.train.pruned.tree=table(pruned.tree.train.pred,PCA.scores.df$`train.data$CLassification`)
conf.mat.train.pruned.tree
pruned.tree.train.pred.accu = sum(diag(conf.mat.train.pruned.tree))/sum(conf.mat.train.pruned.tree)*100 ## 35% accuracy
pruned.tree.train.pred.accu = round(pruned.tree.train.pred.accu,1)


pruned.tree.test.pred = predict(pruned.tree.new,test.data.PCA.scores.df,type = 'class')

pruned.tree.test.conf.mat = table(pruned.tree.test.pred,test.data$CLassification)
pruned.tree.test.conf.mat
pruned.tree.test.accu=pruned.tree.test.conf.mat[2,1]/sum(pruned.tree.test.conf.mat)*100 ## 36.3%
pruned.tree.test.accu = round(pruned.tree.test.accu,1)
library(randomForest)
tneRF.res = tuneRF(PCA.scores.df[,-8],PCA.scores.df[,8],stepFactor = 1,plot=TRUE,ntreeTry = 500,trace = TRUE,improve=0.05)

rf=randomForest(PCA.scores.df$`train.data$CLassification`~.,data=PCA.scores.df,importance=TRUE,ntree=500,nodesize=50,mtry=2)

rf.train.pred = predict(rf,PCA.scores.df,type='class')
conf.mat.train.rf=table(rf.train.pred,PCA.scores.df$`train.data$CLassification`)
conf.mat.train.rf

rf.train.pred.accu = sum(diag(conf.mat.train.rf))/sum(conf.mat.train.rf)*100 ## 35% accuracy
rf.train.pred.accu = round(rf.train.pred.accu,1)

pred.rf = predict(rf,test.data.PCA.scores.df,type = 'class')

rf.test.conf.mat = table(test.data$CLassification,pred.rf)
rf.test.conf.mat
rf.test.accu = rf.test.conf.mat[1,2]/sum(rf.test.conf.mat)*100 ## 55% accurate
rf.test.accu = round(rf.test.accu,1)
attributes(rf)
plot(rf)
print(rf)


##bagging
matrix.of.test.accuracy.bagging=matrix(0,nrow=1000,ncol=1)
matrix.of.train.accuracy.bagging=matrix(0,nrow=1000,ncol=1)

test.data.PCA.with.class = cbind(test.data.PCA.scores.df,test.data$CLassification)
train.PCA.scores.df.bagging = PCA.scores.df
test.data.PCA.scores.df.bagging = test.data.PCA.with.class
train.PCA.scores.df.bagging$Class = train.PCA.scores.df.bagging$`train.data$CLassification`
test.data.PCA.scores.df.bagging$Class = test.data.PCA.with.class$`test.data$CLassification`
train.PCA.scores.df.bagging$`train.data$CLassification` = NULL
test.data.PCA.scores.df.bagging$`test.data$CLassification` = NULL
bagging.df.together = rbind(train.PCA.scores.df.bagging,test.data.PCA.scores.df.bagging)
bagging.df.together$Classification =bagging.df.together$Class
bagging.df.together$Class = NULL
for (i in 1:1000){
  ids = sample(1:nrow(bagging.df.together),nrow(bagging.df.together)/2)
  train.new = bagging.df.together[ids,]
  test.new = bagging.df.together[-ids,]
 
  logitmodel<-glm(Classification~.,family=binomial("logit"),train.new,control = list(maxit = 50))
  logistic.pred.train = predict(logitmodel,train.new[,1:7],type = 'response')
  logistic.pred.test = predict(logitmodel,test.new[,1:7],type = 'response')
  logistic.pred.test.class = ifelse(logistic.pred.test>0.5,1,0)
  logistic.pred.train.class = ifelse(logistic.pred.train>0.5,1,0)
  conf.mat.logistic.test = table(test.new$Class,logistic.pred.test.class)
  conf.mat.logistic.train = table(train.new$Class,logistic.pred.train.class)
  accu.test = sum(diag(conf.mat.logistic.test))/sum(conf.mat.logistic.test)
  accu.train = sum(diag(conf.mat.logistic.train))/sum(conf.mat.logistic.train)
  matrix.of.test.accuracy.bagging[i,1] = accu.test
  matrix.of.train.accuracy.bagging[i,1] = accu.train
  
}
vector.of.bagging.accuracy.test = mean(as.vector(matrix.of.test.accuracy.bagging[,1]))*100
vector.of.bagging.accuracy.train = mean(as.vector(matrix.of.train.accuracy.bagging[,1]))*100
vector.of.bagging.accuracy.test = round(vector.of.bagging.accuracy.test,1)
vector.of.bagging.accuracy.train = round(vector.of.bagging.accuracy.train,1)


all.model.train.accu = rbind(lda.train.accuracy,logistic.train.accu,logistic.train.2.accu,tree.train.pred.accu,pruned.tree.train.pred.accu,rf.train.pred.accu,vector.of.bagging.accuracy.train)
all.model.test.accu = rbind(lda.test.accu,logistic.reg.test.accu,logistic.test.accu.2,tree.test.pred.accu,pruned.tree.test.accu,rf.test.accu,vector.of.bagging.accuracy.test)
type.of.tests = c('LDA','Logistic Reg','Logistic Reg 2','Unpruned Tree','Pruned Tree','Random Forest','Bagging')
accu.mat = cbind(type.of.tests,all.model.train.accu,all.model.test.accu)
accu.df = as.data.frame(accu.mat)
library(data.table)
setnames(accu.df,old=c('type.of.tests','V2','V3'),new=c('Algorithm','Train Accuracy','Test Accuracy'))
library(ggplot2)
##accu.df$`Train Accuracy` = as.numeric(accu.df$`Train Accuracy`)
accu.df$Train.Accuracy = as.numeric(levels(accu.df$`Train Accuracy`)[accu.df$`Train Accuracy`])
accu.df$Test.Accuracy = as.numeric(levels(accu.df$`Test Accuracy`)[accu.df$`Test Accuracy`])



par(mfrow=c(2,2))
train.accuracy.plot = ggplot(accu.df,aes(x=Algorithm,y=Train.Accuracy))+geom_bar(stat='identity',aes(fill=Algorithm),width = 0.3)+ylim(0,100)+ geom_text(size = 2.5,label = accu.df[,4])+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
train.accuracy.plot
test.accuracy.plot=ggplot(accu.df,aes(x=Algorithm,y=Test.Accuracy))+geom_bar(stat='identity',aes(fill=Algorithm),width = 0.3)+ylim(0,100)+ geom_text(size = 2.5,label = accu.df[,5])+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
test.accuracy.plot
library(gridExtra)
grid.arrange(train.accuracy.plot,test.accuracy.plot,ncol=2)

