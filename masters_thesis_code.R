
##The data is meant for the research group of the lab and hence cannot be posted.
## This is the R code used for statistical analysis of data.
## It involves applying machine learning algorithm to model classification of individuals using metadata where the original class label was based on microbiome data 
load("C:/Users/Siddhartha Das/Desktop/ALL_NON_SYSTEM_FILES/MASTERS THESIS PROJECT/VDP2999_META.Rdata")
meta.Data.Cols.Extract = read.csv(file="KnownMetaDataColsToExtract.csv",header = FALSE)
meta.Data.Cols.Chr = as.character(meta.Data.Cols.Extract$V1)
VDP2999.sig.Meta.Data = VDP2999[,names(VDP2999) %in% meta.Data.Cols.Chr]
Alcohol.sig.Meta.Data = ALCOHOL[,names(ALCOHOL) %in% meta.Data.Cols.Chr]
Allergy.sig.Meta.Data = ALLERGY[,names(ALLERGY) %in% meta.Data.Cols.Chr]
Allergy.family.sig.Meta.Data = ALLERGY_FAMILY[,names(ALLERGY_FAMILY) %in% meta.Data.Cols.Chr]
Antibiotics.12montha.family.sig.Meta.Data = ANTIBIOTICS12MONTHS[,names(ANTIBIOTICS12MONTHS) %in% meta.Data.Cols.Chr]
Birth.method.12montha.family.sig.Meta.Data = BIRTH_METHOD[,names(BIRTH_METHOD) %in% meta.Data.Cols.Chr]
Blood.family.sig.Meta.Data = BLOOD[,names(BLOOD) %in% meta.Data.Cols.Chr]
Cancer.sig.Meta.Data =  CANCER[,names(CANCER) %in% meta.Data.Cols.Chr]
Cancer.family.Meta.Data =  CANCER_FAMILY[,names(CANCER_FAMILY) %in% meta.Data.Cols.Chr]
Chocolate.Meta.Data =  CHOCOLATE[,names(CHOCOLATE) %in% meta.Data.Cols.Chr]
CofeeTea.Meta.Data =  COFFEE_TEA[,names(COFFEE_TEA) %in% meta.Data.Cols.Chr]
DiaryPd.Meta.Data =  DIARY_PRODUCT[,names(DIARY_PRODUCT) %in% meta.Data.Cols.Chr]
DietFollow.Meta.Data =  DIET_FOLLOW[,names(DIET_FOLLOW) %in% meta.Data.Cols.Chr]
DiseaseLoadFamily.Meta.Data =  DISEASE_LOAD_FAMILY[,names(DISEASE_LOAD_FAMILY) %in% meta.Data.Cols.Chr]
EggMeat.Meta.Data =  EGG_MEAT[,names(EGG_MEAT) %in% meta.Data.Cols.Chr]
FoodSupplement.Meta.Data =  FOODSUPPLEMENT[,names(FOODSUPPLEMENT) %in% meta.Data.Cols.Chr]
FruitVeg.Meta.Data =  FRUIT_VEG[,names(FRUIT_VEG) %in% meta.Data.Cols.Chr]
GeneralInfo.Meta.Data =  GENERAL_INFO[,names(GENERAL_INFO) %in% meta.Data.Cols.Chr]
GeneralNutriton.Meta.Data =  GENERAL_NUTRITION[,names(GENERAL_NUTRITION) %in% meta.Data.Cols.Chr]
GI.Probiotica.Meta.Data =  GI_PROBIOTICA[,names(GI_PROBIOTICA) %in% meta.Data.Cols.Chr]
Hormon.Meta.Data =  HORMON[,names(HORMON) %in% meta.Data.Cols.Chr]
MedicationSixMonths.Meta.Data =  MEDICATION6MONTHS[,names(MEDICATION6MONTHS) %in% meta.Data.Cols.Chr]
Mental.Meta.Data =  MENTAL[,names(MENTAL) %in% meta.Data.Cols.Chr]
OtherGiTreatment.Meta.Data =  OTHER_GI_TREATMENT[,names(OTHER_GI_TREATMENT) %in% meta.Data.Cols.Chr]
Pets.Meta.Data =  PETS[,names(PETS) %in% meta.Data.Cols.Chr]
SleepSport.Meta.Data =  SLEEP_SPORT[,names(SLEEP_SPORT) %in% meta.Data.Cols.Chr]
Smoking.Meta.Data =  SMOKING[,names(SMOKING) %in% meta.Data.Cols.Chr]
SoftDrinks.Meta.Data =  SOFT_DRINKS[,names(SOFT_DRINKS) %in% meta.Data.Cols.Chr]
Soy.Meta.Data =  SOY[,names(SOY) %in% meta.Data.Cols.Chr]
StoolQuestion.Meta.Data =  STOOL_QUESTION[,names(STOOL_QUESTION) %in% meta.Data.Cols.Chr]
Surgery.Meta.Data =  SURGERY[,names(SURGERY) %in% meta.Data.Cols.Chr]
Teeth.Meta.Data =  TEETH[,names(TEETH) %in% meta.Data.Cols.Chr]
Travel.Meta.Data =  TRAVEL[,names(TRAVEL) %in% meta.Data.Cols.Chr]
Vaccination.Meta.Data =  VACCINATION[,names(VACCINATION) %in% meta.Data.Cols.Chr]
WeightQuestion.Meta.Data =  WEIGHT_QUESTION[,names(WEIGHT_QUESTION) %in% meta.Data.Cols.Chr]
Women.Meta.Data =  WOMEN[,names(WOMEN) %in% meta.Data.Cols.Chr]
Work.Meta.Data =  WORK[,names(WORK) %in% meta.Data.Cols.Chr]
Youth.Meta.Data =  YOUTH[,names(YOUTH) %in% meta.Data.Cols.Chr]
allDataCOmbined = cbind(ALCOHOL,ALLERGY,ALLERGY_FAMILY,ANTIBIOTICS12MONTHS,BIRTH_METHOD,BLOOD,CANCER,CANCER_FAMILY,CANCER_TREATMENT,
                        CHOCOLATE,COFFEE_TEA,DIARY_PRODUCT,DIET_FOLLOW,DISEASE_LOAD_FAMILY,EGG_MEAT,FOODSUPPLEMENT,FRUIT_VEG,
                        GENERAL_INFO,GENERAL_NUTRITION,GI_PROBIOTICA,HORMON,MEDICATION6MONTHS,MENTAL,OTHER_GI_TREATMENT,PETS,
                        SLEEP_SPORT,SMOKING,SOFT_DRINKS,SOY,STOOL_QUESTION,SURGERY,TEETH,TRAVEL,VACCINATION,VDP2999,WEIGHT_QUESTION,
                        WOMEN,WORK,YOUTH)


allDataCOmbined.sig.Meta.Data = allDataCOmbined[,names(allDataCOmbined) %in% meta.Data.Cols.Chr]


write.csv(allDataCOmbined,file="all3000SampleDataCombined.csv",row.names = TRUE,quote = FALSE,col.names = TRUE)

metaDataAsPerNewDF = read.csv(file="MetaData_PaperTable_Label_Matching.csv",header = TRUE,check.names = FALSE)
metaDataComplete = metaDataAsPerNewDF$`Terminology in 3000 sample table`
sig.Meta.Data.Vars.Only = allDataCOmbined[,colnames(allDataCOmbined) %in% metaDataComplete]
sig.Meta.Data.Vars.Cmpl = sig.Meta.Data.Vars.Only[complete.cases(sig.Meta.Data.Vars.Only),]
sig.Meta.Data.Vars.Not.Cmpl = sig.Meta.Data.Vars.Only[!complete.cases(sig.Meta.Data.Vars.Only),]
library(mice)
md.Pattern.Results=md.pattern(sig.Meta.Data.Vars.Only)
## frequency of sick leaves, no of work days and age at first work had 50%,50% and 27% missing. dropping those initially
sig.Meta.Data.Vars.Only.max.ten.percent.missing =sig.Meta.Data.Vars.Only[,-which(names(sig.Meta.Data.Vars.Only) %in% c("frequency_of_sick_leaves_last_year"," number_of_work_daysaweek","age_at_first_work"))]
##LOT OF MISSING DATA- DOING MULTIPLE IMPUTATION

percent.miss = function(x){
  sum(is.na(x))/length(x)*100
}
library(VIM)
aggr_plot <- aggr(sig.Meta.Data.Vars.Only.max.ten.percent.missing, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
missing.feature.percent.by.var = apply(sig.Meta.Data.Vars.Only,1,percent.miss) ## 2 is for cols, 1 is for rows
sort(missing.feature.percent.by.var) ## OVER 5% MISSING IS VERY BAD
sig.MetaDataVars.Indiv.Less.Than.Five.Percent.Missing = sig.Meta.Data.Vars.Only[missing.feature.percent.by.var<5,]##1975 observations,58 features
sig.MetaDataVars.Indiv.Greater.Than.Five.Percent.Missing = sig.Meta.Data.Vars.Only[missing.feature.percent.by.var>5,]
missing.feature.percent.by.Feature = apply(sig.MetaDataVars.Indiv.Less.Than.Five.Percent.Missing,2,percent.miss) ## 2 is for cols
sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing = sig.MetaDataVars.Indiv.Less.Than.Five.Percent.Missing[,missing.feature.percent.by.Feature<5] ## 1975 observations, 58 features
sig.MetaDataVars.Features.More.Than.Five.Percent.Missing = sig.MetaDataVars.Indiv.Less.Than.Five.Percent.Missing[,missing.feature.percent.by.Feature>5]
only.cont.colname = function(x){
  (!is.factor(x)==TRUE)
}
sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont1 = sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing[,1:3]
sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont2 = sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing[,10:18]
sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont3 = sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing[,c(20,23,26,27,29,30,31,32,33)]
sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont4 = sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing[,50:ncol(sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing)-1]
sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont = cbind(sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont1,
                                                                      sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont2,
                                                                      sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont3,
                                                                      sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont4)
'%ni%' <- Negate('%in%')
metadata.col.names = names(sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing)
sig.MetaDataVars.Only.Categ = sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing[,metadata.col.names %ni% names(sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont)]
for(i in 1:ncol(sig.MetaDataVars.Only.Categ)){
  sig.MetaDataVars.Only.Categ[i] = as.factor(unlist(sig.MetaDataVars.Only.Categ[i]))
}

library(missForest)
sig.Meta.Data.cat.miss = prodNA(sig.MetaDataVars.Only.Categ,noNA = 0.1)
summary(sig.Meta.Data.cat.miss)
sig.Meta.Data.cat.imputed = missForest(sig.Meta.Data.cat.miss)     
categorical.imputed.data = sig.Meta.Data.cat.imputed$ximp ## 1975 obs and 28 variables
sig.Meta.Data.cat.imputed$OOBerror ## 16% error
imput.results=mice(sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing.Cont,m=40,method = "pmm",seed = 500)##1975 observations of 58 variables
data.after.imput = complete(imput.results,action = 'long',include = TRUE)
data.after.imput.1 = complete(imput.results,1) ## 1975 observations 32 variables
combined.cat.cont.imputed.data = cbind(categorical.imputed.data,data.after.imput.1)
##na.check.sig.metadata.vars = apply(sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing,2,percent.miss)
##length(which((na.check.sig.metadata.vars) > 0))
write.csv(sig.MetaDataVars.Features.Less.Than.Five.Percent.Missing,file='checkForNA.csv',row.names = TRUE,na='NOT_FOUND')
DMM.class.data = read.csv(file="DMM4clusters2999Samples.csv",header = TRUE)
View(DMM.class.data)
DMM4.class.data = DMM.class.data[,c(1,4)]
View(DMM4.class.data)
View(combined.cat.cont.imputed.data)
combined.cat.cont.imputed.data$SampleId = rownames(combined.cat.cont.imputed.data)
data.after.imput.1.With.dmm.label = merge(combined.cat.cont.imputed.data,DMM4.class.data,by=c('SampleId'),all.x = TRUE)
View(data.after.imput.1.With.dmm.label)
data.after.imput.1.With.dmm.label$DMM_4 = as.factor(data.after.imput.1.With.dmm.label$DMM_4)
rownames(data.after.imput.1.With.dmm.label) = data.after.imput.1.With.dmm.label$SampleId
data.after.imput.1.With.dmm.label$SampleId = NULL
data.after.imput.1.With.dmm.label$DMM_4 = as.factor(data.after.imput.1.With.dmm.label$DMM_4)
## dropping data with one level
data.after.imput.1.With.dmm.label$J01CR02.1 = NULL
set.seed(1)
trainIndex = sample(1:nrow(data.after.imput.1.With.dmm.label),2*nrow(data.after.imput.1.With.dmm.label)/3)##1243 only
testIndex = -trainIndex 
trainData = data.after.imput.1.With.dmm.label[trainIndex,] ##1310
View(trainData)
testData = data.after.imput.1.With.dmm.label[testIndex,] ## 659
testData.DMM4.label = testData$DMM_4
##testDataToUse = testData[,-which(names(testData) %in% c("DMM_4"))]
testData$BetaGLobulines = testData$`BètaGlobulines_gL`
testData$`BètaGlobulines_gL` = NULL
##dat[ , -which(names(dat) %in% c("z","u"))]
##fit a simple tree
library(tree)
trainData$BetaGLobulines = trainData$`BètaGlobulines_gL`
trainData$`BètaGlobulines_gL` = NULL
trainData$FAMILYMEMBER_Inflammatory_Bowel_Disease_andere= NULL
testData$FAMILYMEMBER_Inflammatory_Bowel_Disease_andere= NULL
tree.model = tree(trainData$DMM_4~.,trainData)
summary(tree.model)
tree.model$frame
plot(tree.model)
text(tree.model) ## tree only showing classes 1,2 does not make sense
tree_predicted_train = predict(tree.model,trainData,type='class')
tree.pred.conf.mat.train = table(tree_predicted_train,trainData$DMM_4)
tree.pred.acc.train = sum(diag(tree.pred.conf.mat.train))/sum(tree.pred.conf.mat.train)*100
tree.pred.acc.train
tree.pred.test <- predict(tree.model,testData,type = "class")
tree.pred.conf.mat.test = table(tree.pred.test,testData.DMM4.label)
 ## is not making sense since only two classes predicted
tree.pred.acc.test = sum(diag(tree.pred.conf.mat.test)) / sum(tree.pred.conf.mat.test)*100 ## 39% accuracy
tree.pred.acc.test
library(randomForest)
trainData = trainData[,c(1:55,57,56)]
mtry.results = tuneRF(trainData[,1:56],trainData[,57],stepFactor = 1.5,improve = 1e-5,ntreeTry = 500)
print(mtry.results)
# Manual Search
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(trainData))))
modellist <- list()

metric <- "Accuracy"
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(1)
  fit <- train(DMM_4~., data=trainData, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)
rf.model = randomForest(trainData$DMM_4~.,data=trainData,importance=TRUE,ntree=1000)

varImpPlot(rf.model)
rf.predicted.train = predict(rf.model,trainData,type='class')
rf.pred.conf.mat.train = table(rf.predicted.train,trainData$DMM_4)
rf.pred.conf.mat.train
train.rf.accu = sum(diag(rf.pred.conf.mat.train))/sum(rf.pred.conf.mat.train)*100
train.rf.accu
rf.predicted.test= predict(rf.model,testData,type = 'class') 
##rf_predicted_df = as.data.frame(rf.predicted)
class.rf.conf.mat = table(rf.predicted.test,testData.DMM4.label)
class.rf.conf.mat
test.rf.accu=sum(diag(class.rf.conf.mat))/sum(class.rf.conf.mat)*100 ## 40.66 percent accuracy
test.rf.accu

##BOOSTING MODEL
library(gbm)
boosting.Model <-gbm(trainData$DMM_4~.,data=trainData,				
                     distribution="multinomial",     
                                    # number of trees
                     shrinkage=0.01,              # shrinkage or learning rate, 0.001 to 0.1 usually work
                     interaction.depth=3,n.trees = 1000)       # 1: additive model, 2: two-way interactions, etc.) 




boosting_predicted_train = predict(boosting.Model,trainData,type='response',n.trees = 1000)
boosting_predicted_train_df = as.data.frame(boosting_predicted_train)
boosting.pred=colnames(boosting_predicted_train_df)[apply(boosting_predicted_train_df,1,which.max)]
boosting_predicted_train_df$predicted.group = boosting.pred
boosting_predicted_train_df$predicted.group = as.factor(boosting_predicted_train_df$predicted.group)

library(plyr)
predicted.boosting.train.class=mapvalues(boosting_predicted_train_df
          $predicted.group,from=c('1.1000','2.1000','3.1000','4.1000'),to=c('1','2','3','4'))
boosting.pred.conf.mat.train = table(predicted.boosting.train.class,trainData$DMM_4)
boosting.pred.conf.mat.train
boosting.pred.acc.train = sum(diag(boosting.pred.conf.mat.train))/sum(boosting.pred.conf.mat.train)*100

boosting.pred.test <- predict(boosting.Model,testData,type = "response",n.trees = 1000)

boosting_predicted_test_df = as.data.frame(boosting.pred.test)
boosting.pred.test=colnames(boosting_predicted_test_df)[apply(boosting_predicted_test_df,1,which.max)]
boosting_predicted_test_df$predicted.group = boosting.pred.test
boosting_predicted_test_df$predicted.group = as.factor(boosting_predicted_test_df$predicted.group)
View(boosting_predicted_test_df)
predicted.boosting.test.class=mapvalues(boosting_predicted_test_df$predicted.group,from=c('1.1000','2.1000','3.1000','4.1000'),to=c('1','2','3','4'))
boosting.pred.conf.mat.test = table(predicted.boosting.test.class,testData.DMM4.label)
boosting.pred.conf.mat.test
boosting.pred.acc.test = sum(diag(boosting.pred.conf.mat.test))/sum(boosting.pred.conf.mat.test)*100
boosting.pred.acc.test

##BAGGING
matrix.of.test.accuracy.bagging=matrix(0,nrow=1000,ncol=1)
matrix.of.train.accuracy.bagging=matrix(0,nrow=1000,ncol=1)
colnames(testData) = colnames(trainData)
testData$Family_Member_IBD=testData$FAMILYMEMBER_Inflammatory_Bowel_Disease_andere
trainData$Family_Member_IBD=trainData$FAMILYMEMBER_Inflammatory_Bowel_Disease_andere


##BetaGLobulines

testData$BetaGLobulines = testData$Betaglobulin
testData$Betaglobulin = NULL
testData$BetaGLobulines = testData$`BètaGlobulines_gL`
testData$`BètaGlobulines_gL` = NULL
library(rpart)
for (i in 1:1000){
  ids = sample(1:nrow(trainData),2*nrow(trainData)/3)
  train.new = trainData[ids,]
  treemodel<-rpart(DMM_4~.,data=train.new,method = 'class')
  tree.pred.train = predict(treemodel,train.new,type = 'class')
  tree.pred.test = predict(treemodel,testData,type = 'class')
  conf.mat.tree.test = table(testData$DMM_4,tree.pred.test)
  conf.mat.tree.train = table(train.new$DMM_4,tree.pred.train)
  accu.test = sum(diag(conf.mat.tree.test))/sum(conf.mat.tree.test)
  accu.train = sum(diag(conf.mat.tree.train))/sum(conf.mat.tree.train)
  matrix.of.test.accuracy.bagging[i,1] = accu.test
  matrix.of.train.accuracy.bagging[i,1] = accu.train
  
}
mean.bagging.accuracy.test = mean(as.vector(matrix.of.test.accuracy.bagging[,1]))*100
mean.bagging.accuracy.train = mean(as.vector(matrix.of.train.accuracy.bagging[,1]))*100







acc.df.train = cbind(tree.pred.acc.train,train.rf.accu,boosting.pred.acc.train,mean.bagging.accuracy.train)
acc.df.test= cbind(tree.pred.acc.test,test.rf.accu,boosting.pred.acc.test,mean.bagging.accuracy.test)
convert.to.percent = function(x){
  round(x,digits=1)
}
acc.df.train.rounded = apply(acc.df.train,1,convert.to.percent) ## 2 is for cols, 1 is for rows
acc.df.test.rounded = apply(acc.df.test,1,convert.to.percent)
acc.df.train
acc.df.test
acc.df.train.rounded
acc.df.test.rounded


acc.df.train.rounded = as.data.frame(acc.df.train.rounded)
acc.df.test.rounded = as.data.frame(acc.df.test.rounded)

acc.df.train.rounded$Train_Accuracy = acc.df.train.rounded$V1
acc.df.train.rounded$Test_Accuracy = NULL
acc.df.train.rounded$Train_Accuracy = as.numeric(acc.df.train.rounded$Train_Accuracy)
Algo = c('Tree','Ramdom Forest','Boosting','Bagging')
acc.df.train.rounded$Algorithm = Algo
library(ggplot2)
train.accuracy.plot = ggplot(acc.df.train.rounded,aes(x=Algorithm,y=Train_Accuracy))+geom_bar(stat='identity',aes(fill=Algorithm),width = 0.3,show.legend = FALSE)+ylim(0,100)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

train.accuracy.plot

acc.df.test.rounded$Test_Accuracy = acc.df.test.rounded$V1
acc.df.test.rounded$Algorithm = Algo
test.accuracy.plot = ggplot(acc.df.test.rounded,aes(x=Algorithm,y=Test_Accuracy))+geom_bar(stat='identity',aes(fill=Algorithm),width = 0.3,show.legend = FALSE)+ylim(0,100)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
test.accuracy.plot

library(gridExtra)
grid.arrange(train.accuracy.plot,test.accuracy.plot,ncol=2)



