
##The data is proprietary and hence cannot be posted.
## This is the R code used for statistical analysis of data.
## It involves applying machine learning algorithm to model classification of individuals using metadata where the original class label 
##was based on microbiome data.
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

library(mice)
md.Pattern.Results=md.pattern(sig.Meta.Data.Vars.Only)
## frequency of sick leaves, no of work days and age at first work had 50%,50% and 27% missing. dropping those initially
sig.Meta.Data.Vars.Only.max.ten.percent.missing =sig.Meta.Data.Vars.Only[,-which(names(sig.Meta.Data.Vars.Only) %in% c("frequency_of_sick_leaves_last_year"," number_of_work_daysaweek","age_at_first_work"))]

percent.miss = function(x){
  sum(is.na(x))/length(x)*100
}
library(VIM)
aggr_plot <- aggr(sig.Meta.Data.Vars.Only.max.ten.percent.missing, col=c('navyblue','red','green'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"),cex.axs=colnames(sig.Meta.Data.Vars.Only.max.ten.percent.missing))
missing.feature.percent.by.var = apply(sig.Meta.Data.Vars.Only,2,percent.miss) ## 2 is for cols, 1 is for rows
missing.data.prop = sort(missing.feature.percent.by.var) ## OVER 5% MISSING IS VERY BAD
write.table(missing.data.prop,file='missing_data_prop.txt',quote = FALSE)
missing.feature.less.than.5.percent=which(missing.feature.percent.by.var<10)
sig.MetaDataVars.features.Less.Than.Five.Percent.Missing = sig.Meta.Data.Vars.Only[,missing.feature.less.than.5.percent]##1975 observations,58 features

missing.feature.percent.by.indiv = apply(sig.MetaDataVars.features.Less.Than.Five.Percent.Missing,1,percent.miss) ## 2 is for cols,1 is for rows
sort(missing.feature.percent.by.indiv) 
missing.indiv.less.than.5.percent=which(missing.feature.percent.by.indiv<10)
missing.indiv.greater.than.5.percent = which(missing.feature.percent.by.indiv>10)
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing = sig.MetaDataVars.features.Less.Than.Five.Percent.Missing[missing.indiv.less.than.5.percent,]
check.for.feature.miss = apply(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,2,percent.miss)
check.for.indiv.miss = apply(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,1,percent.miss)
length(which(check.for.indiv.miss>10))
length(which(check.for.feature.miss>10))
 ## now continue analysis with  sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$was_exposed_to_domestic_second_hand_smoking
## 'PARTICIPANTS_ALLERGY_food','PARTICIPANTS_ALLERGY_food.1','A07EC02','J01CA04','J01CR02','J01XE01'
## 'FAMILYMEMBER_Inflammatory_Bowel_Disease_andere','FAMILYMEMBER_Inflammatory_Bowel_Disease_Colitis_ulcerosa'
## 'FOODSUPPLEMENT_Mg','FOODSUPPLEMENT_Q10','A07EC02.1','G03AA12','G03DA04','J01CA04.1'
## 'J01CR02.1','J01XE01.1','L04AX01','N03AE01','N06AX16','R06AX28','A06AD15_65','G03CA04_CC06','L04AB'
## 'OTHER_GASTROENTRITIS_TREATMENT','do_you_want_to_do_something_about_your_weight'
## 'was_exposed_to_domestic_second_hand_smoking'
only.categ.vars = c('PARTICIPANTS_ALLERGY_food','A07EC02','J01CA04','J01CR02','J01XE01','FAMILYMEMBER_Inflammatory_Bowel_Disease_andere',
                    'FAMILYMEMBER_Inflammatory_Bowel_Disease_Colitis_ulcerosa','FOODSUPPLEMENT_Mg','FOODSUPPLEMENT_Q10',
                    'A07EC02.1','G03AA12','G03DA04','J01CA04.1','J01CR02.1','J01XE01.1','L04AX01','N03AE01','N06AX16',
                    'R06AX28','A06AD15_65','G03CA04_CC06','L04AB','OTHER_GASTROENTRITIS_TREATMENT',
                    'do_you_want_to_do_something_about_your_weight','was_exposed_to_domestic_second_hand_smoking')
for (i in only.categ.vars){
  sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing[,i] = as.factor(unlist(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing[,i]))
}
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$BetaGlobulines = sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$`BÃ¨taGlobulines_gL`
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$`BÃ¨taGlobulines_gL` = NULL
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$A07EC02.1= NULL
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$PARTICIPANTS_ALLERGY_food.1=NULL
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$J01CA04.1= NULL
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$J01CR02.1 = NULL
sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing$J01XE01.1 = NULL

DMM.class.data = read.csv(file="DMM4clusters2999Samples.csv",header = TRUE)
##View(DMM.class.data)
DMM4.class.data = DMM.class.data[,c(1,4)]

types.of.imput=c('pmm','midastouch','sample','cart','rf','2lonly.pmm')

set.seed(1)
trainIndex = sample(1:nrow(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing),2*nrow(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing)/3)##1243 only
testIndex = -trainIndex 


##DECISON TREE FUNCTION
model.fitting.tree=function(x,y){
  library(tree)
imput.results=mice(x,m=3,method = y,seed = 500)
data.after.imput=complete(imput.results)
data.after.imput$SampleId = as.factor(rownames(data.after.imput))
data.after.imput.dmm = merge(data.after.imput,DMM4.class.data,DMM4.class.data,by.x=c('SampleId'),by.y=c('SampleId'))
data.after.imput.dmm$DMM_4 = as.factor(data.after.imput.dmm$DMM_4)
  
trainData = data.after.imput.dmm[trainIndex,] ##1310

testData = data.after.imput.dmm[testIndex,] ## 659
testData.DMM4.label = testData$DMM_4
##DECISION TREE
tree.model = tree(trainData$DMM_4~.,trainData[,-1])
tree_predicted_train = predict(tree.model,trainData[,-1],type='class')
tree.pred.conf.mat.train = table(tree_predicted_train,trainData$DMM_4)
tree.pred.acc.train = sum(diag(tree.pred.conf.mat.train))/sum(tree.pred.conf.mat.train)*100
tree.pred.acc.train
tree.pred.test <- predict(tree.model,testData[,-1],type = 'class')
tree.pred.conf.mat.test = table(tree.pred.test,testData.DMM4.label)
  ## is not making sense since only two classes predicted
tree.pred.acc.test = sum(diag(tree.pred.conf.mat.test)) / sum(tree.pred.conf.mat.test)*100 ## 39% accuracy
tree.pred.acc.test
 acc.tree = c(tree.pred.acc.train,tree.pred.acc.test)
 acc.tree
}
## for same test and training set
tree.pmm = model.fitting.tree(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'pmm')
tree.cart = model.fitting.tree(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'cart')
tree.sample = model.fitting.tree(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'sample')
tree.rf = model.fitting.tree(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'rf')


##Random forest function
model.fitting.rf=function(x,y){
  library(randomForest)
  imput.results=mice(x,m=3,method = y,seed = 500)
  ##imput.results=mice(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,m=3,method = 'pmm',seed = 500)
  data.after.imput=complete(imput.results)
  data.after.imput$SampleId = as.factor(rownames(data.after.imput))
  data.after.imput.dmm = merge(data.after.imput,DMM4.class.data,DMM4.class.data,by.x=c('SampleId'),by.y=c('SampleId'))
  data.after.imput.dmm$DMM_4 = as.factor(data.after.imput.dmm$DMM_4)
  trainData = data.after.imput.dmm[trainIndex,] ##1310
  
  testData = data.after.imput.dmm[testIndex,] ## 659
  testData.DMM4.label = testData$DMM_4
 
  rf.model = randomForest(trainData$DMM_4~.,data=trainData[,-1],importance=TRUE,ntree=1000)
  
  varImpPlot(rf.model)
  rf.predicted.train = predict(rf.model,trainData,type='class')
  rf.pred.conf.mat.train = table(rf.predicted.train,trainData$DMM_4)
  rf.pred.conf.mat.train
  train.rf.accu = sum(diag(rf.pred.conf.mat.train))/sum(rf.pred.conf.mat.train)*100
  train.rf.accu
  rf.predicted.test= predict(rf.model,testData[,-1],type = 'class') 
  
  class.rf.conf.mat = table(rf.predicted.test,testData.DMM4.label)
  class.rf.conf.mat
  test.rf.accu=sum(diag(class.rf.conf.mat))/sum(class.rf.conf.mat)*100 ## 40.66 percent accuracy
  test.rf.accu
  acc.rf = c(train.rf.accu,test.rf.accu)
  acc.rf
}
rf.pmm = model.fitting.rf(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'pmm')
rf.cart = model.fitting.rf(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'cart')
rf.sample = model.fitting.rf(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'sample')
rf.rf = model.fitting.rf(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'rf')


##Boosting function
model.fitting.boosting = function(x,y){
  imput.results=mice(x,m=3,method = y,seed = 500)
  ##imput.results=mice(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,m=3,method = 'pmm',seed = 500)
  data.after.imput=complete(imput.results)
  data.after.imput$SampleId = as.factor(rownames(data.after.imput))
  data.after.imput.dmm = merge(data.after.imput,DMM4.class.data,DMM4.class.data,by.x=c('SampleId'),by.y=c('SampleId'))
  data.after.imput.dmm$DMM_4 = as.factor(data.after.imput.dmm$DMM_4)
  trainData = data.after.imput.dmm[trainIndex,] ##1310
  
  testData = data.after.imput.dmm[testIndex,] ## 659
  testData.DMM4.label = testData$DMM_4
  
  library(gbm)
  boosting.Model <-gbm(trainData$DMM_4~.,data=trainData[,-1],				
                       distribution="multinomial",     
                       # number of trees
                       shrinkage=0.01,              # shrinkage or learning rate, 0.001 to 0.1 usually work
                       interaction.depth=1,n.trees = 1000,bag.fraction = 0.5,train.fraction =1,n.cores = NULL )       # 1: additive model, 2: two-way interactions, etc.) 
  boosting_predicted_train = predict(boosting.Model,trainData[,-1],type='response',n.trees = 1000)
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
  
  boosting.pred.test <- predict(boosting.Model,testData[,-1],type = "response",n.trees = 1000)
  
  boosting_predicted_test_df = as.data.frame(boosting.pred.test)
  boosting.pred.test=colnames(boosting_predicted_test_df)[apply(boosting_predicted_test_df,1,which.max)]
  boosting_predicted_test_df$predicted.group = boosting.pred.test
  boosting_predicted_test_df$predicted.group = as.factor(boosting_predicted_test_df$predicted.group)
  
  predicted.boosting.test.class=mapvalues(boosting_predicted_test_df$predicted.group,from=c('1.1000','2.1000','3.1000','4.1000'),to=c('1','2','3','4'))
  boosting.pred.conf.mat.test = table(predicted.boosting.test.class,testData.DMM4.label)
  boosting.pred.conf.mat.test
  boosting.pred.acc.test = sum(diag(boosting.pred.conf.mat.test))/sum(boosting.pred.conf.mat.test)*100
  boosting.pred.acc.test
  acc.boosting = c(boosting.pred.acc.train,boosting.pred.acc.test)
  acc.boosting
}
boosting.pmm = model.fitting.boosting(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'pmm')
boosting.cart = model.fitting.rf(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'cart')
boosting.sample = model.fitting.rf(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'sample')
boosting.rf = model.fitting.rf(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'rf')



##Bagging function
model.fitting.bagging = function(x,y,no.of.trees){
  imput.results=mice(x,m=3,method = y,seed = 500)
  data.after.imput=complete(imput.results)
  data.after.imput$SampleId = as.factor(rownames(data.after.imput))
  data.after.imput.dmm = merge(data.after.imput,DMM4.class.data,DMM4.class.data,by.x=c('SampleId'),by.y=c('SampleId'))
  data.after.imput.dmm$DMM_4 = as.factor(data.after.imput.dmm$DMM_4)
  trainData = data.after.imput.dmm[trainIndex,] ##1310
  
  testData = data.after.imput.dmm[testIndex,] ## 659
  testData.DMM4.label = testData$DMM_4
  matrix.of.test.accuracy.bagging=matrix(0,nrow=no.of.trees,ncol=1)
  matrix.of.train.accuracy.bagging=matrix(0,nrow=no.of.trees,ncol=1)
library(tree)
for (i in 1:no.of.trees){
  ids = sample(1:nrow(trainData),2*nrow(trainData)/3)
  train.new = trainData[ids,]
  treemodel<-tree(DMM_4~.,data=train.new[,-1],method = 'class')
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
  accu.bagging = c(mean.bagging.accuracy.train,mean.bagging.accuracy.test)
}  
## first training, then test
bagging.pmm.100.trees = model.fitting.bagging(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'pmm',100)
bagging.pmm.1000.trees = model.fitting.bagging(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'pmm',1000)
bagging.cart.1000.trees = model.fitting.bagging(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'cart',1000)
bagging.sample.1000.trees = model.fitting.bagging(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'sample',1000)
bagging.rf.1000.trees = model.fitting.bagging(sig.Meta.Data.Vars.less.than.5.percent.indiv.and.feature.missing,'rf',1000)

cart.imput.results.training=round(c(bagging.cart.1000.trees[1],boosting.cart[1],rf.cart[1],tree.cart[1]),1)
cart.imput.results.testing=round(c(bagging.cart.1000.trees[2],boosting.cart[2],rf.cart[2],tree.cart[2]),1)
cart.imput.df = cbind(cart.imput.results.training,cart.imput.results.testing)
rownames(cart.imput.df) =c('Bagging','Boosting','Random Forest','Single Decision Tree')

pmm.imput.results.training=round(c(bagging.pmm.1000.trees[1],boosting.pmm[1],rf.pmm[1],tree.pmm[1]),1)
pmm.imput.results.testing=round(c(bagging.pmm.1000.trees[2],boosting.pmm[2],rf.pmm[2],tree.pmm[2]),1)
pmm.imput.df = cbind(pmm.imput.results.training,pmm.imput.results.testing)
rownames(pmm.imput.df) =c('Bagging','Boosting','Random Forest','Single Decision Tree')


rf.imput.results.training=round(c(bagging.rf.1000.trees[1],boosting.rf[1],rf.rf[1],tree.rf[1]),1)
rf.imput.results.testing=round(c(bagging.rf.1000.trees[2],boosting.rf[2],rf.rf[2],tree.rf[2]),1)
rf.imput.df = cbind(rf.imput.results.training,rf.imput.results.testing)
rownames(rf.imput.df) =c('Bagging','Boosting','Random Forest','Single Decision Tree')


sample.imput.results.training=round(c(bagging.sample.1000.trees[1],boosting.sample[1],rf.sample[1],tree.sample[1]),1)
sample.imput.results.testing=round(c(bagging.sample.1000.trees[2],boosting.sample[2],rf.sample[2],tree.sample[2]),1)
sample.imput.df = cbind(sample.imput.results.training,sample.imput.results.testing)
rownames(sample.imput.df) =c('Bagging','Boosting','Random Forest','Single Decision Tree')

all.pred.accu.df = cbind(cart.imput.df,pmm.imput.df,rf.imput.df,sample.imput.df)

## plot type 1 of per imputation method, how did each algorithm do
## plot type 2 of within an algorith, how did each imputation method do
all.pred.accu.train = all.pred.accu.df[,c(1,3,5,7)]

library(ggplot2)
all.pred.accu.train = as.data.frame(all.pred.accu.train)
algo_type=c('CART','PMM','RF','Sample')



##BAGGING DF TRAIN
bagging.df.train = rbind(all.pred.accu.train[1,],algo_type)
bagging.df.train = t(bagging.df.train)
bagging.df.train = as.data.frame(bagging.df.train)

dev.off()
bagging.df.train$Bagging_Accuracy_Train = as.numeric(as.character(bagging.df.train$Bagging))
setnames(bagging.df.train,old=c('2'),new=c('Imputation_Type'))

bagging.train=ggplot(bagging.df.train,aes(x=Imputation_Type,y=Bagging_Accuracy_Train))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)

##BOOSTING DF TRAIN
boosting.df.train = rbind(all.pred.accu.train[2,],algo_type)
boosting.df.train = t(boosting.df.train)
boosting.df.train = as.data.frame(boosting.df.train)
boosting.df.train$Boosting_Accuracy_Train = as.numeric(as.character(boosting.df.train$Boosting))
setnames(boosting.df.train,old=c('2'),new=c('Imputation_Type'))
boosting.train=ggplot(boosting.df.train,aes(x=Imputation_Type,y=Boosting_Accuracy_Train))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)



##RANDOM FOREST DF TRAIN
random.forest.df.train = rbind(all.pred.accu.train[3,],algo_type)
random.forest.df.train = t(random.forest.df.train)
random.forest.df.train = as.data.frame(random.forest.df.train)
random.forest.df.train$RF_Accuracy_Train = as.numeric(as.character(random.forest.df.train$`Random Forest`))
setnames(random.forest.df.train,old=c('2'),new=c('Imputation_Type'))
random.forest.train=ggplot(random.forest.df.train,aes(x=Imputation_Type,y=RF_Accuracy_Train))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)

## CLASSIFICATION TREE TRAIN
single.tree.df.train = rbind(all.pred.accu.train[4,],algo_type)
single.tree.df.train = t(single.tree.df.train)
single.tree.df.train = as.data.frame(single.tree.df.train)
single.tree.df.train$Tree_Accuracy_Train = as.numeric(as.character(single.tree.df.train$`Single Decision Tree`))
setnames(single.tree.df.train,old=c('2'),new=c('Imputation_Type'))
single.tree.train=ggplot(single.tree.df.train,aes(x=Imputation_Type,y=Tree_Accuracy_Train))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)


library(gridExtra)
grid.arrange(bagging.train,boosting.train,random.forest.train,single.tree.train,nrow=2,ncol=2)

all.pred.accu.test = all.pred.accu.df[,c(2,4,6,8)]
all.pred.accu.test = as.data.frame(all.pred.accu.test)
##BAGGING DF TEST
bagging.df.test = rbind(all.pred.accu.train[1,],algo_type)
bagging.df.test = t(bagging.df.test)
bagging.df.test = as.data.frame(bagging.df.test)

dev.off()
bagging.df.test$Bagging_Accuracy_Test = as.numeric(as.character(bagging.df.test$Bagging))
setnames(bagging.df.test,old=c('2'),new=c('Imputation_Type'))

bagging.test=ggplot(bagging.df.test,aes(x=Imputation_Type,y=Bagging_Accuracy_Test))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)

##BOOSTING DF TEST
boosting.df.test = rbind(all.pred.accu.test[2,],algo_type)
boosting.df.test = t(boosting.df.test)
boosting.df.test = as.data.frame(boosting.df.test)
boosting.df.test$Boosting_Accuracy_Test = as.numeric(as.character(boosting.df.test$Boosting))
setnames(boosting.df.test,old=c('2'),new=c('Imputation_Type'))
boosting.test=ggplot(boosting.df.test,aes(x=Imputation_Type,y=Boosting_Accuracy_Test))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)


##RANDOM FOREST DF TEST
random.forest.df.test = rbind(all.pred.accu.test[3,],algo_type)
random.forest.df.test = t(random.forest.df.test)
random.forest.df.test = as.data.frame(random.forest.df.test)
random.forest.df.test$RF_Accuracy_Test = as.numeric(as.character(random.forest.df.test$`Random Forest`))
setnames(random.forest.df.test,old=c('2'),new=c('Imputation_Type'))
random.forest.test=ggplot(random.forest.df.test,aes(x=Imputation_Type,y=RF_Accuracy_Test))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)


##CLASSIFICATION TREE DF TEST
single.tree.df.test = rbind(all.pred.accu.test[4,],algo_type)
single.tree.df.test = t(single.tree.df.test)
single.tree.df.test = as.data.frame(single.tree.df.test)
single.tree.df.test$Decision_Tree_Accuracy_Test = as.numeric(as.character(single.tree.df.test$`Single Decision Tree`))
setnames(single.tree.df.test,old=c('2'),new=c('Imputation_Type'))
single.tree.df.test=ggplot(single.tree.df.test,aes(x=Imputation_Type,y=Decision_Tree_Accuracy_Test))+geom_bar(stat='identity',aes(fill=Imputation_Type),width = 0.3,show.legend = FALSE)+ylim(0,100)


grid.arrange(bagging.test,boosting.test,random.forest.test,single.tree.df.test,nrow=2,ncol=2)
