# setting the directory
setwd("F:\\Data science\\R\\data files")

# importing the libraries
library(dplyr)
library(tidyr)
library(car)

#Loading the data set
hr1_train=read.csv("hr_train.csv",stringsAsFactors = F)
hr1_test=read.csv("hr_test.csv",stringsAsFactors = F)
hr1_test$left=NA
hr1_train$data="train"
hr1_test$data="test"
hr1_all=rbind(hr1_train,hr1_test)
names(hr1_all)
sort(table(hr1_all$salary))
sort(table(hr1_all$sales))
# Dummy creation
CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

lapply(hr1_all,function(x) sum(is.na(x)))

hr1_all$left=as.factor(hr1_all$left)

cat_cols=c("salary")
for(col in cat_cols){
  hr1_all=CreateDummies(hr1_all,col,1000)
}

cat_cols=c("sales")
for(col in cat_cols){
  hr1_all=CreateDummies(hr1_all,col,600)
}

# MODEL BUILDING
library(randomForest)
library(tree)

hr1_train=hr1_all %>% filter(data=='train') %>% select(-data)
hr1_test=hr1_all %>% filter(data=='test') %>% select(-data,-left)

set.seed(3)
s=sample(1:nrow(hr1_train),0.7*nrow(hr1_train))
hr1_train1=hr1_train[s,]
hr1_train2=hr1_train[-s,]

names(hr1_train)
tree.model_hr=tree(left~.,data=hr1_train1)
summary(tree.model_hr)
plot(tree.model_hr)
text(tree.model_hr)

tree.model_hr

test.score_hr=predict(tree.model_hr,newdata = hr1_train2,type='vector')[,2]

pROC::roc(hr1_train2$left,test.score_hr) ##### 0.82

#model 2

rf.model_hr=randomForest(left~.,data=hr1_train1,do.trace=T)

test.score=predict(rf.model_hr,newdata = hr1_train2,type='prob')[,2]
pROC::roc(hr1_train2$left,test.score) #### 0.83

# model 3
library(extraTrees)
library(rJava)

names(hr1_train1)
x_train=hr1_train1 %>% select(-left)
glimpse(x_train)
y_train=hr1_train1$left
View(y_train)%>% select(-left)
x_test=hr1_train2 


et.model_hr=extraTrees(x_train,y_train)

test.score=predict(et.model_hr,newdata = x_test,probability = T)[,2]
test.score
pROC::roc(x_test$left,test.score) 
#####0.83


library(cvTools)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}


param=list(mtry=c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),
           ntree=c(50,100,150,200,250,300,350,400,450,500,550,600,650),
           maxnodes=c(5,10,15,20,25),
           nodesize=c(1,2,5,10,15,20))


subset_paras=function(full_list_para,n=100){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}


my_params=subset_paras(param,100)
my_params

myauc=0


for(i in 1:100){
  print('starting iteration')
  
  params=my_params[i,]
  
  k=cvTuning(randomForest,left~.,
             data =hr1_train1,
             tuning =params,
             folds = cvFolds(nrow(hr1_train), K=10, type = "random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob"))
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    myauc=score.this
    print(myauc)
  }
  
  print('DONE')
}
rf.tuned.model_hr=randomForest(left~.,data=hr1_train1,
                            ntree=600,mtry=10,maxnodes=25,nodesize=10,do.trace=T)

test.score_hr=predict(rf.tuned.model_hr,newdata = hr1_train2,type='prob')[,2]
pROC::roc(hr1_train2$left,test.score_hr)

score=predict(rf.tuned.model_hr,newdata= hr1_test, type="prob")[,2]
write.csv(score,"saiprasad_Rf_tuningmodel_sub2",row.names = F)







