setwd("F:\\Data science\\R\\data files")
library(dplyr)
library(tidyr)
library(car)
b_train=read.csv("bank-full_train.csv",stringsAsFactors = F)
b_test=read.csv("bank-full_test.csv",stringsAsFactors = F)
b_test$y=NA
b_train$data="train"
b_test$data="test"
b_all=rbind(b_train,b_test)

names(b_all)
sort(table(b_all$job))
sort(table(b_all$marital))
sort(table(b_all$education))
sort(table(b_all$default))
sort(table(b_all$housing))
sort(table(b_all$contact))
sort(table(b_all$poutcome))


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
lapply(b_all,function(x) sum(is.na(x)))


cat_cols=c("marital","education","default","housing",
           "loan","contact","month","poutcome","y","job")


for(col in cat_cols){
  b_all=CreateDummies(b_all,col,100)
}

b_all$y_no=as.factor(b_all$y_no)

library(randomForest)
library(tree)

b_train=b_all %>% filter(data=='train') %>% select(-data)
b_test=b_all %>% filter(data=='test') %>% select(-data,-y_no)

set.seed(2)
s=sample(1:nrow(b_train),0.7*nrow(b_train))
b_train1=b_train[s,]
b_train2=b_train[-s,]


rf.model_b=randomForest(y_no~.-ID,data=b_train1,do.trace=T)

test.score=predict(rf.model_b,newdata = b_train2,type='prob')[,2]
pROC::roc(b_train2$y_no,test.score)





real=b_train2$y_no

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(test.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}


cutoff_data=cutoff_data[-1,]

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff



test.predicted=as.numeric(test.score>my_cutoff)


test.score_b=predict(rf.model_b,newdata = b_test,type='prob')[,2]

test.predicted_b=as.numeric(test.score_b>my_cutoff)
write.csv(test.predicted_b,"Saiprasad_P5_SUB2.csv",row.names = F)



