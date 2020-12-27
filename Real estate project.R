#Setting the directory
setwd("F:\\Data science\\R\\data files")

#Importing the libraries
library(dplyr)
library(tidyr)
library(car)
library(randomForest)
library(extraTrees)

#Loading the data
d1_train=read.csv("housing_train.csv",stringsAsFactors = F)
d1_test=read.csv("housing_test.csv",stringsAsFactors = F)
d1_test$Price=NA
d1_train$data="train"
d1_test$data="test"
d1_all=rbind(d1_train,d1_test)

sort(table(d1_all$Rooms))
sort(table(d1_all$Type))
sort(table(d1_all$Method))
sort(table(d1_all$Suburb))
sort(table(d1_all$CouncilArea))
sort(table(d1_all$SellerG))
sort(table(d1_all$YearBuilt))

d1_all$store=as.factor(d1_all$Price)

# creating dummies
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

cat_cols=c("Type","Method")
for(col in cat_cols){
  d1_all=CreateDummies(d1_all,col,500)
}


cat_cols=c("CouncilArea")
for(col in cat_cols){
  d1_all=CreateDummies(d1_all,col,100)
}

cat_cols=c("Suburb")
for(col in cat_cols){
  d1_all=CreateDummies(d1_all,col,10)
}

cat_cols=c("SellerG")
for(col in cat_cols){
  d1_all=CreateDummies(d1_all,col,10)
}

glimpse(d1_all)

# filling the missing values
lapply(d1_all,function(x) sum(is.na(x)))

for(col in names(d1_all)){
  
  if(sum(is.na(d1_all[,col]))>0 & !(col %in% c("Price","Bedroom2","Bathroom"
                                               ,"Car","YearBuilt"))){
    
    d1_all[is.na(d1_all[,col]),col]=mean(d1_all[,col],na.rm=T)
  }
}

glimpse(d1_all)

lapply(d1_all,function(x) sum(is.na(x)))
a=table(d1_all$Bedroom2)
b=table(d1_all$Bathroom)
r=table(d1_all$Car)
View(a)
e=table(d1_all$YearBuilt)
d1_all$Bedroom2[is.na(d1_all$Bedroom2)]=as.numeric(names(a[which.max(a)])[1])
d1_all$Bathroom[is.na(d1_all$Bathroom)]=as.numeric(names(b[which.max(b)])[1])
d1_all$Car[is.na(d1_all$Car)]=as.numeric(names(r[which.max(r)])[1])
d1_all$YearBuilt[is.na(d1_all$YearBuilt)]=as.numeric(names(e[which.max(e)])[1])


lapply(d1_all,function(x) sum(is.na(x)))


d1_train=d1_all %>% filter(data=='train') %>% select(-data)
d1_test=d1_all %>% filter(data=='test') %>% select(-data,-Price)

set.seed(2)
s=sample(1:nrow(d1_train),0.7*nrow(d1_train))
d1_train1=d1_train[s,]
d1_train2=d1_train[-s,]
       
# model building
rf.model_house=randomForest(Price~.-Address-Postcode-Distance-CouncilArea_-SellerG_Abercromby,data=d1_train1,do.trace=T)

test.score=predict(rf.model_house,newdata = d1_train2,type='prob')[,1]
pROC::roc(d1_train2$Price,test.score)



score=predict(rf.model_house,newdata= d1_test, type="prob")[,1]
write.csv(score,"saiprasad_housingprice_Randomforest_sub3",row.names = F)

#model 2

vi1=lm(Price~.-Address-Postcode-Distance-CouncilArea_,data = d1_train1)
sort(vif(vi1),decreasing = T)[1:10]
summary(vi1)
vi1=step(vi1)
summary(vi1)
formula(vi1)
fit=lm(Price ~ Rooms + Bedroom2 + Bathroom + Car + Landsize + BuildingArea + 
         YearBuilt + Type_u + Type_h + Method_SP + Method_S + CouncilArea_Whitehorse + 
         CouncilArea_Manningham + CouncilArea_Brimbank + CouncilArea_HobsonsBay + 
         CouncilArea_Bayside + CouncilArea_Melbourne + CouncilArea_Banyule + 
         CouncilArea_PortPhillip + CouncilArea_Maribyrnong + CouncilArea_Stonnington + 
         CouncilArea_GlenEira + CouncilArea_MooneeValley + CouncilArea_Moreland + 
         CouncilArea_Boroondara + Suburb_Kingsbury + Suburb_KeilorPark + 
         Suburb_Jacana + Suburb_Kealba + Suburb_Balaclava + Suburb_Eaglemont + 
         Suburb_EastMelbourne + Suburb_IvanhoeEast + Suburb_Cremorne + 
         Suburb_MiddlePark + Suburb_WestMelbourne + Suburb_Gowanbrae + 
         Suburb_GlenHuntly + Suburb_Yallambie + Suburb_Albion + Suburb_CaulfieldNorth + 
         Suburb_Parkville + Suburb_Aberfeldie + Suburb_Alphington + 
         Suburb_Hughesdale + Suburb_CarltonNorth + Suburb_Southbank + 
         Suburb_Viewbank + Suburb_MontAlbert + Suburb_Watsonia + Suburb_Fairfield + 
         Suburb_HamptonEast + Suburb_Heidelberg + Suburb_CaulfieldSouth + 
         Suburb_Fitzroy + Suburb_Braybrook + Suburb_Carlton + Suburb_Canterbury + 
         Suburb_CliftonHill + Suburb_Flemington + Suburb_KewEast + 
         Suburb_AlbertPark + Suburb_BoxHill + Suburb_Windsor + Suburb_Elsternwick + 
         Suburb_Collingwood + Suburb_OakPark + Suburb_Hadfield + Suburb_Abbotsford + 
         Suburb_HeidelbergWest + Suburb_NorthMelbourne + Suburb_OakleighSouth + 
         Suburb_CoburgNorth + Suburb_Murrumbeena + Suburb_HeidelbergHeights + 
         Suburb_Malvern + Suburb_Moorabbin + Suburb_SouthMelbourne + 
         Suburb_Ashburton + Suburb_BrunswickEast + Suburb_Maidstone + 
         Suburb_AirportWest + Suburb_FitzroyNorth + Suburb_Ormond + 
         Suburb_SunshineNorth + Suburb_WestFootscray + Suburb_AvondaleHeights + 
         Suburb_Fawkner + Suburb_AltonaNorth + Suburb_Armadale + Suburb_Burwood + 
         Suburb_Williamstown + Suburb_Melbourne + Suburb_SunshineWest + 
         Suburb_Ivanhoe + Suburb_TemplestoweLower + Suburb_BrunswickWest + 
         Suburb_KeilorEast + Suburb_HawthornEast + Suburb_Prahran + 
         Suburb_SurreyHills + Suburb_Kensington + Suburb_Sunshine + 
         Suburb_Toorak + Suburb_Elwood + Suburb_Maribyrnong + Suburb_Doncaster + 
         Suburb_AscotVale + Suburb_MooneePonds + Suburb_Thornbury + 
         Suburb_Hampton + Suburb_Balwyn + Suburb_MalvernEast + Suburb_Camberwell + 
         Suburb_Carnegie + Suburb_PortMelbourne + Suburb_Bentleigh + 
         Suburb_PascoeVale + Suburb_BrightonEast + Suburb_Hawthorn + 
         Suburb_BalwynNorth + Suburb_Northcote + Suburb_Kew + Suburb_Brighton + 
         Suburb_Glenroy + Suburb_GlenIris + Suburb_Essendon + Suburb_Brunswick + 
         Suburb_SouthYarra + Suburb_StKilda + Suburb_Preston + Suburb_Richmond + 
         Suburb_BentleighEast + Suburb_Reservoir + SellerG_Castran + 
         SellerG_Melbourne + SellerG_Tim + SellerG_Haughton + SellerG_Lindellas + 
         SellerG_Paul + `SellerG_Abercromby's` + SellerG_Philip + 
         SellerG_McDonald + SellerG_Thomson + SellerG_LITTLE + SellerG_Nick + 
         SellerG_Cayzer + SellerG_Collins + SellerG_Raine + SellerG_Kay + 
         SellerG_Noel + SellerG_Greg + SellerG_RT + SellerG_Marshall + 
         SellerG_hockingstuart + SellerG_Jellis,data=d1_train1)
summary(fit)

plot(fit,1)
plot(fit,2)
plot(fit,3)
plot(fit,4)

# predictions
val.pred=predict(fit,newdata = d1_train2)
d1_test$predicted_value=predict(fit,newdata = d1_test)
write.csv(d1_test$predicted_value,"Saiprasad_linearmodel_submission3",row.names = F)
errors=d1_train2$Price-val.pred

rmse=errors**2 %>% mean() %>% sqrt()












