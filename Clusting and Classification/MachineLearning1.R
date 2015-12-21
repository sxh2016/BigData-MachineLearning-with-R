library(igraph)
library(stringr)
library(hash)
library(fpc)
library(cluster)
library(kknn)

mr<-read.table(file.path("~/Desktop/bigdatapro2/agaricus-lepiota.data"),sep = ",",header = FALSE)
names(mr)<-c("class", "cshape", "csurface", "ccolor", "bruises", "odor", 
             "gattach", "gspace", "gsize", "gcolor", "sshape", "sroot", 
             "ssabove", "ssbelow", "scabove", "scbelow", "vtype", "vcolor", 
             "rnumber", "rtype", "spcolor", "popnum", "habitat")
pairs(class ~ cshape + csurface + ccolor+bruises, data=mr)
plot(class ~ cshape + csurface + ccolor+bruises, data=mr)

pairs(class ~ odor + gattach + gspace+gsize, data=mr)
plot(class ~ odor + gattach + gspace+gsize, data=mr)

pairs(class ~ gcolor + sshape + sroot+ssabove, data=mr)
plot(class ~ gcolor + sshape + sroot+ssabove, data=mr)

pairs(class ~ ssbelow + scabove + scbelow + vtype, data=mr)
plot(class ~ ssbelow + scabove + scbelow + vtype, data=mr)

pairs(class ~ vcolor + rnumber + rtype + spcolor, data=mr)
plot(class ~ vcolor + rnumber + rtype + spcolor, data=mr)

pairs(class ~ popnum + habitat, data=mr)
plot(class ~ popnum + habitat, data=mr)

mrf<-as.data.frame(mr, stringAsFactors=FALSE)
mrf[,]<-sapply(mrf[,],as.character)

mrf1<-as.data.frame(mr, stringAsFactors=FALSE)
mrf1[,]<-sapply(mrf1[,],as.character)

mrf_num<-data.frame()
for(i in 1:length(mrf)){
  ind = vector()
  dimsum = dim(table(mrf[,i]))
  minnum=strtoi(charToRaw('a'),16L)
  for(k in 1:dimsum){
    cha=names(table(mrf[,i]))[k]
    ind[k] = strtoi(charToRaw(cha),16L)-minnum
  }
  
  elements=names(table(mrf[,i]))
  for(j in 1:length(mrf[,i])){
    for(m in 1:length(elements)){
      if(mrf[j,i] == elements[m]){
        mrf[j,i] = ind[m]
        mrf_num[j,i]=as.numeric(mrf[j,i])
        break
      }
    }
  }
}


names(mrf_num)<-c("class", "cshape", "csurface", "ccolor", "bruises", "odor", 
                  "gattach", "gspace", "gsize", "gcolor", "sshape", "sroot", 
                  "ssabove", "ssbelow", "scabove", "scbelow", "vtype", "vcolor", 
                  "rnumber", "rtype", "spcolor", "popnum", "habitat")

names(mrf_num)

set.seed(1234)
train70.df <- sample(nrow(mrf_num), 0.7*nrow(mr))
train60.df <- sample(nrow(mrf_num), 0.6*nrow(mr))
train50.df<-sample(nrow(mrf_num), 0.5*nrow(mr))

mrtrain70.df <- mrf_num[train70.df,]
mrtest30.df <- mrf_num[-train70.df,]

mrtrain60.df <- mrf_num[train60.df,]
mrtest40.df  <- mrf_num[-train60.df,]

mrtrain50.df <- mrf_num[train50.df,]
mrtest50.df <- mrf_num[-train50.df,]

mrf$class[mrf$class=='e'] <- 0




km=pamk(mrf_num, krange=2)
plot(mrf_num[c("class","odor","spcolor","rtype")],col=km$pamobject$clustering)
kn=kmeans(mrf_num[c("odor","spcolor","rtype")],2)
plot(mrf_num[c("class","odor","spcolor","rtype")],col=kn$pamobject$clustering)
 

kma=pamk(mrf_num, krange=3)
plot(mrf_num[c("class","odor","spcolor","rtype")],col=kma$pamobject$clustering)
kna=kmeans(mrf_num[c("odor","spcolor","rtype")],3)
plot(mrf_num[c("class","odor","spcolor","rtype")],col=kna$pamobject$clustering)


kmb=pamk(mrf_num, krange=5)
plot(mrf_num[c("odor","spcolor","rtype")],col=kmb$pamobject$clustering)
knb=kmeans(mrf_num[c("odor","spcolor","rtype")],5)
plot(mrf_num[c("class","odor","spcolor","rtype")],col=knb$pamobject$clustering)


kmc=pamk(mrf_num, krange=7)
plot(mrf_num[c("odor","spcolor","rtype")],col=kmc$pamobject$clustering)
knc=kmeans(mrf_num[c("odor","spcolor","rtype")],7)
plot(mrf_num[c("class","odor","spcolor","rtype")],col=knc$pamobject$clustering)



plot(mrtest30.df[c("class","odor")],col=kn$cluster)
plot(mrtest30.df[c("class","spcolor")],col=kn$cluster)
plot(mrtest30.df[c("class","rtype")],col=kn$cluster)

model1<-train.kknn(formula = class ~ odor + spcolor + rtype, data = mrtrain50.df)
predicton = predict(model1,mrtest50.df)
pre1<-predicton
pre1

CM1<-table(mrtest50.df[ , 1], pre1)
CM1

accuracy <- (CM1[1,1] + CM1[nrow(CM1),ncol(CM1)])/sum(CM1)
accuracy

model2<-train.kknn(formula = class ~ odor + spcolor + rtype, data = mrtrain60.df)
predicton = predict(model2,mrtest40.df)
pre2<-predicton
pre2

CM2<-table(mrtest40.df[ , 1], pre2)
CM2

accuracy <- (CM2[1,1] + CM2[nrow(CM2),ncol(CM2)])/sum(CM2)
accuracy

model3<-train.kknn(formula = class ~ odor + spcolor + rtype, data = mrtrain70.df)
predicton = predict(model3,mrtest30.df)
pre3<-predicton
pre3

CM3<-table(mrtest30.df[ , 1], pre3)
CM3

accuracy <- (CM3[1,1] + CM3[nrow(CM3),ncol(CM3)])/sum(CM3)
accuracy

