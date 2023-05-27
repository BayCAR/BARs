library("zoo")
library('readxl')
library('splines2')
library('survival')
library('splines')

library(readxl)
library(irr)
library(psych)
library(BlandAltmanLeh)

files=list.files("C:/Users/shpry/Desktop/WFH/Mcdougal/data/", all.files=FALSE)
ids=substr(files, 1, 24)
uid=unique(ids)

dd1=vector("list", 10)
dd2=vector("list", 10)
i=2

data.path="C:/Users/shpry/Desktop/WFH/Mcdougal/data/"
out.path="C:/Users/shpry/Desktop/WFH/Mcdougal/out/"
plot.path="C:/Users/shpry/Desktop/WFH/Mcdougal/plots/"

for (i in 1:10)
{uids=uid[i]

outi=MET.plot.fun (data.path, uids, out.path, plot.path, pairs="yes")


out.i=rbind(data.frame(ID=paste0(substr(uids,12, 50)), visit="V1", outi[[1]][[2]]),
            data.frame(ID=paste0(substr(uids,12, 50)), visit="V2", outi[[2]][[2]]))


if (i==1) {out=out.i} else
{out=rbind(out, out.i)}
}

write.csv(out, "C:/Users/shpry/Desktop/WFH/Mcdougal/out/MET.out.fun.csv", row.names = FALSE)

out=read.csv("C:/Users/shpry/Desktop/WFH/Mcdougal/out/MET.out.fun.csv")
plot.path="C:/Users/shpry/Desktop/WFH/Mcdougal/plots/ICC/"
i=1
nms=names(out)[-(1:2)]

for (i in 1:length(nms))
{
  ICC.plot.fun (out, nms[i], plot.path)
    
}


