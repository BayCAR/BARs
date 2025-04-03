
b=1

#source ("H:/WFH/bar2/sim/R/FUN.integration.R")
#source ("H:/WFH/bar2/sim/R/FUN.BCARxy.R")
#source ("H:/WFH/bar2/sim/R/FUN.AdaptP.R")
#source ("C:/Users/shpry/Desktop/WFH/bar2/R/FUN.integration.R")
#source ("C:/Users/shpry/Desktop/WFH/bar2/R/FUN.BCARxy.R")
#source ("/home/shpyang/sim/R/FUN.integration.R")
#source ("/home/shpyang/sim/R/FUN.BCARi.R")
#source ("/home/shpyang/sim/R/FUN.AdaptP.R")
#source ("C:/Users/shpry/Desktop/WFH/bar2/R/FUN.cBCARA.R")
#source ("/home/shpyang/sim/R/FUN.cBCARA.R")
#source ("/home/shpyang/sim/R/FUN.xyBCARA.R")
#source ("H:/WFH/bar2/sim/R/FUN.xyBCAR.R")
#source ("/work/shpyang/bar2/R/FUN.BayCARA.R")
#source ("C:/Users/shpry/Desktop/WFH/bar2/R/BayCARA.R")
#source ("/work/shpyang/p6/R/BayCARA.R")
#source ("H:/WFH/bar2/sim/R/BayCARA.R")
# source ("C:/Users/shpry/Desktop/WFH/bar2/new/R/tBayCARA.R")

source ("H:/WFH/bar2/new/R/tBayCARA.R")

planned.sample.size=220  
delay=.5  # unit is year; .5 = 6mon
delays=floor(planned.sample.size/4*delay)

premu=82; presize=1; p3=1; p4=50^2

mu0=82;mu1=84; mu2=86; mu3=88; mu4=90

#mu0= mu1= mu2= mu3= mu4=82
premu=c(mu1,mu2,mu3,mu4)
sd0=sd1=sd2=sd3=sd4= 12

data = data.frame("gender" = rep(NA, planned.sample.size), "age"=NA,
                  continuous1=NA, continuous2=NA, group=NA, y=NA, stringsAsFactors = TRUE)

group.level=c("0", "A", "B", "C", "D")
tgroup.level=group.level[-1]
ratio=rep(1, length(group.level))
ratio=ratio/sum(ratio)

data$gender[1:length(group.level)] =sample(c("female", "male"), length(group.level), TRUE, c(.6, .4)) 
data$age[1:length(group.level)]=sample(c("<=40", "40-65", ">65"), length(group.level), TRUE, c(.5, .5, .5))  
data$continuous1[1:length(group.level)]=rnorm(length(group.level))
data$continuous2[1:length(group.level)]=rnorm(length(group.level))
data$group[1:length(group.level)]=sample(group.level, length(group.level))

n.runin=round(planned.sample.size/3) 
si=start=sum(!is.na(data$group))+1
pRA= rep(1, length(group.level))#[-1]

for (si in start:planned.sample.size)
  #  for (si in start:75)
{data$gender[si] = sample(c("female", "male"), 1, TRUE, c(.6, .4))
data$age[si]= sample(c("<=40", "40-65", ">65"), 1, TRUE, c(.5, .5, .5)) 
data$continuous1[si]=rnorm(1)
data$continuous2[si]=rnorm(1)


#
#interims=n.runin+(0:2)*round(147/3)
#if (si%in%interims)
  if (si>n.runin)
{tdata=data.frame(trt=data$group[!is.na(data$group)],
                  y=data$y[!is.na(data$group)])
tdata=tdata[tdata$trt!="0",]
#tdata=tdata[!is.na(tdata$y),]
tdata=tdata[order(tdata$trt),]

pRA=as.numeric(postP.Tfun(tdata, premu, presize, p3, p4)$PosteriorP )
pRA=c(max(pRA)[1], pRA)
pRA=pRA/sum(pRA)

ratio=pRA
############print(si)
###################print(ratio)
}

outRA=c(b, si, ratio  )
outn=c(b, si, table(data$group) )

if (si==start) {outRAp=outRA
outnn=outn} else {outRAp=rbind(outRAp, outRA)
outnn=rbind(outnn, outn)}

datat=data

runF=F.BayCARA(data, categorical.covariates = c("gender", "age"), 
                #ratio=ratio,
                continuous.covariates = c("continuous1", "continuous2"), group.level=group.level, planned.sample.size=planned.sample.size ,
                pRA=pRA, transformation.parameter=1/2 )
data$group[si]=runF[[1]]

runF0=F.BayCARA(datat, categorical.covariates = c("gender", "age"), 
                #ratio=ratio,
                continuous.covariates = c("continuous1", "continuous2"), group.level=group.level, planned.sample.size=planned.sample.size ,
                pRA=pRA*0+1, transformation.parameter=1/2 )
 
T1=table(data$gender, data$group)
if (nrow(T1)==1) {T1=rbind(T1, rep(0, length(group.level)))}

for (i in 1:dim(T1)[[2]])
{
  T2=cbind(T1[,1],  T1[, i] )
  rT2=sum(T2[1,])/sum(T2)
  erT2=colSums(T2)*rT2
  
  if (i==1) {imb=sum(abs(T2[1,]-erT2))/sum(T2)} else
  {imb=c(imb,sum(abs(T2[1,]-erT2))/sum(T2))}
}

imb=c(b, si, imb)

bpp=c(b, si, runF[[2]], runF0[[2]], ratio, runF0[[2]]*ratio )

opp=c(b, si, runF[[2]] )
if (si==start) {imbs=imb; bppp=bpp; oppp=opp} else {imbs=rbind(imbs, imb); bppp=rbind(bppp,bpp); oppp=rbind(oppp,opp)}



if (si> delays)
{if (data$group[si-delays]=="0") {data$y[si-delays]=rnorm(1, mu0, sd0)}
  if (data$group[si-delays]=="A") {data$y[si-delays]=rnorm(1, mu1, sd1)}
  if (data$group[si-delays]=="B") {data$y[si-delays]=rnorm(1, mu2, sd2)}
  if (data$group[si-delays]=="C") {data$y[si-delays]=rnorm(1, mu3, sd3)}
  if (data$group[si-delays]=="D") {data$y[si-delays]=rnorm(1, mu4, sd4)}
}
}

datay=data[!is.na(data$y),]
datan=data[is.na(data$y),]

datan$y[datan$group=="0"]=rnorm(sum(datan$group=="0"), mu0, sd0)
datan$y[datan$group=="A"]=rnorm(sum(datan$group=="A"), mu1, sd1)
datan$y[datan$group=="B"]=rnorm(sum(datan$group=="B"), mu2, sd2)
datan$y[datan$group=="C"]=rnorm(sum(datan$group=="C"), mu3, sd3)
datan$y[datan$group=="D"]=rnorm(sum(datan$group=="D"), mu4, sd4)

data=rbind(datay, datan)

#table(data$gender, data$group)
#table(data$age, data$group)
Bout=table(data$group)
Bout
#boxplot(data$continuous1~data$group)

#
#boxplot(data$y~data$group)
#boxplot(data$continuous1 ~data$group)
#round(ratio/sum(ratio)*planned.sample.size)
table(data$gender, data$group)

prt=rbind(Bout,
          c(round(mean(data$y[data$group=="0"]),2),
            round(mean(data$y[data$group=="A"]),2),
            round(mean(data$y[data$group=="B"]),2),
            round(mean(data$y[data$group=="C"]),2),
            round(mean(data$y[data$group=="D"]),2)))


prt=data.frame(prt)


inc=c("0",names(prt)[1+ which(prt[2,2:5]==max(prt[2,2:5]))[1]])


ttdata=data.frame(trt=data$group[!is.na(data$group)],
                  y=data$y[!is.na(data$group)])
ttdata=ttdata[ttdata$trt%in%inc,]

ttdata=ttdata[order(ttdata$trt),]


ppRA=postP.Tfun(ttdata, c(mu0 ), presize, p3, p4)
ppRA2=as.numeric(ppRA[,2])
ppRA2=sqrt(ppRA2/sum(ppRA2))
pwr=ppRA2[2]






