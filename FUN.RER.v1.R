library("zoo")
library('readxl')
library('splines2')
library('survival')
library('splines')

library(readxl)
library(irr)
library(psych)
library(BlandAltmanLeh)

 
  
  
  MET.plot.fun=function(data.path, uids, out.path, plot.path, pairs)
{if (missing(pairs)) {pairs=FALSE}
   if (pairs%in%c("Yes", "yes", "paired", "pairs", "PAIR", "TRUE")) 
      { tiff(paste0(plot.path,uids, ".tif"), width=5600, height = 3800, res=600, compression = 'zip')
      par(mfrow=c(2,1), mar=c(3,5,1,1))
      uid1=paste0(uids, "1")
      uid2=paste0(uids, "2")
      out1=MET.fun(data.path, uid1, out.path, plot.path)
      out2=MET.fun(data.path, uid2, out.path, plot.path)
      return(list(out1, out2))
      dev.off()
      
      } else
      { tiff(paste0(plot.path,uids, ".tif"), width=5600, height = 2000, res=600, compression = 'zip')
       par(mfrow=c(1,1), mar=c(3,5,1,1))
       out=MET.fun(data.path, uids, out.path, plot.path)
       return(out)
       dev.off()
      }
  
  }
  
  
  MET.fun=function(data.path, uid, out.path, plot.path)
  {f.i1=paste0(data.path,uid, ".xlsx")
    
  d1=read_excel(f.i1, skip=9) 
  d1=d1[!is.na(d1$RecalcVCO2),] 
  d1$ind=1:nrow(d1) 

  d1$RER=d1$RecalcVCO2/d1$RecalcVO2
  d1$time=  strptime(d1$Time, format = "%Y-%m-%d %H:%M:%S")
  d1$ntime=as.numeric(d1$time)
  d1$time2=  as.POSIXct(d1$Time, format = "%Y-%m-%d %H:%M:%S")
  d1$time2=format(d1$time2, "%H:%M:%S")
  
  d1$RER[d1$RER>1.2 | d1$RER<0.6]=NA
  
  start <- paste0(as.Date(d1$time)[1]," 19:00:00 CDT")
  start <- as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S")
  d1$dtime <- as.numeric(difftime(d1$time,  start, units = "secs"))
  d1$etime <- as.numeric(difftime(d1$time,  start+12*3600, units = "secs"))
  d1$ee11time <- as.numeric(difftime(d1$time,  start-3600, units = "secs"))
  d1$ee12time <- as.numeric(difftime(d1$time,  start-3600/4, units = "secs"))
  d1$ee21time <- as.numeric(difftime(d1$time,  start+11*3600+900, units = "secs"))
  d1$ee22time <- as.numeric(difftime(d1$time,  start+12*3600, units = "secs"))
  ee11whs=which(abs(d1$ee11time)==min(abs(d1$ee11time)))
  ee12whs=which(abs(d1$ee12time)==min(abs(d1$ee12time)))
  ee21whs=which(abs(d1$ee21time)==min(abs(d1$ee21time)))
  ee22whs=which(abs(d1$ee22time)==min(abs(d1$ee22time)))
  
  d.ee=2700*(mean(d1$RecalcEE[ee11whs:ee12whs])-mean(d1$RecalcEE[ee21whs:ee22whs]))
   
  ewhs=which(abs(d1$etime)==min(abs(d1$etime)))
  whs=which(abs(d1$dtime)==min(abs(d1$dtime)))
  whs2=whs+3*60#+15
  whs3=whs+4*60#+15
  whs4=whs+10*60#+15  
 
  df=7
  lm1=lm(RER~ns(as.numeric(time), df=df), data=d1)
  lm1p=predict(lm1)
  p1=as.data.frame(t(t(lm1p)))
  p1$ind=as.numeric(row.names(p1))
  mg1=merge(p1, d1,by="ind", all.y=TRUE)
  p7_a7.activity=mean(d1$Activity[whs:ewhs])
  p7_a7.EE=mean(d1$RecalcEE[whs:ewhs])
  p7_a7.RER=mean(mg1$V1[whs:ewhs],na.rm=TRUE)
  
  peak=which(mg1$V1[whs:whs2]==max(mg1$V1[whs:whs2], na.rm=TRUE))+whs
  low=which(mg1$V1[whs3:whs4]==min(mg1$V1[whs3:whs4], na.rm=TRUE))+whs3
  
  plot(mg1$time, mg1$RER, pch=16, cex=.2, xlab="Time", ylab="RER", xaxt="n", col=8, ylim=c(.3, 1.2), main=uid)
  arrows(mg1$ntime[whs], .4, as.numeric(mg1$time[whs]), 1, lty=2, col=5, lwd=2, length = 0)
  # abline(h=mg1$V1[whs], col=2, lty=2, lwd=1.5)
  baseline.RQ=mean(mg1$V1[ee11whs:ee12whs])
  abline(h=baseline.RQ, col=2, lty=2, lwd=1.5)
  axis(1, mg1$ntime[whs], "19:00", cex.axis=.7)
  axis(1, mg1$ntime[whs2], "22:00", cex.axis=.7)
  axis(1, mg1$ntime[whs3], "23:00", cex.axis=.7)
  axis(1, mg1$ntime[whs4], "05:00", cex.axis=.7)
  axis(1, mg1$ntime[ewhs], "07:00", cex.axis=.7)
  
  arrows(mg1$ntime[c(whs )], .75, mg1$ntime[c(peak)], .75, col=6, length=.05, angle=90)
  arrows(mg1$ntime[c(peak)], .75, mg1$ntime[c(whs )], .75, col=6, length=.05, angle=90)
  text(mg1$ntime[round((peak+whs)/2)], .78, "Time to peak", cex=.5, col=6)
  
  time.7p.peak=(mg1$ntime[peak]-mg1$ntime[whs])/3600
  
  arrows(mg1$ntime[c(whs )], .5, mg1$ntime[c(low)], .5, col=6, length=.05, angle=90)
  arrows(mg1$ntime[c(low)], .5, mg1$ntime[c(whs )], .5, col=6, length=.05, angle=90)
  text(mg1$ntime[round((low+whs)/2)], .53, "Time to nadir", cex=.5, col=6)
  
  time.7p.nadir=(mg1$ntime[low]-mg1$ntime[whs])/3600
  total.d.RER=mg1$V1[peak]-mg1$V1[low]
  d.RER.peak=mg1$V1[peak]-baseline.RQ
  d.RER.nadir=baseline.RQ-mg1$V1[low]
  peak.RER=mg1$V1[peak]
  nadir.RER=mg1$V1[low]
  slope.RER=(nadir.RER-peak.RER)/(time.7p.nadir-time.7p.peak)
  
  # axis(1, mg1$ntime[whs], "19:00", line=0, col = "white")
  arrows(mg1$ntime[peak], baseline.RQ, as.numeric(mg1$time[peak]), peak.RER, col=4, length = 0.05, lwd=2)
  arrows(mg1$ntime[peak], peak.RER, as.numeric(mg1$time[peak]), baseline.RQ, col=4, length = 0.05, lwd=2)
  arrows(mg1$ntime[low], nadir.RER, mg1$ntime[low], peak.RER, col=4, length = 0.05, lwd=2)
  arrows(mg1$ntime[low], peak.RER, mg1$ntime[low], nadir.RER, col=4, length = 0.05, lwd=2)
  
  #abline(v=as.numeric(c(mg1$time[c(whs, whs2)])), col=6)
  arrows(mg1$ntime[c(whs )], .4, mg1$ntime[c(whs2)], .4, col="darkblue", length=.05, angle=90)
  arrows(mg1$ntime[c(whs2)], .4, mg1$ntime[c(whs )], .4, col="darkblue", length=.05, angle=90)
  text(mg1$ntime[round((whs2+whs)/2)], .43, "Meal stimulation period", cex=.5, col="darkblue")
  text(mg1$ntime[low]-1300, (mg1$V1[low]+mg1$V1[peak])/2, "Total \nΔRER", cex=.5, col="darkblue")
  
  lines(as.numeric(mg1$time), mg1$V1, col=1, lwd=3)
  
  zm=rollmean(mg1$RER, k=55)
  
  out.if=data.frame(baseline.RQ, time.7p.peak, time.7p.nadir, total.d.RER, 
                    peak.RER, nadir.RER, p7_a7.RER, slope.RER, 
                    p7_a7.activity, p7_a7.EE, d.RER.peak, d.RER.nadir)
  
  out.i=rbind(data.frame(ID=paste0(substr(uid,12, 50)), visit="V1", out.ib),
              data.frame(ID=paste0(substr(uid,12, 50)), visit="V2", out.if))
  
  
  if (i==1) {out=out.i} else
  {out=rbind(out, out.i)}
  #  lines(as.numeric(d2$time)[-c(1:12)], zm[!is.na(zm)][1:length(as.numeric(d2$time)[-c(1:12)])], col=4, lwd=3, lty=1)
  
  zm=rollmean(mg1$RER, k=55)
  text(mg1$ntime[ee21whs], .7, paste0("Baseline.RQ=", round(baseline.RQ, 2)), cex=.6)
  text(mg1$ntime[ee21whs], .65, paste0("Time to peak=", round(time.7p.peak, 2), "hrs"), cex=.6)
  text(mg1$ntime[ee21whs], .6, paste0("Time to nadir=", round(time.7p.nadir, 2), "hrs"), cex=.6)
  text(mg1$ntime[ee21whs], .55 , paste0("Peak RER=", round(peak.RER, 2)), cex=.6)
  text(mg1$ntime[ee21whs], .5 , paste0("Nadir RER=", round(nadir.RER, 2)), cex=.6)
  text(mg1$ntime[ee21whs], .45 , paste0("Slope of RER=", round(slope.RER, 2)), cex=.6)
  text(mg1$ntime[ee21whs], .4 , paste0("Δ RER peak=", round(d.RER.peak, 3)), cex=.6)
  text(mg1$ntime[ee21whs], .35 , paste0("Δ RER nadir=", round(d.RER.nadir, 3)), cex=.6)
  text(mg1$ntime[ee21whs], .3, paste0("Total Δ RER=", round(total.d.RER, 3)), cex=.6)

 #zm=rollmean(mg1$RER, k=55)
  
 #dd1[[i]]=d1
 #rm(d1); rm(mg1)
 
 out.ib=data.frame(baseline.RQ, time.7p.peak, time.7p.nadir, total.d.RER, 
                   peak.RER, nadir.RER, p7_a7.RER, slope.RER, 
                   p7_a7.activity, p7_a7.EE, d.RER.peak, d.RER.nadir)
 
 return=list(mg1, out.ib)
 }
  
   
  
  
  
  ICC.plot.fun=function(out, nmsi, plot.path)
  {y1=as.vector(out[out$visit=="V1",nmsi])
  y2=out[out$visit=="V2",nmsi]
  
  mgi=data.frame(y1, y2)
  
  iout=ICC(mgi) 
  
  iiout= iout[[1]][2,]
  
  mixy=min(mgi, na.rm=TRUE) 
  maxy=max(mgi, na.rm=TRUE)
  dif=maxy-mixy
  
  mixy=mixy-dif*.1
  maxy=maxy+dif*.1
  
  tiff(paste(plot.path, nmsi,".tiff", sep=""), width = 8, height = 4, units = 'in',  res = 1200, compression = "zip" ) 
  par(mfrow=c(1,2))
  
  
  plot(mgi[,1], mgi[,2],pch=16, xlim=c(mixy, maxy), ylim=c(mixy, maxy),
       main=nms[i],   xlab="Visit 1", ylab="Visit 2")
  abline(0,1, col=2)
  pp=round(iout[[1]][2,]$p,3)
  if (pp==0) {pp="<0.001"} else
  {pp=paste0("=",pp[pp!=0])}
  bland.altman.plot(mgi[,2], mgi[,1],main=paste0(nms[i]," (ICC=", round(iout[[1]][2,]$ICC,2), "; P",pp, ")"),pch=16, xlab="Means", ylab="Differences")
  abline(h=0, col=2)
  
  dev.off()
  }