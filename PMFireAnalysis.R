#PMFireAnalysis.R
#Steven Coles (Scoles@ucmerced.edu)
#Reads in basin PM 2.5 file from EPA process.
#Reads in fires data from Fire process.
#Aggregates data to monthly mean and max.
#Generates time series plots, and PM 2.5 as a function of burn area.
#Generates linear models and determines p-value and R2
#Calculates monthly mean for observation period, and deviations from mean.

#Read in data
pmdata<-read.csv(paste(getwd(),"/","SJCABPM25.csv",sep=""))
fires<-read.csv(paste(getwd(),"/","firesdata.csv",sep=""))

pmdata$DATE<-as.POSIXct(pmdata$DATE)

sjcpm<-pmdata[which(pmdata$COUNTY.NAME=="San Joaquin"),]
sjcsubset<-subset(pmdata,pmdata$COUNTY.NAME=="San Joaquin")

kingspm<-pmdata[which(pmdata$COUNTY.NAME=="Kings"),]

plot(kingspm$DATE,kingspm$PM25_CONCENTRATION)

plot(fires$DATE,fires$area_burned_km2)

month<-NULL
year<-NULL
for(j in seq(from=2001,to=2011,by=1)){
  for(k in seq(from=1, to=12, by=1)){
    year<-c(year,j)
    month<-c(month,k)
  }
}

area_burned_km2<-NULL
for(j in seq(from=1, to=length(year),by=1)){
  firerows<-which(fires$YEAR_==year[j])
  firerows<-firerows[which(fires$month[firerows]==month[j])]
  burned<-sum(fires$area_burned_km2[firerows])
  area_burned_km2<-c(area_burned_km2,burned)
}

firesum<-as.data.frame(cbind(year,month,area_burned_km2))

month<-NULL
year<-NULL
for(j in seq(from=2001,to=2011,by=1)){
  for(k in seq(from=1, to=12, by=1)){
    year<-c(year,j)
    month<-c(month,k)
  }
}

fresnomavg<-NULL
kernmavg<-NULL
kingsmavg<-NULL
mercedmavg<-NULL
sanjoaquinmavg<-NULL
stanislausmavg<-NULL
tularemavg<-NULL
basinmavg<-NULL
sjabcountynames<-c("San Joaquin","Stanislaus","Merced","Fresno","Kings","Tulare","Kern")
for(j in seq(from=1, to=length(year),by=1)){
  yearrows<-which(pmdata$year==year[j])
  yearmonthrows<-yearrows[which(pmdata$month[yearrows]==month[j])]
  
  ymavg<-mean(pmdata$PM25_CONCENTRATION[yearmonthrows],na.rm=TRUE)
  basinmavg<-c(basinmavg,ymavg)
  
  crows<-yearmonthrows[which(pmdata$COUNTY.NAME[yearmonthrows]==sjabcountynames[1])]
  sanjoaquinmavg<-c(sanjoaquinmavg,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-yearmonthrows[which(pmdata$COUNTY.NAME[yearmonthrows]==sjabcountynames[2])]
  stanislausmavg<-c(stanislausmavg,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-yearmonthrows[which(pmdata$COUNTY.NAME[yearmonthrows]==sjabcountynames[3])]
  mercedmavg<-c(mercedmavg,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-yearmonthrows[which(pmdata$COUNTY.NAME[yearmonthrows]==sjabcountynames[4])]
  fresnomavg<-c(fresnomavg,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-yearmonthrows[which(pmdata$COUNTY.NAME[yearmonthrows]==sjabcountynames[5])]
  kingsmavg<-c(kingsmavg,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-yearmonthrows[which(pmdata$COUNTY.NAME[yearmonthrows]==sjabcountynames[6])]
  tularemavg<-c(tularemavg,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-yearmonthrows[which(pmdata$COUNTY.NAME[yearmonthrows]==sjabcountynames[7])]
  kernmavg<-c(kernmavg,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
}
burnlog<-log(area_burned_km2)
logbasinmavg<-log(basinmavg)
basin<-as.data.frame(cbind(year,month,basinmavg,fresnomavg,kernmavg,
                                kingsmavg,mercedmavg,
                                sanjoaquinmavg,
                                stanislausmavg,
                                tularemavg,area_burned_km2,burnlog,logbasinmavg))

firetime<-firesum$year+(firesum$month/12)
plot(firesum$year+(firesum$month/12),firesum$area_burned_km2,cex=.75,pch=20,col="red",
     type="b")
#par(new=TRUE)
decyears<-basin$year+(basin$month/12)
basin<-cbind(basin,decyears)

sjcpmtime<-sjcpm$year+(sjcpm$month/12)+(sjcpm$day/365)
#plot(sjcpmtime,sjcpm$PM25_CONCENTRATION,xaxt="n",yaxt="n",type="l",lwd=.5,cex=.75,col="blue")
#axis(4)

summerrows<-which(basin$month>5)
summerrows<-which(basin$month[summerrows]<11)

#plot(log(basin$basinmavg[summerrows]),log(basin$area_burned_km2[summerrows]))

#loglm<-lm(log(area_burned_km2)~log(basinmavg),data=basin[summerrows,],na.omit)

par(mfrow=c(3,3))

for(j in seq(from=3, to=10,by=1)){
  x<-basin[,j]
  y<-basin$area_burned_km2
  #rho=cor(x,y)
  plot(basin[,j],basin$area_burned_km2,main=paste(colnames(basin)[j],"Corr:"),
       xlab="Monthly average PM 2.5 Concentration (ug/m3)",
       ylab="Area Burned (km2)")
}
par(mfrow=c(1,1))
par(mfrow=c(3,3))
for(j in seq(from=3, to=10,by=1)){
  plot(basin[,1]+basin$month/12,basin[,j],main=colnames(basin)[j],
       xlab="Year",
       ylab="PM 2.5 Concentration (ug/m3)",
       type="b")
}
#Anomaly Analysis
basinavg<-NULL
months<-c(1:12)
sanjoaquinavgpm<-NULL
stanislausavgpm<-NULL
mercedavgpm<-NULL
fresnoavgpm<-NULL
kingsavgpm<-NULL
tulareavgpm<-NULL
kernavgpm<-NULL
for(j in seq(from=1, to=length(months),by=1)){
  
  monthrows<-which(pmdata$month==j)
  
  mavg<-mean(pmdata$PM25_CONCENTRATION[monthrows],na.rm=TRUE)
  basinavg<-c(basinavg,mavg)
  
  crows<-monthrows[which(pmdata$COUNTY.NAME[monthrows]==sjabcountynames[1])]
  sanjoaquinavgpm<-c(sanjoaquinavgpm,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-monthrows[which(pmdata$COUNTY.NAME[monthrows]==sjabcountynames[2])]
  stanislausavgpm<-c(stanislausavgpm,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-monthrows[which(pmdata$COUNTY.NAME[monthrows]==sjabcountynames[3])]
  mercedavgpm<-c(mercedavgpm,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-monthrows[which(pmdata$COUNTY.NAME[monthrows]==sjabcountynames[4])]
  fresnoavgpm<-c(fresnoavgpm,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-monthrows[which(pmdata$COUNTY.NAME[monthrows]==sjabcountynames[5])]
  kingsavgpm<-c(kingsavgpm,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-monthrows[which(pmdata$COUNTY.NAME[monthrows]==sjabcountynames[6])]
  tulareavgpm<-c(tulareavgpm,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
  
  crows<-monthrows[which(pmdata$COUNTY.NAME[monthrows]==sjabcountynames[7])]
  kernavgpm<-c(kernavgpm,mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE))
}

baselines<-as.data.frame(cbind(months,basinavg,sanjoaquinavgpm,stanislausavgpm,
                               mercedavgpm,fresnoavgpm,kingsavgpm,tulareavgpm,
                               kernavgpm))

month<-NULL
year<-NULL
for(j in seq(from=2001,to=2011,by=1)){
  for(k in seq(from=1, to=12, by=1)){
    year<-c(year,j)
    month<-c(month,k)
  }
}
basinanom<-NULL
months<-c(1:12)
sanjoaquinanom<-NULL
stanislausanom<-NULL
mercedanom<-NULL
fresnoanom<-NULL
kingsanom<-NULL
tulareanom<-NULL
kernanom<-NULL

for(i in seq(from=2001,to=2011,by=1)){
  for(j in seq(from=1, to=12,by=1)){
    
    rows<-which((pmdata$year==i)&(pmdata$month==j))
    
    mavg<-mean(pmdata$PM25_CONCENTRATION[rows],na.rm=TRUE)
    anom<-mavg-baselines[j,2]
    basinanom<-c(basinanom,anom)
    
    crows<-monthrows[which(pmdata$COUNTY.NAME[rows]==sjabcountynames[1])]
    mavg<-mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE)
    anom<-mavg-baselines[j,3]
    sanjoaquinanom<-c(sanjoaquinanom,anom)
    
    crows<-monthrows[which(pmdata$COUNTY.NAME[rows]==sjabcountynames[2])]
    mavg<-mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE)
    anom<-mavg-baselines[j,4]
    stanislausanom<-c(stanislausanom,anom)
    
    crows<-monthrows[which(pmdata$COUNTY.NAME[rows]==sjabcountynames[3])]
    mavg<-mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE)
    anom<-mavg-baselines[j,5]
    mercedanom<-c(mercedanom,anom)
    
    crows<-monthrows[which(pmdata$COUNTY.NAME[rows]==sjabcountynames[4])]
    mavg<-mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE)
    anom<-mavg-baselines[j,6]
    fresnoanom<-c(fresnoanom,anom)
    
    crows<-monthrows[which(pmdata$COUNTY.NAME[rows]==sjabcountynames[5])]
    mavg<-mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE)
    anom<-mavg-baselines[j,7]
    kingsanom<-c(kingsanom,anom)
    
    crows<-monthrows[which(pmdata$COUNTY.NAME[rows]==sjabcountynames[6])]
    mavg<-mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE)
    anom<-mavg-baselines[j,8]
    tulareanom<-c(tulareanom,anom)
    
    crows<-monthrows[which(pmdata$COUNTY.NAME[rows]==sjabcountynames[7])]
    mavg<-mean(pmdata$PM25_CONCENTRATION[crows],na.rm=TRUE)
    anom<-mavg-baselines[j,9]
    kernaanom<-c(kernanom,anom)
  }
}

anomalies<-as.data.frame(cbind(year,month,basinanom,sanjoaquinanom,stanislausanom,
                               mercedanom,fresnoanom,kingsanom,tulareanom,
                               kernanom))
burnarea<-firesum$area_burned_km2
anomalies<-cbind(anomalies,burnarea)
par(mfrow=c(1,1))
anomsub<-subset(anomalies,anomalies$burnarea>15)
logburn<-log(anomsub$burnarea)
anomsub<-cbind(anomsub,logburn)
plot(anomsub$burnarea,anomsub$basinanom,cex=.75,pch=20)

basinlm<-lm(basinanom~burnarea,data=anomsub)
abline(basinlm)

sanjoaquinlm<-lm(sanjoaquinanom~burnarea,data=anomsub)
stanislauslm<-lm(sanjoaquinanom~burnarea,data=anomsub)
mercedlm<-lm(sanjoaquinanom~burnarea,data=anomsub)
fresnolm<-lm(sanjoaquinanom~burnarea,data=anomsub)
kingslm<-lm(sanjoaquinanom~burnarea,data=anomsub)
tularelm<-lm(sanjoaquinanom~burnarea,data=anomsub)

go=FALSE
if(go){
par(mfrow=c(2,2))
plot(basin$year+basin$month/12,basin$basinmavg,main="Basin Monthly Average Air Quality",ylab="PM 2.5 (ug/cm3)",xlab="Year",type="b")
plot(basin$year+basin$month/12,basin$area_burned_km2,main="Monthly Area Burned",ylab="Burn Area (km2)",xlab="Year",type="b")
acf(basin$basinmavg,main="Basin Monthly Average PM2.5",cex.main=.5)
acf(basin$area_burned_km2, main="Monthly Area Burned",cex.main=.5)
par(mfrow=c(1,1))

basinsub<-subset(basin,basin$area_burned_km2>0)
burnlog<-log(basinsub$area_burned_km2)
basinlog<-log(basinsub$basinmavg)
basinsub<-cbind(basinsub,burnlog,basinlog)
plot(basinsub$burnlog,basinsub$basinlog)
basinlm<-lm(basinlog~burnlog,data=basinsub)
abline(basinlm)
print(summary(basinlm))


par(mfrow=c(2,2))
hist(basin$area_burned_km2,xlab="Area Burned (km2)",main="Monthly Burn Area",cex.main=1)
hist(basin$basinmavg,xlab="PM 2.5 (ug/cm3)",main="Air Quality",cex.main=1)
hist((log(basin$area_burned_km2)),xlab="Log Area Burned",main="Log Monthly Burn Area",cex.main=1)
hist(log(basin$basinmavg),xlab="Log PM 2.5",main="Log Air Quality",cex.main=1)

par(mfrow=c(1,3))
plot(basin$area_burned_km2,basin$basinmavg,xlab="Area Burned (km2)",ylab="Monthly Average PM 2.5 (ug/cm3)",main="All Months")
basinlm<-lm(basinmavg~area_burned_km2,data=basin)
abline(basinlm)
plot(basinsub$area_burned_km2,basinsub$basinmavg,xlab="Area Burned (km2)",ylab="Monthly Average PM 2.5 (ug/cm3)",main="Months with Burned Area > 0")
basinsublm<-lm(basinmavg~area_burned_km2,data=basinsub)
abline(basinsublm)
plot(basin$burnlog,basin$logbasinmavg,xlab="Log Area Burned",ylab="Log Monthly Average PM 2.5",main="Log Transform, Burn Area > 0")
basinsubloglm<-lm(logbasinmavg~burnlog,data=basinsub)
abline(basinsubloglm)

plot(yeardays,anomalies$basinanom,xlab="Year",ylab="PM 2.5 Anomaly (ug/cm3)",main="Basin Average Monthly  \n Air Quality Anomalies")
abline(h=0,lty=2)
hist(anomalies$basinanom,main="PM 2.5 Anomalies",xlab="PM 2.5 Anomaly (ug/cm3)")
plot(anomsub$logburn,anomsub$basinanom,main="Log Transform \n Months with Burn Area > 0",xlab="Log Area Burned (km2)",
     ylab="PM 2.5 Anomaly (ug/cm3)")
anomlm<-lm(basinanom~logburn,data=anomsub)
abline(anomlm)
}