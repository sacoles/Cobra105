#EPA PM2.5 Process Script
#Steven Coles (scoles@ucmerced.edu)
#Takes in raw csv of daily PM 2.5 obtained from EPA website
#Subsets to include only counties within San Joaquin Air Basin using FIPS code.
#Adds columns for year, month, and day, and POSIXct date.
#Exports in one csv time series of PM 2.5 for each county.

filenames<-c("p252001ctymsa.csv","p252002ctymsa.csv","p252003ctymsa.csv","p252004ctymsa.csv",
             "p252005ctymsa.csv","p252006ctymsa.csv","p252007ctymsa.csv","p252008ctymsa.csv",
             "p252009ctymsa.csv","p252010ctymsa.csv","p252011ctymsa.csv")

#Read in data
pmdata.2001<-read.csv(paste(getwd(),"/PMData/",filenames[1],sep=""))
pmdata.2001<-subset(pmdata.2001, pmdata.2001$STATE=="06")

pmdata.2002<-read.csv(paste(getwd(),"/PMData/",filenames[2],sep=""))
pmdata.2002<-subset(pmdata.2002, pmdata.2002$STATE=="06")

pmdata.2003<-read.csv(paste(getwd(),"/PMData/",filenames[3],sep=""))
pmdata.2003<-subset(pmdata.2003, pmdata.2003$STATE=="06")

pmdata.2004<-read.csv(paste(getwd(),"/PMData/",filenames[4],sep=""))
pmdata.2004<-subset(pmdata.2004, pmdata.2004$STATE=="06")

pmdata.2005<-read.csv(paste(getwd(),"/PMData/",filenames[5],sep=""))
pmdata.2005<-subset(pmdata.2005, pmdata.2005$STATE=="06")

pmdata.2006<-read.csv(paste(getwd(),"/PMData/",filenames[6],sep=""))
pmdata.2006<-subset(pmdata.2006, pmdata.2006$STATE=="06")

pmdata.2007<-read.csv(paste(getwd(),"/PMData/",filenames[7],sep=""))
pmdata.2007<-subset(pmdata.2007, pmdata.2007$STATE==6)

pmdata.2008<-read.csv(paste(getwd(),"/PMData/",filenames[8],sep=""))
pmdata.2008<-subset(pmdata.2008, pmdata.2008$STATE==6)

pmdata.2009<-read.csv(paste(getwd(),"/PMData/",filenames[9],sep=""))
pmdata.2009<-subset(pmdata.2009, pmdata.2009$STATE==6)

pmdata.2010<-read.csv(paste(getwd(),"/PMData/",filenames[10],sep=""))
pmdata.2010<-subset(pmdata.2010, pmdata.2010$STATE==6)

pmdata.2011<-read.csv(paste(getwd(),"/PMData/",filenames[11],sep=""))
pmdata.2011<-subset(pmdata.2011, pmdata.2011$STATE==6)

pmdata.2001.2006<-rbind(pmdata.2001,pmdata.2002,pmdata.2003,pmdata.2004,pmdata.2005,pmdata.2006)

pmdata.2007.2011<-rbind(pmdata.2007,pmdata.2008,pmdata.2009,pmdata.2010,pmdata.2011)

pmdata.2001.2006<-pmdata.2001.2006[,-c(4,6)]
pmdata.2007.2011<-pmdata.2007.2011[,-c(4,6)]

pmdata<-rbind(pmdata.2001.2006,pmdata.2007.2011)

sjabcounties<-c(77,99,47,39,19,31,107,29)
sjabcountynames<-c("San Joaquin","Stanislaus","Merced","Madera","Fresno","Kings","Tulare","Kern")

countyrows<-NULL
for(j in seq(from=1, to=length(sjabcounties),by=1)){
  newrows<-which(pmdata$COUNTY==sjabcounties[j])
  countyrows<-c(countyrows,newrows)
}

pmdata<-pmdata[countyrows,]
COUNTY.NAME<-c(1:dim(pmdata)[1])
for(j in seq(from=1,to=length(sjabcounties),by=1)){
  COUNTY.NAME[which(pmdata$COUNTY==sjabcounties[j])]<-sjabcountynames[j]
}

pmdata<-cbind(pmdata,COUNTY.NAME)

pmdata<-pmdata[,-4]

year<-0*c(1:dim(pmdata)[2])
mo<-year
day<-year

for(j in seq(from=1, to=dim(pmdata)[1],by=1)){
  t<-toString(pmdata[j,1])
  year[j]<-substr(t,nchar(t)-3,nchar(t))
  t<-substr(t,1,nchar(t)-4)
  day[j]<-substr(t,nchar(t)-1,nchar(t))
  t<-substr(t,1,nchar(t)-2)
  mo[j]<-t  
}

DATE<-as.Date(paste(year, mo, day, sep = "-"))

pmdata<-pmdata[,-1]
pmdata<-cbind(DATE,pmdata)

pmdata$DATE<-as.POSIXct(pmdata$DATE)
pmdata$year<-year
pmdata$month<-mo
pmdata$day<-day

write.csv(pmdata,"SJCABPM25.csv")


