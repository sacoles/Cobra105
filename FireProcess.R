#Fire Process.R
#Steven Coles (scoles@ucmerced.edu)
#Reads in a table exported from ArcGIS of CALFIRE FRAP with fires clipped to San Joaquin Basin
#Cleans the table by removing unused columns and converting shape area to km2.
#Columns for year, month, and day based on ignition date added.
#POSIXct date column added.
#CSV of cleaned tabled output with filename "firesdata.csv".

fires<-read.csv(paste(getwd(),"/FireData/","FiresClipTableExport.csv",sep=""))

area_burned_km2<-fires$Shape_Area/10^6
fires<-fires[,-c(1,3,4,5,7,10,12,13,14,15,16,17,18)]
fires<-cbind(fires,area_burned_km2)

year<-0*c(1:dim(fires)[2])
mo<-year
day<-year

for(j in seq(from=1, to=dim(fires)[1],by=1)){
  t<-toString(fires[j,3])
  day[j]<-substr(t,nchar(t)-1,nchar(t))
  t<-substr(t,1,nchar(t)-2)
  mo[j]<-substr(t,nchar(t)-1,nchar(t))
  t<-substr(t,1,nchar(t)-2)
  year[j]<-t  
}

DATE<-as.Date(paste(year, mo, day, sep = "-"))

fires$DATE<-DATE

fires$DATE<-as.POSIXct(fires$DATE)
fires$month<-mo

write.csv(fires,"firesdata.csv")