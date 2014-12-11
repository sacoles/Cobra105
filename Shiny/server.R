library(shiny)
library(maptools)
library(maps)
library(RColorBrewer)
library(aplpack)


data(county.fips)
data(us.cities)     
basin<-read.csv(paste(getwd(),"/","basinmavgpm.csv",sep=""))
sjcabfips<-c(06077,06047,06099,06107,06029,06031,06019,06039)
sjcabfipsa<-c("X6077","X6047","X6099","X6107","X6029","X6031","X6019","X6039")
sjcab<-sapply(sjcabfips, function(x) county.fips$polyname[which(county.fips$fips==x)])
sjcab<-sort(sjcab)
months<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

shinyServer(function(input, output) {
  
  
  output$timePlot <- renderPlot({
    date<-round(as.numeric(input$obs),3)
    county<-which(colnames(basin)==input$county)
   basin$decyear<-round(basin$decyear,3)
  
   daterow<-which(basin$decyear==date)
   
   
   par(mar=c(5,4,4,5)+.1)
   colors<-brewer.pal(3,'Set1')
    plot(basin$decyear[1:daterow],basin[c(1:daterow),county],type="l",xlim=c(2001,2012),ylim=c(0,75),
         xlab="Year",ylab="PM 2.5 (ug/cm3)",col=colors[2],main=paste(basin$year[daterow],"\n",months[basin$month[daterow]],sep=""),
         cex.main=1.5,lwd=3)
    points(basin$decyear[daterow],basin[daterow,county],col=colors[2],pch=20,cex=4)
   par(new=TRUE) 
   plot(basin$decyear[1:daterow],basin$area_burned_km2[c(1:daterow)],type="l",col=colors[1],xlim=c(2001,2012),ylim=c(0,650),
        xaxt="n",yaxt="n",xlab="",ylab="",lwd=3)
   axis(4)
   mtext("Burn Area (km2)",side=4,line=3)
   points(basin$decyear[daterow],basin$area_burned_km2[daterow],col=colors[1],pch=20,cex=4)
   axis(4)
   
    
  })
  output$countyPlot<-renderPlot({
    date<-round(as.numeric(input$obs),3)
    county<-which(colnames(basin)==input$county)
    basin$decyear<-round(basin$decyear,3)
    
    daterow<-which(basin$decyear==date)
    
    ncols<-30
    colfunc<-colorRampPalette(c("green","red"))
    colors<-colfunc(ncols)
    mapcolors<-as.vector(sapply(county.fips$fips[sjcab],function(x) floor(basin[daterow,which(colnames(basin)==sjcabfipsa[which(sjcabfips==x)])])))
    mapcolors[which(mapcolors>ncols)]<-ncols
    mapcolors<-sapply(mapcolors,function(x) colors[mapcolors])
    
    map('county',sjcab,fill=TRUE,col=mapcolors,names=TRUE)
    map.cities(x=us.cities,label=TRUE,pch=20,cex=1.5,minpop=120000)
    
  })
})
