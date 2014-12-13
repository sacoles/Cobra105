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
  
  output$about<-renderUI({
    spc<-""
    str1<-"About the Forest Fire and Air Quality Visualization App"
    str2<-"Data Sources"
    str3<-"Forest Fire Data: Obtained from the CALFIRE Fire Area Perimeter shapefile. Fires were clipped to only include those within the drainage
    area of the San Joaquin and Tulare Lake Basins. Area burned was calculated by summing the shape area for each month based on fire ignition date."
    str4<-"Air Quality Data: Obtained from the national EPA daily average PM2.5 dataset. Data was aggregated to monthly average PM 2.5."
    str5<-"Interpretation"
    str6<-"The top pane show a time series of air quality and monthly area burned. The peaks do not occur at the same time, indicating that forest 
    fires are not a significant driver of air quality in the San Joaquin Air Basin. However, small peaks within the troughs indicate forest fires 
    may slightly increase PM 2.5 concentration during the fire season."
    str7<-"The bottom pane shows a map with outlines of each county within the San Joaquin Air Basin. Counties are colored based on the air quality for 
    the time indicated in the time series. The southern end of the basin generally experiences worse air quality than the northern end."
    
    HTML(paste(str1,spc,str2,spc,str3,str4,spc,str5,spc,str6,str7,sep='<br/>'))
    
  })
})
