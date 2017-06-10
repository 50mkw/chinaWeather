#' match the color with ADCODE99.
#'
#' @param temp the temperature
#' @param breaks cut the numbers
#' @return new color vector
#' @keywords color
getColors<-function(temp,breaks){
  f=function(x,y) ifelse(x %in% y,which(y==x),0)
  colIndex=sapply(chinaMap$ADCODE99,f,WOEID$adcode99)

  arr <- findInterval(temp, breaks)
  arr[which(is.na(arr))]=19
  return(arr[colIndex])
}

#' match the color with ADCODE99.
#'
#' @param map the map data
#' @param prov the province
#' @param ctype the color type
#' @return new color vector
#' @keywords color
getColors2<-function(map,prov,ctype){
  #name change to ADCODE99
  ADCODE99<-read.csv(file="ADCODE99.csv",header=TRUE,fileEncoding="utf-8", encoding="utf-8")
  fc<-function(x){ADCODE99$ADCODE99[which(x==ADCODE99$prov)]}
  code<-sapply(prov,fc)

  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(map$ADCODE99,f,code);
  ctype[which(is.na(ctype))]=19
  return(ctype[colIndex])
}

#' Draw the background.
#'
#' @param title the image's title
#' @param date the date
#' @param lang the language zh or en
drawBackground<-function(title,date,lang='zh'){
  text(100,58,title,cex=2)
  text(105,54,format(date,"%Y-%m-%d"))
  #text(98,65,paste('chinaweatherapp','http://apps.weibo.com/chinaweatherapp'))
  #text(120,-8,paste('provided by The Weather Channel',format(date, "%Y-%m-%d %H:%M")),cex=0.8)
}

#' Draw the description.
#'
#' @importFrom stringi stri_unescape_unicode
#' @param data daily data
#' @param temp the temperature
#' @param lang the language zh or en
drawDescription<-function(data,temp,lang='zh'){
  rows<-1:nrow(data)
  x<-ceiling(rows/7)*11+68
  y<-17-ifelse(rows%%7==0,7,rows%%7)*3
  fontCols<-c("#08306B","#000000","#800026")[findInterval(temp,c(0,30))+1]
  if(lang=='zh'){
    txt<-stri_unescape_unicode(data$zh)
    text(x,y,paste(txt,temp),col=fontCols)
  }else{
    text(x,y,paste(data$en,temp),col=fontCols)
  }
  #text(x,y,bquote(paste(.(data$en),.(temp),degree,C)),col=fontCols)
}

#' Draw the legend.
#'
#' @param breaks cut the numbers
#' @param colors match the color
drawLegend<-function(breaks,colors){
  breaks2 <- breaks[-length(breaks)]
  par(mar = c(5, 0, 15, 10))
  image(x=1, y=0:length(breaks2),z=t(matrix(breaks2)),col=colors[1:length(breaks)-1],axes=FALSE,breaks=breaks,xlab="",ylab="",xaxt="n")
  axis(4, at = 0:(length(breaks2)), labels = breaks, col = "white", las = 1)
  abline(h = c(1:length(breaks2)), col = "white", lwd = 2, xpd = FALSE)
}

#' Draw temperature picture.
#'
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringi stri_unescape_unicode
#' @import maptools
#' @param data daily data
#' @param lang language
#' @param type low or high
#' @param date the date
#' @param output output a file or not
#' @param path image output position
#' @export
drawTemperature<-function(data,lang='zh',type='high',date=Sys.time(),output=FALSE,path=''){
  colors <- c(rev(brewer.pal(9,"Blues")),"#ffffef",brewer.pal(9,"YlOrRd"),"#500000")
  breaks=seq(-36,44,4)

  if(type=='high') {
    #temp<-data$high
    temp <- round((data$high-32)*5/9,1)
    ofile<-paste(format(date,"%Y%m%d"),"_day.png",sep="")
  }else{
    #temp<-data$low
    temp <- round((data$low-32)*5/9,1)
    ofile<-paste(format(date,"%Y%m%d"),"_night.png",sep="")
  }

  if(lang=='zh'){
    title<-stri_unescape_unicode(props[which(props$key=='high'),]$zh)
  }else{
    title<-props[which(props$key=='high'),]$en
  }

  if(output)png(filename=paste(path,ofile,sep=''),width=600,height=600)
  par(family='STKaiti') #解决中文显示为方框的问题
  layout(matrix(data=c(1,2),nrow=1,ncol=2),widths=c(8,1),heights=c(1,2))
  par(mar=c(0,0,3,10),oma=c(0.2,0.2,0.2,0.2),mex=0.3)
  plot(chinaMap,border="white",col=colors[getColors(temp,breaks)])
  #plot(map,border="white",col=colors[getColors2(map,data$prov,ctype)])
  points(data$long,data$lat,pch=19,col=rgb(0,0,0,0.3),cex=0.8)

  drawBackground(title,date,lang)
  drawDescription(data,temp,lang)
  drawLegend(breaks,colors)
}
