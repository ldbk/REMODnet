#' This function downloads human activities point data from EMODNET using wfs service
#'
#' @param name A character vector of the shortname of the variable
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A data frame 
#' @keywords EMODnet 
#' @keywords human activities
#' @keywords wms
#' @source http://www.emodnet-humanactivities.eu
#'
gethumactpoint<-function(name="shellfish",xmin=-1.3,xmax=0.3,ymin=49.2,ymax=49.9){
#name="EUSeaMap2016";xmin=15;xmax=20.5;ymin=30;ymax=32.5
  checkparameter<-data.frame(name=FALSE,bbox=FALSE)
  #check parameters name
  listvar<-c("finfish","shellfish","dredging","hydrocarbons","aggregates",
		  "portvessels","portlocations","platforms","windfarms","dredgespoil",
		  "munitions")

  name<-as.character(name)
  if(name%in%listvar){
	checkparameter$name<-TRUE
  }else{
    	checkparameter$name<-FALSE
    	print("Variable name do not exist on EMODnet/human activities")
  }

  #check bbox format
  n1<-is.numeric(xmin)
  n2<-length(xmin)==1
  n3<-is.numeric(xmax)
  n4<-length(xmax)==1
  n5<-is.numeric(ymin)
  n6<-length(ymin)==1
  n7<-is.numeric(ymax)
  n8<-length(ymax)==1
  n9<-xmin<xmax
  n10<-ymin<ymax
  if(!(n1&n2&n3&n4&n5&n6&n7&n8)){
     checkparameter$bbox<-FALSE
     print("Spatial limits are not correct (1 numeric vector for xmin, xmax, ymin and ymax)")
  }else{
     checkparameter$bbox<-TRUE
  }
  if(!(n9&n10)){
      checkparameter$bbox<-FALSE
      print("Spatial limits are not ordered correctly (xmin<xmax AND ymin<ymax)")
  }
  #need to check if spatial extent is in line with EMODnet coverage
  
  if(apply(checkparameter,1,sum)==2){
  	#define WCS connection"
	name="shellfish";xmin=-4;xmax=4;ymin=49;ymax=55
	xmin0<-xmin-0.1
	xmax0<-xmax+0.1
	ymin0<-ymin-0.1
	ymax0<-ymax+0.1
        bbox<-paste(xmin,xmax,ymin,ymax,sep=",")
  	res<-0.003
  	width<-floor(1+(xmax0-xmin0)/0.003)
  	height<-floor(1+(ymax0-ymin0)/0.003)
	#width<-400
	#height<-200
	con<-paste0("http://77.246.172.208/geoserver/emodnet/wfs?SERVICE=WFS&VERSION=1.1.0&request=GetFeature&typeName=",
		    name,"&OUTPUTFORMAT=csv&bbox=",bbox)
	download.file(con,"dat.csv", quiet = TRUE, mode = "wb")
	pipo<-read.csv("dat.csv")
	xy<-strsplit(gsub("\\)","",gsub("POINT \\(","",pipo$the_geom)),split=" ")
	fun1<-function(a){as.numeric(a[1])}
	fun2<-function(a){as.numeric(a[2])}
	pipo$x<-sapply(xy,fun1)
	pipo$y<-sapply(xy,fun2)
	plot(pipo$x,pipo$y)
 }
}


#' test0
#'
#' @keywords internal
#'
test0<-function(){
	

	library(REMODnet)
	img<-getseabed(-1,3,49,55)
	rasterVis::levelplot(img,par.settings=rasterVis::rasterTheme(region=img@data@attributes[[1]]$color))
#'	plot(img)

}

