#' This function downloads one human activities polygon data from EMODNET using wfs service
#'
#' @param name A character vector of the shortname of the variable
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A spatial polygon data frame 
#' @keywords EMODnet 
#' @keywords human activities
#' @keywords wfs
#' @source \url{http://www.emodnet-humanactivities.eu}
#'
gethumactpoly<-function(name="natura2000",xmin=-1,xmax=1,ymin=49,ymax=50){
  checkparameter<-data.frame(name=FALSE,bbox=FALSE)
  #check parameters name
  listvar<-c("natura2000")
  name<-as.character(name)
  if(name%in%listvar){
	checkparameter$name<-TRUE
  }else{
    	checkparameter$name<-FALSE
    	print("Variable name do not exist on EMODnet/human activities")
  }
if(F){
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
}
  checkparameter$bbox<-T
  #need to check if spatial extent is in line with EMODnet coverage
  
  if(apply(checkparameter,1,sum)==2){
  	#define WCS connection"
	#name="natura2000";xmin=-1000;xmax=10000;ymin=-10000;ymax=50000
	#name="natura2000";xmin=-10;xmax=100;ymin=-100;ymax=100
        bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
        #bbox<-paste(49,-1,50,1,sep=",")
        #bbox<-paste(-1,49,1,50,sep=",")
        #bbox<-paste(ymin,xmin,ymax,xmax,sep=",")
	#http://77.246.172.208/geoserver/emodnet/wfs?SERVICE=WFS&VERSION=1.1.0&request=GetFeature&typeName=natura2000&OUTPUTFORMAT=json&maxfeatures=1
	con<-paste0("http://77.246.172.208/geoserver/emodnet/wfs?SERVICE=WFS&VERSION=1.0.0&request=GetFeature&typeName=",
		    name,"&OUTPUTFORMAT=csv&bbox=",bbox)
	utils::download.file(con,"dat.csv", quiet = TRUE, mode = "wb")
	pipo<-utils::read.csv("dat.csv")
#	library(utils)
#	library(rgdal)

#	utils::unzip("dat.zip",exdir=tempdir())
#	zipmap <- rgdal::readOGR(dsn = "cablesschematicLine.shp")#, layer = "myZIPmap" )

	if(ncol(pipo)<=1){
		return(data.frame())
	}else{
		l1<-rgeos::readWKT(pipo$the_geom[1],id=pipo$name[1])
		raster::crs(l1)<-"+proj=longlat +datum=WGS84"
		if(nrow(pipo)>1){
		for(i in 2:nrow(pipo)){
				l2<-rgeos::readWKT(pipo$the_geom[i],id=pipo$sitename[i])
				raster::crs(l2)<-"+proj=longlat +datum=WGS84"
				l1<-raster::bind(l1,l2,keepnames=T)
		}
		}
		return(l1)
	}
  }
}
