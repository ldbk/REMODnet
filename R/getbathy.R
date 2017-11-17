#' This function downloads bathymetry data from EMODNET using wcs service
#'
#' @param name A character vector of the shortname of the variable: mean, mean_atlas_land,mean_multicolour, mean_rainbowcolour
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A rasterlayer object
#' @keywords EMODnet 
#' @keywords Bathymetry
#' @keywords wcs
#' @examples
#'   \dontrun{
#'	img<-getbathy("emodnet:mean",-1,1,49,50)
#'	plot(img)
#'   } 
#'
getbathy<-function(name="emodnet:mean",xmin=15,xmax=20.5,ymin=30,ymax=32.5){
  checkparameter<-data.frame(name=FALSE,bbox=FALSE)
  #check parameters name
  listvar<-paste0("emodnet:",c("mean","mean_atlas_land","mean_multicolour","mean_rainbowcolour"))
  name<-as.character(name)
  if(name%in%listvar){
	checkparameter$name<-TRUE
  }else{
    	checkparameter$name<-FALSE
    	print("Variable name do not exist on EMODnet/bathymetry")
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
#name="emodnet:mean";xmin=-1;xmax1;ymin=49;ymax=51
        bbox<-paste(xmin,ymin,xmax,ymax,sep=",")
  	con <- paste0("http://ows.emodnet-bathymetry.eu/wcs?service=wcs&version=1.0.0&request=getcoverage&coverage=", 
	              name,"&crs=EPSG:4326&BBOX=",bbox,"&format=image/tiff&interpolation=nearest&resx=0.00208333&resy=0.00208333") 
  	#download the image file"
  	nomfich<-paste(name,"img.tiff",sep="_")
	nomfich<-tempfile(nomfich)
  	utils::download.file(con,nomfich,quiet=TRUE,mode="wb")
  	#return the corresponding raster"
  	img<-raster::raster(nomfich)
  	#img[img==0]<-NA
	#test<-raster::match(img,0)
	#img<-raster::mask(img,test)
	#img[img <0] <- 0
	negval<-unique(raster::values(img))
	negval<-negval[negval>=0]
	test<-raster::match(img,negval)
	img<-raster::mask(img,test)
	negval<-unique(raster::values(img))
	negval<-negval[negval<=stats::quantile(negval,.99,na.rm=T)]
	test<-raster::match(img,negval)
	img<-raster::mask(img,test)
	#img<-raster::focal(img,w=matrix(1,ncol=11,nrow=11),fun=function(a){raster::mean(a)},NAonly=T)

	#correction of strange value using quantile...
	#img[img >stats::quantile(img,.99)] <- 0
  	#log inverse backtransform if chl or k490 data
  	#if(length(grep("log",data_gmis$unit[idvar],ignore.case=TRUE))>0){
   	#	img<-10^img
  	#}
        names(img)<-paste(name)
  	return(img)
 }
}
