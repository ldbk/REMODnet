#' This function downloads seabed habitat data from EMODNET using wms service
#'
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A rasterlayer object
#' @keywords EMODnet 
#' @keywords Seabed habitat
#' @keywords wms
#' @source \url{https://marinemaps.jrc.ec.europa.eu/}
#' @examples
#'   \dontrun{
#'	img<-getseabed(-1.3,0.3,49.2,49.9)
#'	rasterVis::levelplot(img,par.settings=rasterVis::rasterTheme(region=img@data@attributes[[1]]$color))
#'   } 
#'
getseabed<-function(xmin=-1.3,xmax=0.3,ymin=49.2,ymax=49.9){

#name="EUSeaMap2016";xmin=15;xmax=20.5;ymin=30;ymax=32.5
  checkparameter<-data.frame(name=TRUE,bbox=FALSE)
  #check parameters name
  #listvar<-paste0("emodnet:",c("mean","mean_atlas_land","mean_multicolour","mean_rainbowcolour"))
  #name<-as.character(name)
  #if(name%in%listvar){
#	checkparameter$name<-TRUE
  #}else{
  #  	checkparameter$name<-FALSE
  #  	print("Variable name do not exist on EMODnet/bathymetry")
  #}
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
	#name="EUSeaMap2016";xmin=-1.5;xmax=1;ymin=49.25;ymax=49.75
	xmin0<-xmin-0.1
	xmax0<-xmax+0.1
	ymin0<-ymin-0.1
	ymax0<-ymax+0.1
        bbox<-paste(xmin0,ymin0,xmax0,ymax0,sep=",")
  	res<-0.003
  	width<-floor(1+(xmax0-xmin0)/0.003)
  	height<-floor(1+(ymax0-ymin0)/0.003)
	#width<-400
	#height<-200
	con <- paste0("https://marinemaps.jrc.ec.europa.eu/geoserver/wms/?layers=geonode:eunis_hl_coastmar_cl&bbox=", bbox,
		      "&service=WMS&format=image/png&interpolation=nearest&srs=EPSG:4326&request=GetMap&height=",height,"&width=",width,"")
	utils::download.file(con, "img.png", quiet = TRUE, mode = "wb")
	img <- raster::brick("img.png")
	#pltseabed <- plotRGB(img, main="EUNIS Seabed Habitats", axes=T)
	img@extent@xmin <- xmin0
	img@extent@ymin <- ymin0
	img@extent@xmax <- xmax0
	img@extent@ymax <- ymax0
	#sp::proj4string(img)<-sp::CRS("+proj=longlat +datum=WGS84")
	raster::crs(img)<-"+proj=longlat +datum=WGS84"

	#pltseabed <- plotRGB(img, main="EUNIS Seabed Habitats", axes=T)
	i1<-img[[1]];i2<-img[[2]];i3<-img[[3]]
	i2<-raster::calc(i2,fun=function(x){1000*x})
	i3<-raster::calc(i3,fun=function(x){1000*1000*x})
	img1<-i1+i2+i3
	test<-raster::match(img1,REMODnet::seabedcolorbar$ID)
	img1<-raster::mask(img1,test)
	#img1[!img1%in%seabedcolorbar$RGB2]<-NA
	#img1[match(img1,seabedcolorbar$RGB2,nomatch=NA)]
	img1<-raster::focal(img1,w=matrix(1,ncol=11,nrow=11),fun=function(a){raster::modal(a,na.rm=T)},NAonly=T)
	img2<-raster::ratify(img1,drop=T)
	lev<-img2@data@attributes[[1]]
	lev<-base::merge(lev,REMODnet::seabedcolorbar,by=c("ID"),all.x=T,all.y=F)
	lev<-lev[,c("ID","code","Description","color","RGB")]

	img2@data@attributes[[1]]<-lev
	
	
	
	#lev$code<-code=REMODnet::seabedcolorbar$code[REMODnet::seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID],
	#list(data.frame(ID=REMODnet::seabedcolorbar$RGB2[REMODnet::seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID],
	#			code=REMODnet::seabedcolorbar$code[REMODnet::seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID],
	#			description=REMODnet::seabedcolorbar$Description[REMODnet::seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID],
	#			color=REMODnet::seabedcolorbar$color[REMODnet::seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID]))
	#img2@data<- data.frame(ID=seabedcolorbar$RGB2[seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID],
	#			code=seabedcolorbar$code[seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID],
	#			description=seabedcolorbar$Description[seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID],
	#			color=seabedcolorbar$color[seabedcolorbar$RGB2%in%levels(img2)[[1]]$ID])
	img2<-raster::crop(img2,raster::extent(xmin,xmax,ymin,ymax))
	#rasterVis::levelplot(img2)
	#plot(img2,col=img2@data@attributes[[1]]$color)
	#rasterVis::levelplot(img2,par.settings=rasterVis::rasterTheme(region=img2@data@attributes[[1]]$color))
  	return(img2)
 }
}
