#' This function downloads seabed habitat data from EMODNET using wms service
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
#' @keywords Seabed habitat
#' @keywords wms
#' @source https://marinemaps.jrc.ec.europa.eu/
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
	download.file(con, "img.png", quiet = TRUE, mode = "wb")
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
	img1<-raster::focal(img1,w=matrix(1,nc=11,nr=11),fun=function(a){raster::modal(a,na.rm=T)},NAonly=T)
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


#' test0
#'
#' @keywords internal
#'
test0<-function(){
	

colorbar <- data.frame(RGB=c('255-255-255','67-193-111','27-88-154','108-159-54','172-59-53','54-179-175','48-38-159','60-202-34','153-70-119','198-168-46','184-39-196','138-79-198','188-116-48','34-128-149','192-54-88','160-199-48','49-88-207','180-70-170','106-40-210','169-135-64','23-169-60','196-84-38','41-129-179','191-42-126','113-202-74','59-165-119','197-117-93','80-56-152','166-65-79','146-156-33','44-63-200','57-175-202','189-201-93','124-47-201','74-159-42','193-65-51','98-53-183','55-201-165','48-200-97','146-51-164','173-135-32','160-57-94','173-51-64','195-132-71','150-34-97','156-97-50','40-159-75','202-199-39','92-179-38','152-34-139','171-158-64','41-165-141','152-75-39','76-150-197','168-30-167','55-68-166','190-68-155','66-107-206','47-179-124','194-38-158','191-96-66','103-169-21','118-43-143','186-56-109','35-143-169','169-63-42','67-41-204','93-71-202','172-28-28','187-78-71','107-59-151','48-57-193','188-133-62','167-74-189','27-179-79','142-182-35','118-187-72','62-122-163','117-28-172','214-162-61','30-202-54','67-95-185','147-189-55','81-190-192','163-139-52','233-226-11','172-248-8','27-248-160','157-4-218','178-6-126','181-175-179','191-150-177','135-151-139'),
 code=c('XXLand','A3','A3.1','A3.2','A3.3','A3.31','A3.4','A3.5','A3.6','A4','A4.1','A4.11/13','A4.12','A4.2','A4.2/3','A.26','A.26/32','A4.27','A4.3','A4.31','A4.33','A4.4','A4.5','A4.6','A5','A5.1','A5.11','A5.13','A5.14','A5.15','A5.2','A5.21','A5.23','A5.23/24','A5.24','A5.25','A5.25/26','A5.26','A5.27','A5.3','A5.31','A5.33','A5.33/34','A5.34','A5.35','A5.35/36','A5.36','A5.37','A5.378','A5.38','A5.39','A5.4','A5.41','A5.412','A5.43','A5.44','A5.45','A5.46','A5.47','A5.5','A5.51','A5.53','A5.531','A5.535','A6','A6.1','A6.11','A6.2','A6.3','A6.3/4','A6.4','A6.5','A6.51','A6.511','A6.52','A6.7','A6.71','A6.72','A6.73','A6.732','A6.74','A6.8','A6.81','A6.814','A6.82','B1','B2','B3','A1','A2','X01','X02/X03','A1/A2'), 
Description=c('XXLand','  Infralittoral rock and other hard substrata','  Atlantic and Mediterranean high energy infralittoral rock',
'  Atlantic and Mediterranean moderate energy infralittoral rock',' Atlantic and Mediterranean low energy infralittoral rock',
'  Silted kelp on low energy infralittoral rock with full salinity',' Baltic exposed infralittoral rock',
'  Baltic moderately exposed infralittoral rock',' Baltic sheltered infralittoral rock',
'  Circalittoral rock and other hard substrata',' Atlantic and Mediterranean high energy circalittoral rock',
'  Circalittoral rock - doubt between A4.11 and A4.13',' Sponge communities on deep circalittoral rock',
'  Atlantic and Mediterranean moderate energy circalittoral rock',' Atlantic and Mediterranean circalittoral rock - doubt between A4.2 and A4.3',
'  Mediterranean coralligenous communities moderately exposed to hydrodynamic action',' Mediterranean coralligenous communities - doubt between A4.26 and A4.32',
'  Faunal communities on deep moderate energy circalittoral rock',' Atlantic and Mediterranean low energy circalittoral rock',
'  Brachiopod and ascidian communities on circalittoral rock',' Faunal communities on deep low energy circalittoral rock',
'  Baltic exposed circalittoral rock',' Baltic moderately exposed circalittoral rock',
'  Baltic sheltered circalittoral rock',' Sublittoral sediment',
'  Sublittoral coarse sediment',' Infralittoral coarse sediment in low or reduced salinity',
'  Infralittoral coarse sediment',' Circalittoral coarse sediment',
'  Deep circalittoral coarse sediment',' Sublittoral sand',
'  Sublittoral sand in low or reduced salinity',' Infralittoral fine sand',
'  Infralittoral sand - doubt between A5.23 and A5.24',' Infralittoral muddy sand',
'  Circalittoral fine sand',' Circalittoral sand - doubt between A5.25 and A5.26',
'  Circalittoral muddy sand',' Deep circalittoral sand',
'  Sublittoral mud',' Sublittoral mud in low or reduced salinity',
'  Infralittoral sandy mud',' Infralittoral mud - doubt between A5.33 and A5.34',
'  Infralittoral fine mud',' Circalittoral sandy mud','Circalittoral mud - doubt between A5.35 and A5.36',
'  Circalittoral fine mud',' Deep circalittoral mud',
'  Baltic muddy bottoms of the aphotic zone',' Mediterranean communities of muddy detritic bottoms',
'  Mediterranean communities of coastal terrigenous muds',' Sublittoral mixed sediments',
'  Sublittoral mixed sediment in low or reduced salinity',' Baltic mixed sediment bottoms of the aphotic zone',
'  Infralittoral mixed sediments',' Circalittoral mixed sediments',
'  Deep circalittoral mixed sediments',' Mediterranean animal communities of coastal detritic bottoms',
'  Mediterranean communities of shelf-edge detritic bottoms',' Sublittoral macrophyte-dominated sediment',
'  Maerl beds',' Sublittoral seagrass beds',
'  Cymodocea beds',' Posidonia beds',
'  Deep-sea bed',' Deep-sea rock and artificial hard substrata',
'  Deep-sea bedrock',' Deep-sea mixed substrata',
'  Deep-sea sand','Deep-sea sand - doubt between A6.3 and A6.4',
'  Deep-sea muddy sand',' Deep-sea mud',
'  Mediterranean communities of bathyal muds',' Facies of sandy muds with Thenea muricata',
'  Communities of abyssal muds',' Raised features of the deep-sea bed',
'  Permanently submerged flanks of oceanic islands',' Seamounts, knolls and banks',
'  Oceanic ridges',' Communities of ridge axial trough - i.e. non-vent fauna',
'  Abyssal hills',' Deep-sea trenches and canyons, channels, slope failures and slumps on the continental slope',
'  Canyons, channels, slope failures and slumps on the continental slope',' Turbidites and fans',
'  Deep-sea trenches',' Coastal dunes and sandy shores',
'  Coastal shingle',' Rock cliffs, ledges and shores, including the supralittoral',
'  Littoral rock and other hard substrata',' Littoral sediment',
'  Estuaries','Saline - Brackish coastal lagoons',
'  Littoral rock and other hard substrata - Littoral sediment'))
fun<-function(a){as.numeric(a[1])+1000*as.numeric(a[2])+1000*1000*as.numeric(a[3])}
colorbar$ID<-sapply(strsplit(colorbar$RGB,split="-"),fun,simplify=T)
fun<-function(a){rgb(as.numeric(a[1]),as.numeric(a[2]),as.numeric(a[3]),max=255)}
colorbar$color<-sapply(strsplit(colorbar$RGB,split="-"),fun,simplify=T)
seabedcolorbar<-colorbar
rm(colorbar)
save(seabedcolorbar,file="seabedcolorbar.rdata")


	library(REMODnet)
	img<-getseabed(-1,3,49,55)
	rasterVis::levelplot(img,par.settings=rasterVis::rasterTheme(region=img@data@attributes[[1]]$color))
#'	plot(img)

}

