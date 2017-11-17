#' This function downloads one human activities point data from EMODNET using wfs service
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
#' @keywords wfs
#' @source \url{http://www.emodnet-humanactivities.eu}
#'
gethumactpoint<-function(name="shellfish",xmin=-1000,xmax=1000,ymin=-1000,ymax=1000){
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
	#name="shellfish";xmin=-4;xmax=4;ymin=49;ymax=55
        bbox<-paste(xmin,xmax,ymin,ymax,sep=",")
	con<-paste0("http://77.246.172.208/geoserver/emodnet/wfs?SERVICE=WFS&VERSION=1.1.0&request=GetFeature&typeName=",
		    name,"&OUTPUTFORMAT=csv&bbox=",bbox)
	utils::download.file(con,"dat.csv", quiet = TRUE, mode = "wb")
	pipo<-utils::read.csv("dat.csv")
	if(ncol(pipo)<=1){
		return(data.frame())
	}else{
		xy<-strsplit(gsub("\\)","",gsub("POINT \\(","",pipo$the_geom)),split=" ")
		fun1<-function(a){as.numeric(a[1])}
		fun2<-function(a){as.numeric(a[2])}
		pipo$y<-sapply(xy,fun1)
		pipo$x<-sapply(xy,fun2)
		return(pipo)
	}
 }
}
#' This function downloads all the human activities point data from EMODNET using wfs service
#'
#' @param xmin A numeric vector of the lower longitude of the spatial area
#' @param xmax A numeric vector of the max longitude of the spatial area
#' @param ymin A numeric vector of the lower latitude of the spatial area
#' @param ymax A numeric vector of the max latitude of the spatial area
#' 
#' @export
#' @return A data frame
#' @keywords EMODnet 
#' @keywords human activities
#' @keywords wfs
#' @source \url{http://www.emodnet-humanactivities.eu}
#' @examples
#' \dontrun{	
#'	library(REMODnet)
#'	library(mapdata)
#'	library(ggplot2)
#'	ha<-gethumactpointall(-1,1,49,51)
#'	ggplot(ha,aes(x=x,y=y,color=type))+
#'	borders("worldHires",xlim=range(ha$x),ylim=range(ha$y),fill="light grey",colour="light grey")+
#'	geom_point()+
#'	coord_quickmap(xlim=range(ha$x),ylim=range(ha$y))
#' }
gethumactpointall<-function(xmin=-1.3,xmax=0.3,ymin=49.2,ymax=49.9){
	ha<-data.frame()
	#finfish
	rez0<-gethumactpoint("finfish",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="finfish",
			 status=rez0$status,
			 purpose=rez0$purpose,
			 info=rez0$species_harmonised,
			 name=rez0$owner_name,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#shellfish
	rez0<-gethumactpoint("shellfish",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="shellfish",
			 status=rez0$site_typology,
			 purpose="",
			 info=rez0$species,
			 name=rez0$site_name,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#dredging
	rez0<-gethumactpoint("dredging",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="dredging",
			 status=rez0$purpose,
			 purpose=rez0$extraction_type,
			 info=rez0$extraction_area,
			 name=rez0$areaid,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#hydrocarbons
	rez0<-gethumactpoint("hydrocarbons",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="hydrocarbons",
			 status=rez0$status,
			 purpose=rez0$purpose,
			 info=rez0$company,
			 name=rez0$name,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#aggregates
	rez0<-gethumactpoint("aggregates",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="aggregates",
			 status=rez0$notes,
			 purpose=rez0$purpose,
			 info=rez0$extraction_type,
			 name=rez0$extraction_area,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#platforms
	rez0<-gethumactpoint("platforms",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="platforms",
			 status=rez0$class,
			 purpose=rez0$primary_production,
			 info=rez0$category,
			 name=rez0$name,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#windfarms
	rez0<-gethumactpoint("windfarms",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="windfarms",
			 status=rez0$status,
			 purpose=rez0$power_mw,
			 info="",
			 name=rez0$name,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#dredgespoil
	rez0<-gethumactpoint("dredgespoil",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="dredgespoil",
			 status="",
			 purpose="",
			 info=rez0$oscom_code,
			 name=rez0$name,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#munitions
	rez0<-gethumactpoint("munitions",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="munitions",
			 status="",
			 purpose="",
			 info=rez0$munition_type,
			 name="",
			 country="",
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#portlocations
	rez0<-gethumactpoint("portlocations",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="portlocations",
			 status="",
			 purpose="",
			 info="",
			 name=rez0$port_id,
			 country="",
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	#portvessels
	rez0<-gethumactpoint("portvessels",ymin,xmin,ymax,xmax)
	if(nrow(rez0)>0){
	 ha1<-data.frame(type="portvessels",
			 status="",
			 purpose=rez0$vesseltype,
			 info=rez0$port_id,
			 name=rez0$portname,
			 country=rez0$country,
			 x=rez0$x,y=rez0$y)
	 ha<-rbind(ha,ha1);rm(rez0)
	}
	return(ha)
}
