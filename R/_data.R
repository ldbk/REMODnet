#' Seabed colorbar
#'
#' @format dataframe
#'
#' Columns:
#'
#' RGB = RGB code
#' code = seabed code
#' Description = description of seabed classes
#' RGB2 = internet RGB code
#' color = color in R format
#' 
#' @source https://marinemaps.jrc.ec.europa.eu/
"seabedcolorbar"

#' Fishing intensity
#'
#' ICES files of fishing intensity and pressure mapping, extraction 15 november 2017 from the ICES website.
#' Fishing intensity is defined as the swept area ratios : the number of times the c‐square has
#' been swept (see the report for further details http://www.ices.dk/sites/pub/Publication%20Reports/Forms/DispForm.aspx?ID=32429
#' 
#' @format raster stack of 4 layers : surface and subsurface fishing intensity in 2014 and 2015
#'
#' @author Youen Vermard
#' @source http://www.ices.dk/sites/pub/Publication%20Reports/Forms/DispForm.aspx?ID=32178
"fishingintensity"

#' Fishing effort
#'
#' Fishing effort in hour and landings in tons by quarter, gear, area, ICES rectangle (and species for landings) for 2014, 2015 and 2016. 
#' Data were extracted from the JRC data dissemination facility the 17/11/2017.
#'
#' @format dataframe
#' @author Laurent Dubroca
#' @source https://stecf.jrc.ec.europa.eu/dd/effort/graphs-quarterhttp://www.ices.dk/sites/pub/Publication%20Reports/Forms/DispForm.aspx?ID=32178
#' @name fdidata
NULL

#' @rdname fdidata
"fdieff"
#' @rdname fdidata
"fdilan"

