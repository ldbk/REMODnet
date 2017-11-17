#' Seabed colorbar
#'
#' A colorbar used to tranforsm seabed map in seabed data.
#' See \url{https://marinemaps.jrc.ec.europa.eu}.
#' Columns:
#' RGB (RGB code), code (seabed code),
#' Description (description of seabed classes),
#' RGB2 (internet RGB code), color (color in R format).
#'
#' @format dataframe
#' @author Laurent Dubroca
#'
'seabedcolorbar'
#' Fishing intensity
#'
#' ICES files of fishing intensity and pressure mapping, extraction 15 november 2017 from the ICES website.
#' Fishing intensity is defined as the swept area ratios : the number of times the c‐square has
#' been swept : see the report for further details
#' Source \url{http://www.ices.dk/}
#' 
#' @format raster stack of 4 layers : surface and subsurface fishing intensity in 2014 and 2015
#'
#' @author Youen Vermard
#'
'fishingintensity'
#' Fishing effort
#'
#' Fishing effort in hour and landings in tons by quarter, gear, area, ICES rectangle (and species for landings) for 2014, 2015 and 2016. 
#' Data were extracted from the JRC data dissemination facility the 17/11/2017.
#' See \url{https://stecf.jrc.ec.europa.eu/dd/effort/graphs-quarter}
#'
#' @format dataframe
#' @author Laurent Dubroca
#' @name fdidata
NULL
#' @rdname fdidata
'fdieff'
#' @rdname fdidata
'fdilan'
