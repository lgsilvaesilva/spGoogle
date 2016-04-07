#' @title Interface GoogleMaps within R
#' @description  Plot spatial data with GoogleMaps background.
#'
#' @param data a spatial data from any sp classes.
#' @param var an attribute name in the spatial data.
#' @param maptype GoogleMaps types: 'satellite', 'terrain', 'roadmap', 'satellite', 'hybrid'.
#' @param decimals number of decimals in the legend.
#' @param col.pallete a list containing the pallete of colors to be used (col) and the 
#' transparency level (alpha) .
#' @param legend.att a list containing the attributes for legend. See \code{\link[graphics]{legend}}.
#' @param cuts number of cuts in the legend. Can also be a vector with the cuts point.
#' @param cuts.type method of selecting the cuts points. Ignored if cuts = vector .
#' @param lwd the line width for SpatialLines/SpatialLinesDataframe class.
#' @param zoom zoom level.
#' @param add boolean. Default FALSE, if adding information to a previous plot.
#' @param ... further arguments passed to or from other methods.
#'
#' @return A plot window on the browser.
#'
#' @seealso See \code{\link{spGplot}}.
#' 
#' @examples
#' 
#' \dontrun{
#' spRplot(rio)
#' }
#' @export spRplot
#' @import RgoogleMaps PBSmapping
spRplot <- function(data, var=NULL, maptype = 'satellite', decimals = 3, add = FALSE,
                    cuts = 5, cuts.type="range", 
                    col.pallete = list(col=heat.colors(if(length(cuts)==1) cuts else length(cuts)),alpha = 1), 
                    legend.att = NULL, 
                    zoom = NULL,
                    lwd = 2, ...)
{
   if(!(class(data) == "SpatialPolygonsDataFrame") & !(class(data) == "SpatialPolygons") 
      & !(class(data) == "SpatialPointsDataFrame") & !(class(data) == "SpatialPoints")
      & !(class(data) == "SpatialPixelsDataFrame") & !(class(data) == "SpatialPixels")
      & !(class(data) == "SpatialGridDataFrame") & !(class(data) == "SpatialGrid")
      & !(class(data) == "SpatialLinesDataFrame") & !(class(data) == "SpatialLines"))
 	stop("The data must be one of the Spatial classes\n")

   if((length(grep("+proj=longlat",data@proj4string@projargs)) == 0) & (length(grep("+proj=latlong",data@proj4string@projargs)) == 0)) stop("Data projection must be in latlong format.\n For more details look at ?CRS")

   ## treat col.pallete options
   if(length(names(col.pallete)) > 2 | length(names(col.pallete)) <= 0) stop("The col.pallete must have at least one element and no more than 2")
   
   if(length(names(col.pallete)) == 1){
     if(names(col.pallete)[1] != "col" & names(col.pallete)[1] != "alpha") stop("The col.pallete names must be \"col\" and \"alpha\"")
     else{
       if(names(col.pallete)[1] == "col") col.pallete$alpha = 1
       if(names(col.pallete)[1] == "alpha") col.pallete$col = heat.colors(if(length(cuts)==1) cuts else length(cuts))
     }
   }
   else if(names(col.pallete)[1] != "col" | names(col.pallete)[2] != "alpha") stop("The col.pallete names must be col and alpha")
   
   col.pallete <- ColAlpha(col.pallete$col, col.pallete$alpha)
   
   if(class(data) == "SpatialPolygonsDataFrame" | class(data) == "SpatialPolygons") 
        plot.poly(data,var,decimals,maptype,cuts,col.pallete,add,cuts.type, legend.att, zoom = zoom, ...)

   if(class(data) == "SpatialPointsDataFrame"  | class(data) == "SpatialPoints") 
        plot.points(data,var,decimals,maptype,cuts,col.pallete,add,cuts.type, legend.att, zoom = zoom, ...)

   if(class(data) == "SpatialLinesDataFrame"  | class(data) == "SpatialLines") 
     plot.line(data,var,decimals,maptype,cuts,col.pallete,add,cuts.type, lwd, legend.att, zoom = zoom, ...)
#   if(class(data) == "im"){
#        cat("\nPlotting kernel may take a while!\n")
#        plot.im(data,NULL,decimals,maptype,cuts,col.pallete,add,cuts.type)
#   }

   if(class(data) == "SpatialPixelsDataFrame" | class(data) == "SpatialPixels"
      | class(data) == "SpatialGridDataFrame" | class(data) == "SpatialGrid"){
        cat("\nPlotting grid may take a while!\n")
        plot.pixel(data,var,decimals,maptype,cuts,col.pallete,add,cuts.type, legend.att, zoom = zoom, ...)
   }

}  



