## Funcao para geracao do mapa de Kernel
## para os dados da BHTrans
## dado: 26/01/2011
spRplot <- function(data, var=NULL, maptype = 'satellite', decimals = 3, add = FALSE,
                    cuts = 5, cuts.type="range", 
                    col.pallete = list(col=heat.colors(if(length(cuts)==1) cuts else length(cuts)),alpha = 1))
#brewer.pal(cuts,"RdBu"))
{
   #source("utils.R")
   #source("col.R")
   #require(RColorBrewer)         ##
   #require(RgoogleMaps)          ##
   #require(PBSmapping)		 ##
   #require(maptools)		 ##

   if(!(class(data) == "SpatialPolygonsDataFrame") & !(class(data) == "SpatialPolygons") 
      & !(class(data) == "SpatialPointsDataFrame") & !(class(data) == "SpatialPoints")
      #& !(class(data) == "im")
      & !(class(data) == "SpatialPixelsDataFrame") & !(class(data) == "SpatialPixels")
      & !(class(data) == "SpatialGridDataFrame") & !(class(data) == "SpatialGrid"))
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
        plot.poly(data,var,decimals,maptype,cuts,col.pallete,add,cuts.type)

   if(class(data) == "SpatialPointsDataFrame"  | class(data) == "SpatialPoints") 
        plot.points(data,var,decimals,maptype,cuts,col.pallete,add,cuts.type)

#   if(class(data) == "im"){
#        cat("\nPlotting kernel may take a while!\n")
#        plot.im(data,NULL,decimals,maptype,cuts,col.pallete,add,cuts.type)
#   }

   if(class(data) == "SpatialPixelsDataFrame" | class(data) == "SpatialPixels"
      | class(data) == "SpatialGridDataFrame" | class(data) == "SpatialGrid"){
        cat("\nPlotting grid may take a while!\n")
        plot.pixel(data,var,decimals,maptype,cuts,col.pallete,add,cuts.type)
   }

}  



