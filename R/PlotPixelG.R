#PlotPixelG <- function(data, var, decimals, maptype, cuts, col.pallete,add,cuts.type)]
PlotPixelG <- function(data,
					 var,
					 description,
					 map.name,
					 map.description,
					 decimals,
					 col.pallete,
			                 cuts, 
					 cuts.type,
			                 savekml)
{
  box <- data@bbox

  if((class(data) == "SpatialGridDataFrame")){
       data <- as(data,"SpatialPixelsDataFrame")
       var <- names(data@data)
  }
  if((class(data) == "SpatialGrid")) data <- as(data,"SpatialPixels")
  
 
  if((class(data) == "SpatialPixelsDataFrame")) spol <- as(data, "SpatialPolygonsDataFrame") 
  else spol <- as(data, "SpatialPolygons") 

  spol@bbox <- box
  
  out <- PlotPolyG(data = spol, var = var, description = description, map.name = map.name, 
	    map.description = map.description, decimals = decimals, col.pallete = col.pallete,
	    cuts = cuts, cuts.type = cuts.type, savekml = savekml, border = "#A5A5A500")
  return(out)
#  box <- data@bbox
#  wei <- set.wei(data,var)
#
# if(is.numeric(wei)){ 
#   if(length(cuts) == 1) prob <- genprob(wei,cuts,cuts.type)
#   else prob <- cuts
# }
#
# ch.var <- set.var(wei,tval,decimals,prob,col.pallete)
#
# file.name.kml <- tempfile(pattern="map_r_", fileext=".kml")
# png(filename = "xx.png", width, height, bg="#FFFFFF00")
#	op <- par(bg="transparent", oma = c(0, 0, 0, 0), mar = rep(0, 4))
#	plot(data, lwd=.5, col = unlist(ch.var$vcol))
# dev.off()
}
