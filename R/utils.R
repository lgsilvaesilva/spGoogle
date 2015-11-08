###########################################################################################
## Set Weights
###########################################################################################
set.wei <- function(data,var){
 if(length(var) == 0) wei <- NULL
 else {
        if(length(names(data@data) == var) > 0) wei <- data@data[,var]
        else{}
        }
 wei
}

###########################################################################################
## Set plot variables
###########################################################################################
set.var <- function(wei,decimals,prob,nam){
  if(length(wei)>0)
   {
      if(is.numeric(wei)){ 
   	   brks    <- prob
   	   #nclr    <- length(brks) - 1
  	   plotclr <- nam     ## http://colorbrewer2.org/
           #nclr    <- min(nclr,length(plotclr))
  	   vcol    <- plotclr[ findInterval(wei,brks) ]
  	   vsize   <- (findInterval(wei,brks)/max(findInterval(wei,brks)) + 1) 
  	   leg     <- leglabs(round(brks,decimals))
  	 }
  	 else{ 
  	   wei     <- as.factor(wei)
   	   #nclr    <- length(levels(wei))
  	   plotclr <- nam     ## http://colorbrewer2.org/
           #nclr    <- min(nclr,length(plotclr))
  	   #nclr    <- length(levels(wei))
  	   #if(length(nam) == 1) plotclr <- brewer.pal(nclr,nam)     ## http://colorbrewer2.org/
  	   #else plotclr <- nam     ## http://colorbrewer2.org/
           #nclr    <- min(nclr,length(plotclr))
  	   vcol    <- plotclr[ match(wei,levels(wei)) ]
  	   vsize   <- 1 #match(wei,levels(wei))
  	   leg     <- as.character(levels(wei))
  	 }
  }
  else{
  	   vcol    <- nam[1]
  	   vsize   <- 1
           leg     <- NULL
           plotclr <- vcol
  }   
  
  return(list(vcol=vcol,vsize=vsize,cols=plotclr,leg=leg))
}

###########################################################################################
## Plot Polygons
###########################################################################################
plot.poly <- function(data, var, decimals, maptype, cuts, col.pallete, add, cuts.type, border=NULL, legend.position = "bottomright")
{
 box <- data@bbox
 wei <- set.wei(data,var)

 if(is.numeric(wei)){ 
   if(length(cuts) == 1) prob <- genprob(wei,cuts,cuts.type)
   else prob <- cuts
 }

 ch.var <- set.var(wei,decimals,prob,col.pallete)
 ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
 ## Plota o mapa no mapa do Google Maps	
 #maptype  <- 'satellite'	##'terrain', 'roadmap', 'satellite', 'hybrid'
 n_pix    <- 640
 destfile <- 'TemporaryMap.png'

 zoom <- min(MaxZoom(range(box[1,]), range(box[2,])))
 map <- GetMap.bbox(box[1,], box[2,], format = 'png', maptype = maptype, 
                    destfile = destfile, zoom = zoom)		## png

 
 PlotOnStaticMap(map,add=add)
 PlotPolysOnStaticMap(map, SpatialPolygons2PolySet(data), lwd=.5, col = ch.var$vcol,border=border)
 if((!add) & (length(wei) > 0)) legend(legend.position,fill=ch.var$cols, legend=ch.var$leg, cex=1,ncol=1,bty="n")
 invisible(file.remove(c("TemporaryMap.png","TemporaryMap.png.rda")))
}

###########################################################################################
## Plot Points
###########################################################################################
plot.points <- function(data, var, decimals, maptype, cuts, col.pallete, add, cuts.type, legend.position = "bottomright")
{
  box <- data@bbox
  
  wei <- set.wei(data,var)

 if(is.numeric(wei)){ 
   if(length(cuts) == 1) prob <- genprob(wei,cuts,cuts.type)
   else prob <- cuts
 }
   
  ch.var <- set.var(wei,decimals,prob,col.pallete)

   ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
   ## Plota o mapa no mapa do Google Maps	
   #maptype  <- 'satellite'	##'terrain', 'roadmap', 'satellite', 'hybrid'
   n_pix    <- 640
   destfile <- 'TemporaryMap.png'

   zoom <- min(MaxZoom(range(box[1,]), range(box[2,])))
   map <- GetMap.bbox(box[1,], box[2,], format = 'png', maptype = maptype, 
                         destfile = destfile, zoom=zoom)		## png

   #PlotOnStaticMap(map)
   PlotOnStaticMap(map,data@coords[,2],data@coords[,1], cex=ch.var$vsize, pch=20, col = ch.var$vcol, add = add)
   ## Inserir Legenda
   if((!add) & (length(wei) > 0)) legend(legend.position,fill=ch.var$cols, legend=ch.var$leg, cex=1,ncol=1,bty="n")
   invisible(file.remove(c("TemporaryMap.png","TemporaryMap.png.rda")))
}
###########################################################################################
## Plot Pixel
###########################################################################################
plot.pixel <- function(data, var, decimals, maptype, cuts, col.pallete,add,cuts.type, legend.position = "bottomright")
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
  
  plot.poly(spol,var,decimals,maptype, cuts, col.pallete, add, cuts.type, border=NA, legend.position = legend.position)
}

###########################################################################################
## Plot kernel
###########################################################################################
plot.im <- function(data, var, decimals, maptype, cuts, col.pallete,add,cuts.type, legend.position = "bottomright")
{
  sp.point <- SpatialPoints(cbind(data$xcol, data$yrow))
  sp.grid <- points2grid(sp.point)
  
  mat <- t(data$v)
  for(i in 1:nrow(mat)) mat[i,] <- mat[i,ncol(mat):1]
  w <- as.vector(mat)
 
  sp.grid <- SpatialGridDataFrame(sp.grid, data.frame(val=w))
  spix <- as(sp.grid, "SpatialPixelsDataFrame")
  spol <- as(spix, "SpatialPolygonsDataFrame") 
  
  plot.poly(spol,names(spol@data)[1],decimals,maptype, cuts, col.pallete, add, cuts.type, border=NA, legend.position = legend.position)
}


###########################################################################################
## Plot Lines
###########################################################################################
plot.line <- function(data, var, decimals, maptype, cuts, col.pallete, add, cuts.type, lwd = 1.5, legend.position)
{
  box <- data@bbox
  wei <- set.wei(data,var)
  
  if(is.numeric(wei)){ 
    if(length(cuts) == 1) prob <- genprob(wei,cuts,cuts.type)
    else prob <- cuts
  }
  
  ch.var <- set.var(wei,decimals,prob,col.pallete)
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
  ## Plota o mapa no mapa do Google Maps	
  #maptype  <- 'satellite'	##'terrain', 'roadmap', 'satellite', 'hybrid'
  n_pix    <- 640
  destfile <- 'TemporaryMap.png'
  
  zoom <- min(MaxZoom(range(box[1,]), range(box[2,])))
  map <- GetMap.bbox(box[1,], box[2,], format = 'png', maptype = maptype, 
                     destfile = destfile, zoom = zoom)		## png
  
  
  coordLines <- coordinates(data)
  if(!add){
    PlotOnStaticMap(map, coordLines[[1]][[1]][,2], coordLines[[1]][[1]][,1], lwd = lwd, col = ch.var$vcol[1], add = F, FUN = lines)
    for(i in 2:nrow(data)) {
      PlotOnStaticMap(map, coordLines[[i]][[1]][,2], coordLines[[i]][[1]][,1], lwd = lwd, col = ch.var$vcol[i], add = T, FUN = lines)
    }  
  }else{
    for(i in 1:nrow(data)) {
      PlotOnStaticMap(map, coordLines[[i]][[1]][,2], coordLines[[i]][[1]][,1], lwd = lwd, col = ch.var$vcol[i], add = T, FUN = lines)
    }
  }
  
  if((!add) & (length(wei) > 0)) legend(legend.position,fill=ch.var$cols, legend=ch.var$leg, cex=1,ncol=1,bty="n")
  invisible(file.remove(c("TemporaryMap.png","TemporaryMap.png.rda")))
}


