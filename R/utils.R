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
set.var <- function(wei,decimals,prob,nam, under = 'under', over = 'over', between = '-'){
  if(length(wei)>0)
   {
      if(is.numeric(wei)){ 
   	   brks    <- prob
   	   #nclr    <- length(brks) - 1
  	   plotclr <- nam     ## http://colorbrewer2.org/
           #nclr    <- min(nclr,length(plotclr))
  	   vcol    <- plotclr[ findInterval(wei,brks) ]
  	   vsize   <- (findInterval(wei,brks)/max(findInterval(wei,brks)) + 1) 
  	   leg     <- leglabs(round(brks,decimals), under = under, over = over, between = between)
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
plot.poly <- function(data, var, decimals, maptype, cuts, col.pallete, add, cuts.type, legend.att, border = NULL, zoom = NULL, ...)
{
  box <- data@bbox
 wei <- set.wei(data,var)

 if(is.numeric(wei)){ 
   if(length(cuts) == 1) prob <- genprob(wei,cuts,cuts.type)
   else prob <- cuts
 }
 
 ch.var <- set.var(wei, decimals, prob, col.pallete, 
                   under = ifelse(is.null(legend.att$under), 'under', legend.att$under), 
                   over = ifelse(is.null(legend.att$over), 'over', legend.att$over), 
                   between = ifelse(is.null(legend.att$between), '-', legend.att$between))
 
 legend.att['between'] <- NULL
 legend.att['under'] <- NULL
 legend.att['over'] <- NULL
 
 legend.default <- list(x = "bottomright", legend = ch.var$leg, fill = ch.var$cols, cex = 1, ncol = 1, bty = "n")
 legend.default[names(legend.att)] <- NULL
 legend.list <- c(legend.att, legend.default)
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
 ## Plota o mapa no mapa do Google Maps	
 #maptype  <- 'satellite'	##'terrain', 'roadmap', 'satellite', 'hybrid'
 n_pix    <- 640
 destfile <- 'TemporaryMap.png'

 if (is.null(zoom)) {
   zoom <- min(MaxZoom(range(box[1,]), range(box[2,]))) 
 }
 
 map <- GetMap.bbox(box[1,], box[2,], format = 'png', maptype = maptype, 
                    destfile = destfile, zoom = zoom)		## png

 
 PlotOnStaticMap(map,add=add)
 PlotPolysOnStaticMap(map, SpatialPolygons2PolySet(data), lwd=.5, col = ch.var$vcol, border=border, ...)
 
 if((!add) & (length(wei) > 0)) do.call(legend, legend.list)
 invisible(file.remove(c("TemporaryMap.png","TemporaryMap.png.rda")))
}

###########################################################################################
## Plot Points
###########################################################################################
plot.points <- function(data, var, decimals, maptype, cuts, col.pallete, add, cuts.type, legend.att, zoom = NULL, ...)
{
  box <- data@bbox
  
  wei <- set.wei(data,var)

 if(is.numeric(wei)){ 
   if(length(cuts) == 1) prob <- genprob(wei,cuts,cuts.type)
   else prob <- cuts
 }
   
  ch.var <- set.var(wei, decimals, prob, col.pallete, 
                    under = ifelse(is.null(legend.att$under), 'under', legend.att$under), 
                    over = ifelse(is.null(legend.att$over), 'over', legend.att$over), 
                    between = ifelse(is.null(legend.att$between), '-', legend.att$between))
  
  legend.att['between'] <- NULL
  legend.att['under'] <- NULL
  legend.att['over'] <- NULL
  
  legend.default <- list(x = "bottomright", legend = ch.var$leg, fill = ch.var$cols, cex = 1, ncol = 1, bty = "n")
  legend.default[names(legend.att)] <- NULL
  legend.list <- c(legend.att, legend.default)
  
   ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
   ## Plota o mapa no mapa do Google Maps	
   #maptype  <- 'satellite'	##'terrain', 'roadmap', 'satellite', 'hybrid'
   n_pix    <- 640
   destfile <- 'TemporaryMap.png'

   if (is.null(zoom)) {
     zoom <- min(MaxZoom(range(box[1,]), range(box[2,]))) 
   }
   
   map <- GetMap.bbox(box[1,], box[2,], format = 'png', maptype = maptype, 
                         destfile = destfile, zoom=zoom)		## png

   #PlotOnStaticMap(map)
   PlotOnStaticMap(map,data@coords[,2],data@coords[,1], cex=ch.var$vsize, pch=20, col = ch.var$vcol, add = add, ...)
   ## Inserir Legenda
   if((!add) & (length(wei) > 0)) do.call(legend, legend.list)
   invisible(file.remove(c("TemporaryMap.png","TemporaryMap.png.rda")))
}
###########################################################################################
## Plot Pixel
###########################################################################################
plot.pixel <- function(data, var, decimals, maptype, cuts, col.pallete,add,cuts.type, legend.att, zoom = NULL, ...)
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
  
  plot.poly(spol,var,decimals,maptype, cuts, col.pallete, add, cuts.type, border=NA, legend.att, zoom, ...)
}

###########################################################################################
## Plot kernel
###########################################################################################
#' @importFrom sp SpatialPoints points2grid SpatialGridDataFrame
plot.im <- function(data, var, decimals, maptype, cuts, col.pallete,add,cuts.type, legend.att, zoom = NULL, ...)
{
  sp.point <- SpatialPoints(cbind(data$xcol, data$yrow))
  sp.grid <- points2grid(sp.point)
  
  mat <- t(data$v)
  for(i in 1:nrow(mat)) mat[i,] <- mat[i,ncol(mat):1]
  w <- as.vector(mat)
 
  sp.grid <- SpatialGridDataFrame(sp.grid, data.frame(val=w))
  spix <- as(sp.grid, "SpatialPixelsDataFrame")
  spol <- as(spix, "SpatialPolygonsDataFrame") 
  
  plot.poly(spol,names(spol@data)[1],decimals,maptype, cuts, col.pallete, add, cuts.type, border=NA, legend.att, zoom, ...)
}


###########################################################################################
## Plot Lines
###########################################################################################
#' @importFrom sp coordinates
plot.line <- function(data, var, decimals, maptype, cuts, col.pallete, add, cuts.type, lwd = 1.5, legend.att, zoom = NULL, ...)
{
  box <- data@bbox
  wei <- set.wei(data,var)
  
  if(is.numeric(wei)){ 
    if(length(cuts) == 1) prob <- genprob(wei,cuts,cuts.type)
    else prob <- cuts
  }
  
  ch.var <- set.var(wei, decimals, prob, col.pallete, 
                    under = ifelse(is.null(legend.att$under), 'under', legend.att$under), 
                    over = ifelse(is.null(legend.att$over), 'over', legend.att$over), 
                    between = ifelse(is.null(legend.att$between), '-', legend.att$between))
  
  legend.att['between'] <- NULL
  legend.att['under'] <- NULL
  legend.att['over'] <- NULL
  
  legend.default <- list(x = "bottomright", legend = ch.var$leg, fill = ch.var$cols, cex = 1, ncol = 1, bty = "n")
  legend.default[names(legend.att)] <- NULL
  legend.list <- c(legend.att, legend.default)
  
  if(is.null(var)){
    ch.var$vcol <- rep(ch.var$vcol, nrow(data))
  }
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
  ## Plota o mapa no mapa do Google Maps	
  #maptype  <- 'satellite'	##'terrain', 'roadmap', 'satellite', 'hybrid'
  n_pix    <- 640
  destfile <- 'TemporaryMap.png'
  
  if (is.null(zoom)) {
    zoom <- min(MaxZoom(range(box[1,]), range(box[2,]))) 
  }
  
  map <- GetMap.bbox(box[1,], box[2,], format = 'png', maptype = maptype, 
                     destfile = destfile, zoom = zoom)		## png
  
  
  coordLines <- coordinates(data)
  if(!add){
    PlotOnStaticMap(map, coordLines[[1]][[1]][,2], coordLines[[1]][[1]][,1], lwd = lwd, col = ch.var$vcol[1], add = F, FUN = lines, ...)
    for(i in 2:nrow(data)) {
      PlotOnStaticMap(map, coordLines[[i]][[1]][,2], coordLines[[i]][[1]][,1], lwd = lwd, col = ch.var$vcol[i], add = T, FUN = lines, ...)
    }  
  }else{
    for(i in 1:nrow(data)) {
      PlotOnStaticMap(map, coordLines[[i]][[1]][,2], coordLines[[i]][[1]][,1], lwd = lwd, col = ch.var$vcol[i], add = T, FUN = lines, ...)
    }
  }
  
  if((!add) & (length(wei) > 0)) do.call(legend, legend.list)
  invisible(file.remove(c("TemporaryMap.png","TemporaryMap.png.rda")))
}

#################
spGoogle.httpd.handler <- function(path, query, ...) {
  
  # path <- gsub("^/custom/spGoogle/", "", path)
  dir_serv <- gsub("^/custom/spGoogle/", "", dirname(path))
  path <- basename(path)
  
  f <- sprintf("%s%s%s",
               dir_serv,
               .Platform$file.sep,
               path) 
  list(file=f,
       "content-type"="text/html",
       "status code"=200L)
}

##-------------------------------------------------##

#' @title Modify a spGoogle object by adding on new components.
#' @description This operator allows you to add objects to a spGoogle object.
#' @param e1 object of the spGoogle class
#' @param e2 object of the spGoogle class
#' @param ... further arguments passed to or from other methods.
#' 
#' @exportMethod + spGoogle
#' @export
"+.spGoogle" <- function(e1, e2, ...) {
  sp2name <- deparse(substitute(e2))
  # out_plus <- 
    merge_spGoogle(e1, sp2name, ...)
  # invisible(out_plus)
}


#' @title Invisible print to spGoogle objects.
#' @description Invisibly returns the result of spGplot 
#' which is a list with components that contain the kml and legend path.
#' @param x object of the spGoogle class
#' @param ... further arguments passed to or from other methods.
#' @exportMethod print spGoogle
#' @export
print.spGoogle <- function(x, ...) {
  invisible(x)
}

#' @title Merge spGoogle objects
#' @description Merge spGoogle objects
#' @param sp1 object of the spGoogle class
#' @param spgoogleCall list of paramters
#' @param ... further arguments passed to or from other methods.
#' @importFrom tools startDynamicHelp
#' @export
merge_spGoogle <- function(sp1, spgoogleCall, ...) {
  sp2 <- eval(parse(text=spgoogleCall)) 
  output_spgoogle <- list(sp1, sp2)
  kml_path     <- list(sp1$kmlpath, sp2$kmlpath)
  kml_parse    <- lapply(kml_path, xmlTreeParse)
  kml_document <- lapply(kml_parse, function(kml) kml$doc$children$kml[[1]])
  kml_merged   <- Reduce(f = function(...) append.XMLNode(...), list(kml_parse[[1]]$doc$children$kml, kml_document[-1]))
  
  kml_merged_name <- tempfile(pattern = 'MERGED_KML_', fileext = '.kml')
  kml_path <- file.path(dirname(sp1$kmlpath), basename(kml_merged_name))
  saveXML(kml_merged, file = kml_path)
  path <- list(kmlpath = kml_path, leg.path = sp1$leg.path)
  
  html_read <- readLines(sp1$html_path)
  pos_leg <- grep('legenda', readLines(sp1$html_path))
  leg1 <- html_read[pos_leg]
  leg2 <- add_legend(leg = sp2$leg.path, tempdir = '.')
  leg_final <- paste(c(leg1, leg2), collapse = ' ', sep = '')
  html_read[pos_leg] <- leg_final
  
  pos_kml <- grep('kml', html_read)
  html_read[pos_kml] <- gsub("\\(.*\\)", replacement = sprintf("(\'%s\')", basename(kml_path)), html_read[pos_kml])
  
  path.map <- sp1$html_path
  
  html <- file(path.map, open = 'w')
  cat(html_read, sep = '\n', file = html)
  close(html)
  
  env <- get( ".httpd.handlers.env", asNamespace("tools"))
  env[["spGoogle"]] <- spGoogle.httpd.handler
  
  .url <- sprintf("http://127.0.0.1:%s/custom/spGoogle/%s",
                  ifelse(R.version['svn rev'] < 67550 | getRversion() < "3.2.0",
                         get("httpdPort", envir=environment(startDynamicHelp)),
                         tools::startDynamicHelp(NA)
                  ),
                  path.map)
  
  viewer <- getOption("viewer")
  if (!is.null(viewer))
    viewer(.url)
  else
    utils:: browseURL(.url)
  
  out <- c(path, html_path = path.map)
  class(out) <- 'spGoogle'
  invisible(out)
}

add_legend <- function(leg, tempdir) {
  if (!is.na(leg)) {
    leg <- basename(leg)
    legend_string <- paste('<img src=\"', tempdir, "/", leg, '\">', sep = "")
  } else {
    legend_string <- ''
  }
  return(legend_string)
}