spGplot <- function(data,						# Shape contendo as informacoes.
                    var   = NULL,						# Variavel a ser visualizada
                    maptype = 'satellite',
                    description = list(var = NULL, type = NULL, title = NULL),  # Variaveis do description, e o tipo de description
                    decimals = 3,
                    col.pallete = list(col=heat.colors(if(length(cuts)==1) cuts else length(cuts)),alpha = 1),
                    legend.att = list(title = NULL, ncol = 1, bg = "#FFFFFF"),
                    cuts = 5, 
                    cuts.type = "range",
                    lwd = 1.5,
                    savekml = NULL,
                    map.name = "kml",						# Nome do mapa usado no arquivo KML
                    map.description = "description",		 		# Descricao do mapa usado no arquivo KML
                    google.maps = TRUE,
                    google.earth.path = try(system("which google-earth", ignore.stdout = T, ignore.stderr = T), silent = T), ...){
  
  if(!(class(data) == "SpatialPolygonsDataFrame") & !(class(data) == "SpatialPolygons") 
     & !(class(data) == "SpatialPointsDataFrame") & !(class(data) == "SpatialPoints")
     #& !(class(data) == "im")
     & !(class(data) == "SpatialPixelsDataFrame") & !(class(data) == "SpatialPixels")
     & !(class(data) == "SpatialGridDataFrame") & !(class(data) == "SpatialGrid")
     & !(class(data) == "SpatialLinesDataFrame") & !(class(data) == "SpatialLines"))
    stop("The data must be one of the Spatial classes\n")
  
  if((length(grep("+proj=longlat",data@proj4string@projargs)) == 0) & (length(grep("+proj=latlong",data@proj4string@projargs)) == 0)) stop("Data projection must be in latlong format.\n For more details look at ?CRS")
  
  if((!google.maps) & (class(google.earth.path) == "try-error")) stop("The user must set a path for google-earth") 
  # ## treat col.pallete options
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
  
  
  if (class(data) == "SpatialLinesDataFrame" | class(data) == "SpatialLines"){
    path <- PlotLineG(data = data, var = var, description = description, map.name = map.name, 
                      map.description = map.description, decimals = decimals, col.pallete = col.pallete,
                      legend.att = legend.att, cuts = cuts, cuts.type = cuts.type, savekml = savekml, lwd, ...)
  }
  
  if (class(data) == "SpatialPolygonsDataFrame" | class(data) == "SpatialPolygons"){
    path <- PlotPolyG(data = data, var = var, description = description, map.name = map.name, 
                      map.description = map.description, decimals = decimals, col.pallete = col.pallete,
                      legend.att = legend.att, cuts = cuts, cuts.type = cuts.type, savekml = savekml)
  }
  
  if (class(data) == "SpatialPointsDataFrame"  | class(data) == "SpatialPoints"){
    path <- PlotPointsG(data = data, var = var, description = description, map.name = map.name, 
                        map.description = map.description, decimals = decimals, col.pallete = col.pallete,
                        legend.att = legend.att, cuts = cuts, cuts.type = cuts.type, savekml = savekml)
  }
  
  if(class(data) == "SpatialPixelsDataFrame" | class(data) == "SpatialPixels"
     | class(data) == "SpatialGridDataFrame" | class(data) == "SpatialGrid"){
    cat("\nPlotting grid may take a while!\n")
    path <- PlotPixelG(data = data, var = var, description = list(var = NULL, type = NULL, title = NULL), map.name = map.name, 
                       map.description = map.description, decimals = decimals, col.pallete = col.pallete,
                       legend.att = legend.att, cuts = cuts, cuts.type = cuts.type, savekml = savekml)
  }
  
  ##Passar centroide e zoom
  if(google.maps){
    dir <- dirname(path$kmlpath)
    file <- basename(path$kmlpath)
    leg.file <- path$leg.path
    path.map <- genHTML(maptype = maptype, kml = file, leg = leg.file, tempdir = '.', dir_save = dir)

    env <- get( ".httpd.handlers.env", asNamespace("tools"))
    env[["spGoogle"]] <- spGoogle.httpd.handler
    root.dir <- dir
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
  }
}