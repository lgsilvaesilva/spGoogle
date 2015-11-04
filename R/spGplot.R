spGplot <- function(data,						# Shape contendo as informacoes.
                    var   = NULL,						# Variavel a ser visualizada
                    maptype = 'satellite',
                    description = list(var = NULL, type = NULL, title = NULL),  # Variaveis do description, e o tipo de description
                    decimals = 3,
                    col.pallete = list(col=heat.colors(if(length(cuts)==1) cuts else length(cuts)),alpha = 1),
                    cuts = 5, 
                    cuts.type = "range",
                    savekml = NULL,
                    map.name = "kml",						# Nome do mapa usado no arquivo KML
                    map.description = "description",		 		# Descricao do mapa usado no arquivo KML
                    google.maps = TRUE,
                    google.earth.path = try(system("which google-earth", TRUE), TRUE), ...){
  
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
                      cuts = cuts, cuts.type = cuts.type, savekml = savekml)
  }
  
  if (class(data) == "SpatialPolygonsDataFrame" | class(data) == "SpatialPolygons"){
    path <- PlotPolyG(data = data, var = var, description = description, map.name = map.name, 
                      map.description = map.description, decimals = decimals, col.pallete = col.pallete,
                      cuts = cuts, cuts.type = cuts.type, savekml = savekml)
  }
  
  if (class(data) == "SpatialPointsDataFrame"  | class(data) == "SpatialPoints"){
    path <- PlotPointsG(data = data, var = var, description = description, map.name = map.name, 
                        map.description = map.description, decimals = decimals, col.pallete = col.pallete,
                        cuts = cuts, cuts.type = cuts.type, savekml = savekml)
  }
  
  if(class(data) == "SpatialPixelsDataFrame" | class(data) == "SpatialPixels"
     | class(data) == "SpatialGridDataFrame" | class(data) == "SpatialGrid"){
    cat("\nPlotting grid may take a while!\n")
    path <- PlotPixelG(data = data, var = var, description = list(var = NULL, type = NULL, title = NULL), map.name = map.name, 
                       map.description = map.description, decimals = decimals, col.pallete = col.pallete,
                       cuts = cuts, cuts.type = cuts.type, savekml = savekml)
  }
  
  ##Passar centroide e zoom
  if(google.maps){
    dir <- dirname(path$kmlpath)
    file <- basename(path$kmlpath)
    leg.file <- path$leg.path
    #path.map <- genHTML(maptype, file, legpath = path$legpath, dir)
    path.map <- genHTML(maptype, file, leg.file, dir)
    if(.Platform$OS.type == "windows"){
      #USER <- strsplit(tempdir(), "\\\\")[[1]][3]
      #path.chrome <- "C:\\Users\\%s\\AppData\\Local\\Google\\Chrome\\Application\\chrome.exe"
      #path.chrome <- sprintf(path.chrome, USER)
      brw <- unlist(readRegistry("http\\shell\\open\\command", "HCR"))
      path.chrome <- brw[grep("chrome", brw)[1]]
      path.chrome <- gsub("\" -- \"%1\"", "", path.chrome)
      path.chrome <- gsub("\"", "", path.chrome)
      system2(path.chrome, args = paste("--allow-file-access-from-files", path.map), invisible = FALSE, wait=FALSE)
    } else{#platform linux or mac
      options(warn=-1)
      path.chrome  <- try(system("which google-chrome", TRUE), TRUE)
      path.chromium  <- try(system("which chromium-browser", TRUE), TRUE)
      path.firefox <- try(system("which firefox", TRUE), TRUE)
      options(warn=0)
      if(length(is.na(path.firefox)) != 0){ #se possui firefox
        system2(path.firefox, args=path.map, wait=FALSE)
      }else{
        if(length(is.na(path.chrome)) != 0){#se nao possui firefox, testa se possui chrome
          system2(path.chrome, args = paste("--allow-file-access-from-files", path.map), wait=FALSE, stderr=FALSE)  
        }else{
          if(length(is.na(path.chromium)) != 0){#se nao possui chrome, testa se possui chromium
            system2(path.chromium, args = paste("--allow-file-access-from-files", path.map), wait=FALSE, stderr=FALSE)
          }else{  #se nao possui nenhum dos dois, pedir para instalar
          stop("Install Mozilla Firefox or Google Chrome to view the map.")
        }
      }
      #browseURL(path.map)
    }
    }
  }else system2(google.earth.path, args=path[1], invisible=FALSE, wait=FALSE)
}