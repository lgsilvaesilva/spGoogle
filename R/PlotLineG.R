PlotLineG <- function(data,
                      var,
                      description,
                      map.name,
                      map.description,
                      decimals,
                      col.pallete,
                      cuts, 
                      cuts.type,
                      savekml,
                      lwd = 1.5){
  
  output <- GeraMapa(data, var, description, map.name, map.description, decimals, col.pallete, cuts, cuts.type, savekml = savekml)
  
  data <- output$data.out
  leg  <- output$leg
  leg.path  <- output$leg.path
  
  descr.default <- list(var = NULL, type = "table", title = NULL)
  descr.default[names(description)] <- description
  info.window.title <- descr.default$title
  
  if (!is.null(info.window.title)){
    data@data[, info.window.title] <- as.character(data@data[, info.window.title])
  }else{
    data@data$info.window.title <- names(data@data)[1]
  }
  
  ## Criando Mapa em KML #
  if (!is.null(savekml)){
    #file.name.kml <- tempfile(pattern="map_r_", fileext=".kml", tmpdir = getwd())
    file.name.kml <- file.path(getwd(), savekml)
  } else{
    file.name.kml <- tempfile(pattern="map_r_", fileext=".kml")
  }
  out <- sapply(slot(data, "lines"), 
                function(x) { 
                  maptools::kmlLine(x,
                                    name = as(data, "data.frame")[slot(x, "ID"), info.window.title], 
                                    col  = as(data, "data.frame")[slot(x, "ID"), "color"],
                                    lwd  = lwd, 
                                    description = as(data, "data.frame")[slot(x, "ID"), "description"]) 
                }
  )
  
  # Escrevendo o arquivo
  kmlFile <- file(file.name.kml,"w",encoding="UTF-8")
  kmlname.map <- map.name; kmldescription.map <- map.description
  cat(maptools::kmlPolygon(kmlname=kmlname.map, kmldescription=kmldescription.map)$header, file=kmlFile, sep="\n")
  if(TRUE){
    cat(leg,file=kmlFile,sep="\n")
  }
  cat(unlist(out["style",]), file=kmlFile, sep="\n")
  cat(unlist(out["content",]), file=kmlFile, sep="\n")
  cat(maptools::kmlPolygon()$footer, file=kmlFile, sep="\n")
  close(kmlFile)
  #=====================#
  return(list(kmlpath = file.name.kml, leg.path = ifelse(is.na(leg.path), NA, basename(leg.path))))
}
