PlotPolyG <- function(data,
					 var,
					 description,
					 map.name,
					 map.description,
					 decimals,
					 col.pallete,
					 legend.att,
           cuts, 
					 cuts.type,
					 savekml,
					 border = "#A5A5A5"){

output <- GeraMapa(data, var, description, map.name, map.description, decimals, col.pallete, legend.att, cuts, cuts.type, savekml = savekml)
			 
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
  out <- sapply(slot(data, "polygons"), 
		   function(x) {
             maptools::kmlPolygon(x, 
                         name   = as(data, "data.frame")[slot(x, "ID"), info.window.title], 
						 col    = as(data, "data.frame")[slot(x, "ID"), "color"],
						 lwd    = 1,
						 border = border,
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
   # dire <- getwd()
   # tempdire <- dirname(file.name.kml)
   # setwd(tempdire)
   # file.name.kmz <- gsub(".kml", ".kmz", file.name.kml)
   # invisible(zip(file.name.kmz, basename(c(file.name.kml, leg.path))))
   # unlink(basename(c(file.name.kml, leg.path)))
   # setwd(dire)
  return(list(kmlpath = file.name.kml, leg.path = ifelse(is.na(leg.path), NA, basename(leg.path))))
}
