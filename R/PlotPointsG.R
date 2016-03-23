PlotPointsG <- function(data,
					 var,
					 description,
					 map.name,
					 map.description,
					 decimals,
					 col.pallete,
					 legend.att, 
                     cuts, 
					 cuts.type,
					 savekml){

	output <- GeraMapa(data, var, description, map.name, map.description, decimals, col.pallete, legend.att, cuts, cuts.type, savekml = savekml)
	
	data      <- output$data.out
	leg       <- output$leg
	leg.path  <- output$leg.path
	brks      <- output$brks
	plotclr   <- output$plotclr

	size.ball.brks  <- seq(0.5, 1, length = cuts)	## Cria os tamanhos das bolinhas
	if (!is.null(savekml)){
	  name.ball <- tempfile(paste("ball", as.character(1:cuts), sep = ""), fileext = ".png", tmpdir = getwd())
	  #file.name.kml <- tempfile(pattern = "map", fileext = ".kml", tmpdir = getwd())
    file.name.kml <- file.path(getwd(), savekml)
	} else{
	  name.ball <- tempfile(paste("ball", as.character(1:cuts), sep = ""), fileext = ".png")
	  file.name.kml <- tempfile(pattern = "map", fileext = ".kml")
	}
	
        if (!is.null(var)){
		if (is.factor(data@data[, var])){
	          mapply(MakeBall, col = plotclr, file = name.ball, width = 100, height = 100, radius = min(size.ball.brks))	## Cria os arquivos .png das bolinhas
        	  size.ball <- size.ball.brks[data@data[, var]]
		}else{
		  mapply(MakeBall, col = plotclr, file = name.ball, width = 100, height = 100, radius = size.ball.brks)	## Cria os arquivos .png das bolinhas
		  size.ball <- size.ball.brks[findInterval(data@data[, var], brks, rightmost.closed = TRUE, all.inside = TRUE)]	## Atribui para cada valor calculado qual o tamanho da bolinha
		}
	}else{
	        mapply(MakeBall, col = plotclr, file = name.ball, width = 100, height = 100, radius = min(size.ball.brks))	## Cria os arquivos .png das bolinhas
		size.ball <- min(size.ball.brks)
	}

	if (dim(data)[1] != 1) {
		icon <- name.ball[findInterval(size.ball, size.ball.brks)]	## Atribui a bolinnha
	} else {
		# Escolhe o maior icone para apresentar no caso de um unico elemento
		icon <- name.ball[cuts]
	}
	
	icon <- basename(icon)
	
	#name <- cent.data[,3]			## Seleciona o nome da regiao
	ptDescriptions <- data$description 
	descr.default <- list(var = NULL, type = "table", title = NULL)
	descr.default[names(description)] <- description
	
    if (is.null(description$title)){
	  name.point <- ""
	}else{
	  name.point <- as.character(data@data[, description$title]) 
	}
	
	out <- maptools::kmlPoints(data, kmlfile = file.name.kml, name = name.point, description = ptDescriptions,
			       icon = icon, kmlname = "", kmldescription = "")

	## Le o arquivo kml do mapa das bolinhas
	if (dim(data)[2] != 1) {
		doc.aux <- readLines(file.name.kml)
		doc.aux[1] <- gsub("UTF-8", "latin1", doc.aux[1])
		doc <- xmlParse(doc.aux)
	if (!is.null(var)){
		legenda.node <- getNodeSet(doc, "//x:Document","x")
		legenda.xml  <- xmlParse(leg)
		legenda.xml  <- getNodeSet(legenda.xml,"//ScreenOverlay")[[1]]
		legenda.node[[1]] <- addChildren(legenda.node[[1]], legenda.xml)
	}
	  	saveXML(doc, file.name.kml)	##Salva o mapa das bolinhas com a legenda
	}
#	dire <- getwd()
#	tempdire <- dirname(file.name.kml)
#	setwd(tempdire)
#	file.name.kmz <- gsub(".kml", ".kmz", file.name.kml)
#	invisible(zip(file.name.kmz, basename(c(file.name.kml, name.ball, leg.path))))
#	unlink(basename(c(file.name.kml, name.ball, leg.path)))
#	setwd(dire)
#       return(file.name.kmz)
	if (!is.na(leg.path)){
		leg.path <- basename(leg.path)
	}
        return(list(kmlpath = file.name.kml, leg.path = leg.path))
}

MakeBall <- function(col, radius, file, width, height){
	x0 <- 0
	y0 <- 0
	r  <- radius
	t  <- seq(0, 2*pi, length.out = 360)
	x <- x0 + r*cos(t)
	y <- y0 + r*sin(t)

	x <- c(x[length(x)],x, x[length(x)])
	y <- c(y[length(y)],y, y[length(y)])

	png(filename = file, width, height, bg="#FFFFFF00")
		op <- par(bg="transparent", oma = c(0, 0, 0, 0), mar = rep(0, 4))
		plot(0,0, col="white", axes=F, col.axis = "white", xlab = "",ylab = "", xlim = c(-1, 1), ylim = c(-1, 1))
		polygon(x,y, col = col)
	dev.off()
}

LegendBubble <- function(col, radius, width, height){
	x0 <- 0
	y0 <- 0
	t  <- seq(0, 2*pi, length.out = 360)
	r <- sort(radius, decreasing = TRUE)
	x <- lapply(r, FUN = function(x) x0 + x*cos(t))
	y <- lapply(r, FUN = function(x) x0 + x*sin(t))

	x <- lapply(x, FUN = function(x) c(x[length(x)],x, x[length(x)]))
	y <- lapply(y, FUN = function(y) c(y[length(y)],y, y[length(y)]))

	for (i in 1:length(y)){
	  y[[i]] <- y[[i]] - (max(r) - r)[i]
	}
	
	png(filename = "xx.png", width, height, bg="#FFFFFF00")
		op <- par(bg="transparent", oma = c(0, 0, 0, 0), mar = rep(0, 4))
		plot(0,0, col="white", axes=F, col.axis = "white", xlab = "",ylab = "")
		mapply(polygon, x,y, col = col)
	dev.off()
}

