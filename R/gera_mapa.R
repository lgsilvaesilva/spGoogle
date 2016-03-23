
GeraMapa <- function(data = NULL,						    	# Shape contendo as informações.
	 var   = NULL,							# Variável a ser visualizada
	 description = list(var = NULL, type = NULL, title = NULL),   # Variáveis do description, e o tipo de description
	 map.name = "kml",						# Nome do mapa usado no arquivo KML
	 map.description = "description",		# Descrição do mapa usado no arquivo KML
	 decimals,
	 col.pallete = list(col=heat.colors(if(length(cuts)==1) cuts else length(cuts)),alpha = 1),
	 legend.att,
	 cuts,						#Número de quebras do vetor ou vetor com as quebras.
	 cuts.type = NULL,				#Tipo de quebra(range, quantile).
	 border      = 1,				# Visualizar bordas entre os polígonos
	 savekml,
	 legend.vis  = TRUE, ...) {
	 
 shape <- data
 if (class(shape) == "SpatialPolygonsDataFrame" | class(shape) == "SpatialPolygons"){
   execute = TRUE
   ID <- sapply(shape@polygons, FUN = function(x) x@ID)
   rownames(shape@data) <- ID
 }
##-----##

if (!is.null(var)){

  if(is.numeric(shape@data[, var])){

##-- Desfinindo faixas --##
  cut.list <- CutsVector(shape@data[, var], cuts, cuts.type)
  brks <- cut.list$brks
  num.faixas <- cut.list$num.faixas
##-----##

##-- Atribuindo cores segundo as faixas --##
  plotclr <- col.pallete
  #color   <- plotclr[findInterval(shape@data[, var], brks,  rightmost.closed = TRUE, all.inside = TRUE)]
  color   <- plotclr[findInterval(shape@data[, var], brks,  rightmost.closed = TRUE, all.inside = TRUE)]
##-----##

##-- Legenda --##
  dest.fig.attrs <- gera_figura_legenda(brks, plotclr, r = decimals, num.faixas, savekml, legend.att = legend.att)
  dest.fig       <- dest.fig.attrs[1]
  fig.width      <- dest.fig.attrs[2]
  fig.height     <- dest.fig.attrs[3]
  legenda        <- gera_layer_legenda(brks, plotclr, dest.fig, fig.width, fig.height, r = decimals)
##-----##
} else {
    plotclr <- col.pallete
}

##-- Verificando se a variável é factor ou character --#
  if ( (is.factor(shape@data[, var])) || (is.character(shape@data[, var])) ){
    if (is.factor(shape@data[, var])){
      brks <- levels(shape@data[, var])
      num.faixas <- length(brks)
    }  else{
         if (is.character(shape@data[, var])){
           brks <- sort(unique(shape@data[, var]))
		   num.faixas <- length(brks)
         }
      }
##-- Legenda --##
    dest.fig.attrs <- gera_legenda_factor(brks, plotclr, savekml, legend.att)
    dest.fig       <- dest.fig.attrs[1]
    fig.width      <- dest.fig.attrs[2]
    fig.height     <- dest.fig.attrs[3]
    legenda        <- gera_layer_legenda(brks, plotclr, dest.fig, fig.width, fig.height, r = decimals)

    #color   <- plotclr[findInterval(shape@data[, var], brks,  rightmost.closed = TRUE, all.inside = TRUE)]
	color   <- plotclr[ match(shape@data[, var],levels(shape@data[, var])) ]
  }
##-----##	
} else{
  plotclr <- col.pallete
}

##-----##

  descr.default <- list(var = NULL, type = "table", title = NULL)
  descr.default[names(description)] <- description
	
  var.desc   <- descr.default$var
  type       <- descr.default$type
  title.desc <- descr.default$title
	
    if (!is.null(var.desc)){
      if (is.character(var.desc)){
	    col.desc <- which(names(shape@data) %in% var.desc)
      }
    else{
	  col.desc <- var.desc
    }
	  data.desc <- shape@data[, col.desc]
	  data.desc <- as.data.frame(data.desc)
	  filtro <- sapply(1:dim(data.desc)[2], FUN = function(x) is.factor(data.desc[,x]))
	  data.desc[, !filtro] <- round(data.desc[, !filtro], decimals)
	  colnames(data.desc) <- names(shape@data)[col.desc]
	  names.aux <- gsub("\\.", "+", names(data.desc))
	  legenda.aux <- paste(names.aux, sep = "", collapse = "|")
    }
  
  description <- "Enter information here!"
  ## Gráfico no Description
  if(!is.null(var.desc)){
    if (type == "png"){
	  if( dim(data.desc)[2] > 1) stop("There may more than one column.")
	  data.desc <- as.matrix(data.desc)
      description <- apply(data.desc, MARGIN = 1, FUN = DescriptionFig)
    }

    if (type == "pie"){
      if (sum(sapply(1:dim(data.desc)[2], FUN = function(x) is.factor(data.desc[,x]))) != 0)  stop("No factors are accepted in the bar or pie plot")
        description <- apply(data.desc, FUN = function(x) desc.pie(x,legenda.aux,r=decimals), MARGIN=1)
    }

    if (type == "bar"){
      if (sum(sapply(1:dim(data.desc)[2], FUN = function(x) is.factor(data.desc[,x]))) != 0)  stop("No factors are accepted in the bar or pie plot")
      description <- apply(data.desc, FUN = function(x) desc.bar(x,legenda.aux,r=decimals), MARGIN=1)
    }

    if (type == "table"){
      data.desc   <- apply(data.desc, MARGIN = 2, as.character)
      description <- apply(data.desc, MARGIN=1, FUN = DescriptionTable)
    }
	
	if (type == "text"){
      data.desc   <- apply(data.desc, MARGIN = 2, as.character)
      description <- data.desc
    }

  }

  shape$description <- description
  if(exists("color")){
	shape$color <- color
  } else {
	shape$color <- col.pallete[1]
  }
  
  if(exists("brks")){
	brks.out <- brks
  } else {
	brks.out <- NA
  }

  if(exists("dest.fig")){
	leg.path.out <- dest.fig
  } else {
	leg.path.out <- NA
  }

  if(exists("legenda")){
	leg.out <- legenda
  } else {
	leg.out <- NA
  }
  output <- list(data.out = shape, 
				leg = leg.out,
				leg.path = leg.path.out, 
				brks = brks.out, 
				plotclr = plotclr)
  return(output)
}

