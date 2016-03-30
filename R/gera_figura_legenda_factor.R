gera_legenda_factor <- function(brks, cols, savekml, legend.att) {
 
	if (!is.null(savekml)){
	  dest.fig = tempfile(pattern = "legenda", fileext = ".png", tmpdir = getwd())
	} else {
	  dest.fig = tempfile(pattern = "legenda", fileext = ".png")
	}


    brks <- as.character(brks)
	ncat <- length(brks)
	#dest.fig <- tempfile(pattern = "legenda", fileext = ".png")
	dim1 <- nchar(brks[ncat])
	dim2 <- nchar("Legenda")
	dim  <- ifelse(dim1<=dim2, dim2, dim1)
	legend.default <- list(x = 'center', legend = brks, fill = cols, cex = 1.2, bg = '#FFFFFF')
	legend.default[names(legend.att)] <- NULL
	legend.list <- c(legend.att, legend.default)
	
	if (is.null(legend.list$ncol)){
	  legend.list$ncol <- 1
	}
	
	if (!is.null(legend.list$width)) 
	  width.leg <- legend.list$width
	else 
	  width.leg = (dim+0.3)*12*legend.list$ncol
	
	if (!is.null(legend.list$height)) 
	  height.leg <- legend.list$height
	else
	  height.leg = (ncat+2)*12*1.4
	
	png(filename = dest.fig, width = width.leg, height = height.leg, bg="#FFFFFF00", pointsize = 10)
		op <- par(bg="transparent", mar=c(0, 0, 0, 0))
		plot(1, 1, col = "white", axes = F, col.axis = "white", xlab = "", ylab = "")
		do.call(legend, legend.list)
	dev.off()
	
	return (c(dest.fig, width.leg, height.leg))
}
