gera_legenda_factor <-
function(brks, cols, savekml) {

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
	
	
	width = (dim+0.4)*12
	height = (ncat+2)*12*1.4
	
	png(filename = dest.fig, width = width, height = height, bg = "#FFFFFF00", pointsize = 10)
		op <- par(bg="transparent", mar=c(0, 0, 0, 0))
		plot(1,1,col="white",axes=F,col.axis="white",xlab="",ylab="")
		legend(
				"center",
				brks,
				fill = c(cols),
				cex  = 1.2,
				bg   = "#FFFFFF")
	dev.off()
	
	return (c(dest.fig, width, height))
}
