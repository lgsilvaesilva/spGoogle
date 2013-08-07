gera_figura_legenda <- function(brks, cols,r, num.faixas, savekml) {
	brks <- round(brks,r)
	#brks = signif(brks, 3)
	if (!is.null(savekml)){
	  dest.fig = tempfile(pattern = "legenda", fileext = ".png", tmpdir = getwd())
	} else {
	  dest.fig = tempfile(pattern = "legenda", fileext = ".png")
	}
	
	if(length(brks)!=1){
		if(all(brks==brks[1])){
			.GlobalEnv$cats <- as.character(brks[length(brks)])
			cols = cols[num.faixas]
		}else{
			.GlobalEnv$cats = paste("[",brks[-(length(brks))],", ",brks[-1],")",sep="")
			cats[length(cats)] = gsub(")","]",cats[length(cats)])			
		}
		dim1 <- length(strsplit(cats,"")[[num.faixas]])
		dim2 <- length(strsplit("Legenda","")[[1]])
		dim <- ifelse(dim1<=dim2, dim2, dim1)
		ncat = length(cats)
	} else{
		.GlobalEnv$cats = as.character(brks)
		dim1 <- length(strsplit(cats,"")[[1]])
		dim2 <- length(strsplit("Legenda","")[[1]])
		dim <- ifelse(dim1<=dim2, dim2, dim1)
		ncat = length(cats)
		cols = cols[num.faixas]
	}
	
	width = (dim+0.3)*12
	height = (ncat+2)*12*1.4
	#CairoFonts(regular = "sans:style=Regular")
	png(filename = dest.fig, width = width, height = height, bg="#FFFFFF00", pointsize = 10)
		#windows.options(width=2.5, height=1.7, pointsize=12, reset = TRUE)
		op <- par(bg="transparent", mar=c(0, 0, 0, 0))
		plot(1,1,col="white",axes=F,col.axis="white",xlab="",ylab="")
		legend(
				"center",
				#title = "Legenda",
				cats,
				fill=c(cols),
				cex=1.2,
				bg="#FFFFFF")
	dev.off()
	
	return (c(dest.fig, width, height))

}
