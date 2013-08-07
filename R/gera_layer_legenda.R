gera_layer_legenda <-
function(brks, cols, dest.fig, width, height, r) {
        if (is.numeric(brks)){
		brks = round(brks,r)
	}
	#brks = signif(brks, 3)
	fig.name = strsplit(dest.fig, "legenda")
	fig.name = paste("legenda", fig.name[[1]][2], sep="")
	
	width = as.numeric(width)
	height = as.numeric(height)
	width = round(width, 0)
	height = round(height, 0)
	
	# overlayXY x="0" y="0": ponto de referência da imagem é o (0,0)
	# screenXY x="a" y="b" mapeia o ponto overlayXY no ponyo da tela (a,b)
	
	legenda  = c(
			"<ScreenOverlay>",
			"<name>Legenda</name>",
			"<color>ffffffff</color>",
			"<visibility>1</visibility>",
			"<Icon>",
			"href ALTERADO ABAIXO",
			"</Icon>",
			"overlayXY ALTERADO ABAIXO",
			"screenXY ALTERADO ABAIXO",
			"size ALTERADO ABAIXO",
			"</ScreenOverlay>")
	leg.lr=c(
			"<overlayXY x=\"0\" y=\"0\" xunits=\"fraction\" yunits=\"fraction\"/>",
			paste("<screenXY x=\"", width, "\" y=\"18\" xunits=\"insetPixels\" yunits=\"pixels\"/>", sep=""),
			"<size x=\"-1\" y=\"-1\" xunits=\"pixels\" yunits=\"pixels\"/>")
	#"<screenXY x=\"0.67\" y=\"-0.1\" xunits=\"fraction\" yunits=\"fraction\"/>",
	
	legenda[8:10]<-leg.lr
	
	legenda[6] <- paste("<href>",fig.name,"</href>",sep="")
	
	return (legenda)
}
