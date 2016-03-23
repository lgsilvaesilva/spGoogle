gera_figura_legenda <- function(brks, cols,r, num.faixas, savekml, legend.att) {
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
  
  legend.default <- list(x = 'center', legend = cats, fill = cols, cex = 1.2, bg = '#FFFFFF')
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
  
  legend.list$width <- NULL
  legend.list$height <- NULL
  
  png(filename = dest.fig, width = width.leg, height = height.leg, bg="#FFFFFF00", pointsize = 10)
  op <- par(bg="transparent", mar=c(0, 0, 0, 0))
  plot(1,1,col="white",axes=F,col.axis="white",xlab="",ylab="", type = 'n')
  do.call(legend, legend.list)
  dev.off()
  
  return (c(dest.fig, width.leg, height.leg))
  
}
