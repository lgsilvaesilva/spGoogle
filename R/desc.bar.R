desc.bar <-
function(x,legenda.aux=legenda.aux, r=r){
  x <- round(x, r)
  m <- max(x)+3
  n <- length(x)

  pal12 <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", 
		  "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A",
		  "#FFFF99", "#B15928")
  cor.desc <- colorRampPalette(pal12)(n)
  
  
  if(n<=3){
	  #color <- paste(gsub("^(#)","",brewer.pal(3,"Paired")[1:n]),sep=",",collapse=",")
	  color <- paste(gsub("^(#)","", cor.desc),sep=",",collapse=",")
  }else{
	  #color <- paste(gsub("^(#)","",brewer.pal(n,"Paired")),sep=",",collapse=",") 
	  color <- paste(gsub("^(#)","",cor.desc),sep=",",collapse=",")
  }

  dados <- paste(x,sep=",",collapse="|")
  chm.aux <- paste("N,000000,",0:(n-1),",-1,11",sep="",collapse="|")

  ##-- Dimensionando o tamanho das colunas e do balão --##
  if(n<=5){
  	chbh   <- "20,11,31"	#"&chbh=","20,11,31",
	width  <- 300
	height <- 150  		#"height=","150 "
  }else{
	chbh <- "a"
	width  <- 300 #500
	height <- 250 #479
  }
  ##-----##

  out <- sprintf(
     paste("<img src=",
        "http://chart.apis.google.com/chart",
        "?chxr=","0,1,",#m,
        "&chxs=","0,000000,11.5,0,lt,000000|1,000000,11.5,0,lt,000000",
	"&chxt=","y",
        "&chbh=%s",
	"&chs=%sx%s",
        "&chds=a",
	"&chdl=",legenda.aux,
	"&chm=",chm.aux,
        "&cht=","bvg",
        "&chco=",color,
        "&chd=t:%s",
        " width=","%s ",
        "height=","%s ", 
        "alt ","/>", 
        collapse="",sep=""), chbh, width, height, dados, width, height)

  return(out)
  
  }
