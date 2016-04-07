desc.pie <-
function(x,legenda.aux=legenda.aux, r=r){
  x <- round(x, r)
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

  dados <- paste(x, collapse=",", sep=",")
  label <- paste(x, collapse="|", sep=",")
   
 out <- sprintf(
   paste("<img src=",
         "http://chart.apis.google.com/chart",
         "?chxs=0,000000,11.5",
         "&chxt=x",
         "&chs=500x150",
	 "&chds=a",
         "&cht=pc",
	 "&chco=",color,
	 "&chd=t:%s",
	 "&chdl=",legenda.aux,
	 "&chdlp=r",
         "&chdls=000000,13",
	 "&chl=%s",
         "&chma=|70,100",
         " width=","500 ",
         "height=","150 ", 
         "alt=","/>",  
         collapse="",sep=""),dados,label)
  return(out)
}
