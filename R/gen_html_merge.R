genHTML_merge <- function(maptype, kml, leg, tempdir, dir_save){
  
  #local.path <- system.file("html/geo_temp.html", package="spGoogle")
  #local.path <- file.path("/run/media/marcos/OS/UConn/Research/software/spGoogle/inst/html/geo_temp.html")
  #local.path <- file.path("C:/Users/user/Dropbox/Pacote spGoogle/spGoogle/inst/html/geo_temp.html")
  #local.path <- file.path("~/Dropbox/Pacote spGoogle/spGoogle/inst/html/geo_temp.html")
  local.path <- system.file("gmaps/geo_temp.html", package = "spGoogle")
  
  kml <- basename(kml)
  max <- 99999999
  
  MAP <- 'SATELLITE'
  if(length(maptype) > 0){
    if(maptype == 'terrain') MAP <- 'TERRAIN'
    if(maptype == 'roadmap') MAP <- 'ROADMAP'
    if(maptype == 'hybrid' ) MAP <- 'HYBRID'
  }
  
  html <- readLines(local.path)
  
  html <- gsub("_maptype_",MAP,html)
  html <- gsub("_nameKML_",paste(tempdir,"/",kml,sep=""), html)
  
  legend_string <- paste(sapply(leg, add_legend), collapse = '\n')
  html <- gsub("_legendaKML_", legend_string, html)
  
  name <- as.character(as.integer(runif(1,0,max)))
  
  write(html,file=paste(dir_save,"/MAP",name,".html",sep=""))
  return(paste(dir_save,"/MAP",name,".html",sep=""))
}
