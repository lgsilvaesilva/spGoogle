spGplot <- function(data,						# Shape contendo as informacoes.
                    var   = NULL,						# Variavel a ser visualizada
                    maptype = 'satellite',
                    description = list(var = NULL, type = NULL, title = NULL),  # Variaveis do description, e o tipo de description
                    decimals = 3,
                    col.pallete = list(col=heat.colors(if(length(cuts)==1) cuts else length(cuts)),alpha = 1),
                    legend.att = list(title = NULL, ncol = 1, bg = "#FFFFFF"),
                    cuts = 5, 
                    cuts.type = "range",
                    lwd = 1.5,
                    savekml = NULL,
                    map.name = "kml",						# Nome do mapa usado no arquivo KML
                    map.description = "description",		 		# Descricao do mapa usado no arquivo KML
                    google.maps = TRUE,
                    google.earth.path = try(system("which google-earth", ignore.stdout = T, ignore.stderr = T), silent = T),...){
 
    path <- class_test(data = data, var = var, maptype = maptype, description = description, decimals = decimals, col.pallete = col.pallete, legend.att = legend.att, cuts = cuts,  cuts.type = cuts.type, lwd = lwd, savekml = savekml, map.name = map.name, map.description = map.description, google.maps = google.maps, google.earth.path = google.earth.path, ...)
  
  if(google.maps){
    dir <- dirname(path$kmlpath)
    file <- basename(path$kmlpath)
    leg.file <- path$leg.path
    path.map <- genHTML(maptype = maptype, kml = file, leg = leg.file, tempdir = '.', dir_save = dir)

    env <- get( ".httpd.handlers.env", asNamespace("tools"))
    env[["spGoogle"]] <- spGoogle.httpd.handler
    root.dir <- dir
    .url <- sprintf("http://127.0.0.1:%s/custom/spGoogle/%s",
                    ifelse(R.version['svn rev'] < 67550 | getRversion() < "3.2.0",
                           get("httpdPort", envir=environment(startDynamicHelp)),
                           tools::startDynamicHelp(NA)
                    ),
                    path.map)
    
    viewer <- getOption("viewer")
    if (!is.null(viewer))
      viewer(.url)
    else
      utils:: browseURL(.url)
  }
    out <- c(path, html_path = path.map)
    class(out) <- 'spGoogle'
  invisible(out)
}