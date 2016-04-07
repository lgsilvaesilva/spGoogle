#' @title Export R information to Google Maps
#' @description  Export to GoogleMaps R spatial data information
#'
#' @param data a spatial data from any sp classes.
#' @param var an attribute name in the spatial data.
#' @param maptype GoogleMaps types: 'satellite', 'terrain', 'roadmap', 'satellite', 'hybrid'.
#' @param description a \code{\link[base]{list}} containing the description information in the balloon. var = attribute 
#' name(s) in the spatial data. type = type of plot inside the balloon: 'table', 'bar', 'pie' and 'png'. title = balloon title.
#' @param decimals number of decimals in the legend.
#' @param col.pallete a list containing the pallete of colors to be used (col) and the 
#' transparency level (alpha) .
#' @param legend.att a list containing the attributes for legend. See \code{\link[graphics]{legend}}.
#' @param cuts number of cuts in the legend. Can also be a vector with the cuts point.
#' @param cuts.type method of selecting the cuts points. Ignored if cuts = vector .
#' @param lwd the line width for SpatialLines/SpatialLinesDataframe class.
#' @param sizeBall numeric vector of the length 2 providing the min and max of the balls.
#' @param savekml the name of the output KML. If name is not defined the function will
#' create the file in directory temporary. Default is NULL.
#' @param map.name the name of the KML.
#' @param map.description the description of th KML.
#' @param google.maps boolean. Default TRUE. Define if the data should be exported to GoogleMaps.
#' @param google.earth.path  if google.maps = FALSE then GoogleEarth path must be provided
#' @param ... further arguments passed to or from other methods.
#'
#' @return A plot window on the browser.
#'
#' @seealso See \code{\link{spRplot}}.
#' 
#' @examples
#' \dontrun{
#' library(RColorBrewer)
#' library(spGoogle)
#' data(rio)
#' 
#' brks <- hist(rio$Dengue, plot = FALSE)$breaks
#' n_cuts <- length(brks)
#' pallete <- brewer.pal(n_cuts-1, 'Blues')
#' names(rio@data)
#' spGplot(rio, var = "Dengue", cuts = brks, 
#'         col.pallete = list(col = pallete, alpha = 0.8), 
#'         description = list(title = 'Name', type = 'table', var = c('Income','Urban')),
#'         maptype = 'roadmap')
#'                         
#' spGplot(rio, var = "Dengue", cuts = brks, 
#'         col.pallete = list(col = pallete, alpha = 0.8), 
#'         description = list(title = 'Name', type = 'bar', var = c('Income','Urban')),
#'         maptype = 'roadmap')
#'         
#' spGplot(rio, var = "Dengue", cuts = brks, 
#'        col.pallete = list(col = pallete, alpha = 0.8), 
#'        description = list(title = 'Name', type = 'pie', var = c('Income','Urban')),
#'        maptype = 'roadmap')
#'        
#' #custom legend
#' spGplot(rio, var = "Dengue", cuts = brks, 
#'        col.pallete = list(col = pallete, alpha = 0.8), 
#'        description = list(title = 'Name', type = 'pie', var = c('Income','Urban')),
#'        legend.att = list(title = 'Dengue', bg = 'red', cex = 0.8, ncol = 2)
#'        maptype = 'roadmap')
#' }
#'
#' @export spGplot
#' @import maptools XML
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
                    sizeBall = c(0.5, 1),
                    savekml = NULL,
                    map.name = "kml",						# Nome do mapa usado no arquivo KML
                    map.description = "description",		 		# Descricao do mapa usado no arquivo KML
                    google.maps = TRUE,
                    google.earth.path = try(system("which google-earth", ignore.stdout = T, ignore.stderr = T), silent = T),...){
 
    path <- class_test(data = data, var = var, maptype = maptype, description = description, decimals = decimals, col.pallete = col.pallete, legend.att = legend.att, cuts = cuts,  cuts.type = cuts.type, lwd = lwd, sizeBall = sizeBall, savekml = savekml, map.name = map.name, map.description = map.description, google.maps = google.maps, google.earth.path = google.earth.path, ...)
  
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