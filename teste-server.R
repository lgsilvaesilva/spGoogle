##-- Teste pacote --##
library(spGoogle)
require(mapsBR)
data(regMun)
es <- subset(regMun, UF == 'ES')
es$aux <- rnorm(nrow(es))*10

data(rio)
pts <- SpatialPointsDataFrame(coords = coordinates(rio),  data = rio@data, proj4string = CRS(proj4string(rio)) )

spGplot(data = rio, var = 'Income', maptype = 'roadmap', legend.att = list(title = 'Income')) +
  spGplot(data = pts, var = 'Urban', legend.att = list(title = 'Urban')) +
  spGplot(data = es, var = 'aux', legend.att = list(title = 'Auxiliar'))

##-----##



##-- Server --##
require(servr)
require(spGoogle)

data(rio)
spGplot(rio, var = 'Dengue', savekml = T)

pp <- tools::startDynamicHelp(NA)


spGoogle.httpd.handler <- function(path, query, ...) {
  path <- gsub("^/custom/spGoogle/", "", path)
  f <- sprintf("%s%s%s",
               tempdir(),
               .Platform$file.sep,
               path) 
  list(file=f,
       "content-type"="text/html",
       "status code"=200L)
}


env <- get( ".httpd.handlers.env", asNamespace("tools"))
env[["spGoogle"]] <- spGoogle.httpd.handler    
root.dir <- tempdir()
file <- sprinf("http://127.0.0.1:18233/custom/spGoogle/index.html")
.url <- sprintf("http://127.0.0.1:%s/custom/spGoogle/%s",
                #tools:::httpdPort,
                ifelse(R.version['svn rev'] < 67550 | getRversion() < "3.2.0",
                       get("httpdPort", envir=environment(startDynamicHelp)),
                       tools::startDynamicHelp(NA)
                ),
                basename(file))

browseURL(.url)


viewer <- getOption("viewer")
viewer(.url)