\name{spGplot}
\alias{spGplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Export R information to GoogleMaps}
\description{
 Export to GoogleMaps R spatial data information
}
\usage{
spGplot(data, var=NULL, maptype='satellite', 
	description=list(var = NULL, type = NULL, title = NULL),
	decimals = 3, cuts = 5, cuts.type = 'range',
	col.pallete = list(col=heat.colors(if(length(cuts)==1) 
        cuts else length(cuts)),alpha = 1), savekml = NULL,
        map.name='kml',
        map.description='description',
        google.maps = TRUE,
	google.earth.path = try(system('which google-earth', TRUE), 
                                       TRUE))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ a spatial data from any sp classes }
  \item{var}{ an attribute name in the spatial data }
  \item{maptype}{ GoogleMaps types: 'satellite', 'terrain', 'roadmap', 'satellite', 'hybrid'}
  \item{description}{ a list containing the descrption information in the balloon. var = attribute 
name(s) in the spatial data. type = type of plot inside the balloon. title = balloon title}
  \item{decimals}{ number of decimals in the legend }
  \item{cuts}{ number of cuts in the legend. Can also be a vector with the cuts point }
  \item{cuts.type}{ method of selecting the cuts points. Ignored if cuts = vector }
  \item{col.pallete}{ a list containing the pallete of colors to be used (col) and the 
transparency level (alpha) }
  \item{savekml}{ the name of the output KML. If name is not defined the function will
  create the file in directory temporary. Default is NULL.}
  \item{map.name}{ the name of the KML}  
  \item{map.description}{ the description of th KML}  
  \item{google.maps}{ boolean. Default TRUE. Define if the data should be exported to GoogleMaps }
  \item{google.earth.path}{ if google.maps = FALSE then GoogleEarth path must be provided }
}
\value{
  A plot window on the browser
}
\seealso{
See \code{\link{rio}} and \code{\link{meuse}}
}
\examples{#see examples in 
?rio
?meuse}
\author{Luis Gustavo Silva e Silva \email{lgsilvaesilva@gmail.com} and Marcos Prates \email{marcosop@est.ufmg.br}}
\keyword{ interface }
