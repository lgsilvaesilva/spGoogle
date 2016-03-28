#' Dengue Fever in the State of Rio de Janeiro
#' @description Shape file of the state of Rio de Janeiro/Brazilian
#' 
#' @format  A shape file with 92 observations on the following 9 variables.
#'  \describe{
#'  \item{GeoCode}{Rio de Janeiro counties numeric codes}
#'  \item{Name}{Rio de Janeiro counties names}
#'  \item{Lat}{Rio de Janeiro counties latitude}
#'  \item{Long}{Rio de Janeiro counties longitude}
#'  \item{Pop}{Rio de Janeiro counties population}
#'  \item{Income}{Rio de Janeiro counties percentage of homes living with less than one minimun salary}
#'  \item{Urban}{Rio de Janeiro counties percentage of urban area}
#'  \item{Dengue}{Counts of dengue fever cases by county in the state of Rio de Janeiro}
#'  \item{E}{Expected counts of dengue fever cases by county in the state of Rio de Janeiro, conditioned on the total
#'  number of cases the stratified by population rate}
#'  \item{SMR}{The standard mortality ratio, calculated as the observed/expected counts}
#'  }
#' @docType data
#' @keywords datasets
#' @name rio
#' @usage data(rio)
#' @examples 
#'  \dontrun{
#'  ##Using spRplot
#'  #plot the shapefile
#'  spRplot(rio, col.pallete=list(col='green',alpha=0.5))
#'  #plot the characteristcs of data
#'  spRplot(rio,'SMR',cuts=c(0,.5,1,2,5,10),col.pallete=list(alpha=0.75))
#'  spRplot(rio,'Income',cuts=c(0,.2,.4,.6,.8,1))
#'  spRplot(rio,'Urban',cuts=c(0,.2,.4,.6,.8,1))
#'  ##Using spGplot
#'  #spGplot(rio, 
#'  var = 'SMR', 
#'  description = list(var = c('Income', 'Urban'), type = 'bar', title = 'Name'), 
#'  cuts=c(0,.5,1,2,5,10), 
#'  col.pallete = list(alpha=0.8))
#'  }
NULL

#' River Meuse outline
#' 
#' The meuse.riv data consists of an outline of the Meuse
#' river in the area a few kilometers around the \link{meuse} data set.
#' 
#' @format This data frame polygon of the Meuse river in a latitude longitude format.
#' 
#' @source See the \code{\link[base]{meuse.riv}} for more documentation.
#' 
#' @docType data
#' @keywords datasets
#' @name meuse.riv
#' @usage data(meuse.riv)
#' 
#' @examples
#' \dontrun{
#' data(meuse.riv)
#' spRplot(meuse.riv,col.pallete=list(col='blue'))
#' }
NULL

#' Meuse river data set
#' @description This data set gives locations and topsoil heavy metal concentrations, 
#' along with a number of soil and landscape variables at the observation locations, 
#' collected in a flood plain of the river Meuse, near the village of Stein (NL). 
#' Data were extracted from the sp package and converted to latitude and longitude format.
#' 
#' @format This data frame contains the following columns:
#' \describe{
#' \item{x}{a numeric vector; Easting (m) in Rijksdriehoek (RDH)
#' (Netherlands topographical) map coordinates }
#' \item{y}{a numeric vector; Northing (m) in RDH  coordinates }
#' \item{cadmium}{topsoil cadmium concentration, mg kg-1 soil ('ppm'); zero cadmium
#' values in the original data set have been shifted to 0.2 (half the lowest non-zero value) }
#' \item{copper}{topsoil copper concentration, mg kg-1 soil  ('ppm') }
#' \item{lead}{topsoil lead concentration, mg kg-1 soil  ('ppm') }
#' \item{zinc}{topsoil zinc concentration, mg kg-1 soil  ('ppm') } 
#' \item{elev}{relative elevation above local river bed, m }
#' \item{dist}{distance to the Meuse; obtained from the nearest cell in
#' \link{meuse.grid}, which in turn was derived by a spread (spatial distance)
#' GIS operation, horizontal precision 20 metres; then normalized to $[0,1]$ }
#' \item{om}{organic matter, kg (100 kg)-1 soil (percent)}
#' \item{ffreq}{flooding frequency class: 1 = once in two years; 2 = once in ten years; 3 = one in 50 years}
#' \item{soil}{soil type according to the 1:50 000 soil map of the Netherlands. 1 = Rd10A (Calcareous weakly-developed meadow soils, light sandy clay); 2 = Rd90C/VII (Non-calcareous weakly-developed meadow soils, heavy sandy clay to light clay); 3 = Bkd26/VII (Red Brick soil, fine-sandy, silty light clay)}
#' \item{lime}{lime class: 0 = absent, 1 = present by field test with 5\% HCl}
#' \item{landuse}{landuse class: Aa  Agriculture/unspecified = , Ab = Agr/sugar beetsm, Ag = Agr/small grains, Ah = Agr/??, Am = Agr/maize, B = woods, Bw  = trees in pasture, DEN = ??, Fh = tall fruit trees, Fl = low fruit trees; Fw = fruit trees in pasture, Ga = home gardens, SPO = sport field, STA = stable yard, Tv = ??, W = pasture}
#' \item{dist.m}{distance to river Meuse in metres, as obtained during the field survey}
#' }
#' 
#' @usage data(meuse)
#' @docType data
#' @keywords datasets
#' @name meuse
#' @note row.names refer to the original sample number.
#' Soil units were mapped with a minimum delination width of 150 m, and so somewhat generalize the landscape.
#' Approximate equivalent World Reference Base 2002 for Soil Resources names are: Rd10A Gleyic Fluvisols; Rd90C Haplic Fluvisols; Bkd26 Haplic Luvisols. Units Rd90C and Bkd26 have winter groundwater > 80cm, summer > 120cm depth.
#' 
#' @references M G J Rikken and R P G Van Rijn, 1993. Soil pollution with heavy metals - an inquiry into spatial variation, cost of mapping and the risk evaluation of copper, cadmium, lead and zinc in the floodplains of the Meuse west of Stein, the Netherlands. Doctoraalveldwerkverslag, Dept. of Physical Geography, Utrecht University
#' 
#' P.A. Burrough, R.A. McDonnell, 1998. Principles of Geographical Information Systems. Oxford University Press.
#' 
#' Stichting voor Bodemkartering (STIBOKA), 1970. Bodemkaart van Nederland : Blad 59 Peer, Blad 60 West en 60 Oost Sittard: schaal 1 : 50 000. Wageningen, STIBOKA.
#' 
#' \url{http:/www.gstat.org/}
#' 
#' @author Field data were collected by Ruud van Rijn and Mathieu Rikken; compiled for R by Edzer Pebesma; description  extended by David Rossiter 
#' 
#' @examples 
#' \dontrun{
#' data(meuse)
#' spRplot(meuse, 'zinc')
#' meuse@data$title <- rep('Meuse Data', nrow(meuse@data))
#' spGplot(data = meuse, var = 'zinc', 
#' description = list(var=c('cadmium','copper', 'lead'), type = 'table', title = 'title'))
#' 
#' spGplot(data = meuse, var = 'zinc', 
#' description = list(var=c('cadmium','copper', 'lead'), type = 'bar', title = 'title'))
#' 
#' spGplot(data = meuse, var = 'zinc', 
#' description = list(var=c('cadmium','copper', 'lead'), type = 'pie', title = 'title'))
#' }
NULL

#' Prediction Grid for Meuse Data Set 
#'
#' The meuse.grid has a grid with 0.0002 degrees x 0.0002 degrees 
#' spacing that covers the Meuse study area (see \link{meuse}) in a latitude longitude format . 
#' 
#' @format  This data frame contains the following columns:
#' \describe{
#' \item{x}{a numeric vector; latitude coordinate (see \code{\link{meuse})} }
#' \item{y}{a numeric vector; longitude coordinate (see \code{\link{meuse})}}
#' }
#'  
#'  @details \code{x} and \code{y} are in latitude and longitude format. 
#'  Using +datum=WGS84.
#'  
#'  @source \url{http://www.gstat.org/}
#'  @references See the \code{\link[base]{meuse}} documentation.
#'  
#' @usage data(meuse.grid)
#' 
#' @docType data
#' @keywords datasets
#' @name meuse.grid
#' @examples 
#'\dontrun{
#'  data(meuse.grid)
#'  spRplot(meuse.grid, col.pallete=list(col='lightgrey',alpha=0.5),
#'  maptype = 'hybrid')
#'}
NULL
