spGoogle
========

Development package for users R.
The spGoogle package provides a interface between R and GoogleMaps for data visualization in both directions. Import GoogleMaps background to R plots and export R data information to GoogleMaps. Currently, on Windows the spGoogle can only export to the browser Google Chrome while on Linux both Google Chrome and Firefox are accepted.

###Installation

You can install ``spGoogle`` from github using the ``devtools`` package


```r
install.packages("devtools")
devtools::install_github("lgsilvaesilva/spGoogle")
```

###Example 1

A basic example to show how to use the spGoogle package.

```r
require(spGoogle)
data(rio)

spRplot(rio) #plot in R
spGplot(rio) #plot in Google Maps ou Google Earth

spGplot(rio, var = "Dengue")
```

###Example 2

Adding some information in description balloon. After run commands below, click on polygon to see the output.


```r
library(RColorBrewer)
require(spGoogle)
data(rio)

brks <- hist(rio$Dengue, plot = FALSE)$breaks
n_cuts <- length(brks)
pallete <- brewer.pal(n_cuts-1, 'Blues')
names(rio@data)
```

```
##  [1] "GeoCode" "Name"    "Lat"     "Long"    "Pop"     "Income"  "Urban"  
##  [8] "Dengue"  "E"       "SMR"
```

```r
spGplot(rio, var = "Dengue", cuts = brks, 
        col.pallete = list(col = pallete, alpha = 0.8), 
        description = list(title = 'Name', type = 'table', var = c('Income','Urban')),
        maptype = 'roadmap')

spGplot(rio, var = "Dengue", cuts = brks, 
        col.pallete = list(col = pallete, alpha = 0.8), 
        description = list(title = 'Name', type = 'bar', var = c('Income','Urban')),
        maptype = 'roadmap')

spGplot(rio, var = "Dengue", cuts = brks, 
        col.pallete = list(col = pallete, alpha = 0.8), 
        description = list(title = 'Name', type = 'pie', var = c('Income','Urban')),
        maptype = 'roadmap')
```
