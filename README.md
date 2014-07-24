spGoogle
========

Development package for users R.
The spGoogle package provides a interface between R and GoogleMaps for data visualization in both directions. Import GoogleMaps background to R plots and export R data information to GoogleMaps. Currently, on Windows the spGoogle can only export to the browser Google Chrome while on Linux both Google Chrome and Firefox are accepted.

###Installation

You can install ``spGoogle`` from github using the ``devtools`` package

```
install.package("devtools")
devtools::install_github("lgsilvaesilva/spGoogle")
````

###Example

```coffee
require(spGoogle)
data(rio)

spRplot(rio) #plot in R
spGplot(rio) #plot in Google Maps ou Google Earth

spGplot(rio, var = "Dengue")
```
