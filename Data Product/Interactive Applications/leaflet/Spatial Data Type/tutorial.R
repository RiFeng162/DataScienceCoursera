# Introduction to Spatial Data Type
# link:https://cengel.github.io/rspatial/2_spDataTypes.nb.html

# create spatial object in sp-package
library(sp)
  # 1. create geometric objects
ln <- Line(matrix(runif(6), ncol = 2)) # create Line-object
ln
str(ln)

lns <- Lines(list(ln), ID = "a") # create Lines-object

  # 2. create spaital objects: Spatial*
sp_lns <- SpatialLines(list(lns)) # transit as Spatial*-object
str(sp_lns)

  # 3. add attributes(optional): Spatial*DataFrame
dfr <- data.frame(id = "a", use = "road", cars_per_hour = 10)
  # create df to represent the attributes
sp_lns_dfr <- SpatialLinesDataFrame(sp_lns, dfr, match.ID = "id")
  # create Spatial*DataFrame-object 
  # methods(class = "sp)
str(sp_lns_dfr)

# create spatial object in sf-package
library(sf)
  # 1. create geometric object
lnstr_sfg <- st_linestring(matrix(runif(6), ncol=2)) 
lnstr_sfg # represented in WKT format
str(lnstr_sfg)
class(lnstr_sfg) # sfg represents Simple Feature Geometry

  # 2. combine all individual single feature object
lnstr_sfc <- st_sfc(lnstr_sfg)
lnstr_sfc # sfc represents Simple Feature Collection
class(lnstr_sfc)

  # 3. add attributes
lnstr_sf <- st_sf(dfr , lnstr_sfc)
class(lnstr_sf)





