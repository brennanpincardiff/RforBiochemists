## START
# script to plot United Kingdom as per the exercise in R101, Module 5
# install.packages("sf")

library(sf)
library(ggplot2)

# identify the URL
link <- "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_GBR.gpkg"

# download the data
download.file(url=link, destfile="file.gpkg", mode="wb")
# it's a big file and takes a while to download

# check layers within the data
st_layers("file.gpkg")
# this function is from the package sf

# output
# Driver: GPKG 
# Available layers:
#   layer_name geometry_type features fields crs_name
# 1  ADM_ADM_0 Multi Polygon        1      2   WGS 84
# 2  ADM_ADM_1 Multi Polygon        5     11   WGS 84
# 3  ADM_ADM_2 Multi Polygon      183     13   WGS 84
# 4  ADM_ADM_3 Multi Polygon      406     16   WGS 84
# 5  ADM_ADM_4 Multi Polygon     9111     14   WGS 84

# this file contains a lot of data 

# open one of the layers in the file
gb <- st_read("file.gpkg", layer = "ADM_ADM_2")

# plot the map
ggplot() + geom_sf(data = gb)
# this take a while but it does work... 

# pull out the data of layer 1 with 5 features
gb_1 <- st_read("file.gpkg", layer = "ADM_ADM_1")

# plot the file
ggplot() + geom_sf(data = gb_1)

# still quite slow... gives a nice map with England, Northern Ireland, 
# Scotland and Wales

## END