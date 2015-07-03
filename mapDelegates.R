# draw a map of the UK with R. 
# https://www.students.ncl.ac.uk/keith.newman/r/maps-in-r

library(maps)       # Provides functions that let us plot the maps
library(mapdata)    # Contains the hi-resolution points that mark out the countries.

# use the map() function which comes from the maps package
map('worldHires',
    c('UK', 'Ireland', 'Isle of Man','Isle of Wight', 'Wales:Anglesey'))
# very small map

# make it larger by limiting the x and y axis which corresponds to
# longitude and latitude
map('worldHires',
    c('UK'),
    xlim=c(-10,2), ylim=c(50,60))

# import delagate info
data <- read.csv("http://science2therapy.com/data/locations.csv")
points(data$longitude,data$latitude,col=2,pch=18)

# we can also do this in ggplot probably better...
library(ggplot2)

# use map_data() function to create a data.frame containing the map of the UK
uk<-map_data("worldHires", region = "UK")

# create the object m with the map in it. 
# we can make the map different colours
# see here for more about colours in R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
m <- ggplot() +
  geom_polygon(data=uk, 
               aes(x=long, y=lat, group=group),
               fill="gray88") +   # see link above for a list of R colours
  scale_x_continuous(limits = c(-10,3)) +
  scale_y_continuous(limits =c(49,60.9))

# add the points on the plot telling us where the longitude and latitude of various places
# note the data is coming from a different data.frame to the map
m <- m +  geom_point(data=data, 
                aes(x=longitude, y=latitude), 
                alpha=0.5, colour="red", size = 5) 

# change the theme to make the map nicer
m <- m + theme_bw()

# add a title
m + ggtitle("Where the R for Biochemists delagates came from...")
