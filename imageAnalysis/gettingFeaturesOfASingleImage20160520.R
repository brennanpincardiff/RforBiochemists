# showing how to extract features from an image with EBImage

library(EBImage)
library(ggplot2)

# load up image
# the readImage( ) function will work on URLs to import image into R
c2 <- readImage("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/images/Dros_c2.tif") 
# creates an object called c2

# show a brighter version of the image in the R graphics window
display(c2*4, method = "raster")  #"raster" method means within R

# create the "mask" of this image by blur, threshold and counting
c2.b <- gblur(c2*2, sigma = 5)  #blur
c2.t <- c2.b > otsu(c2.b) # apply otsu threshold 
c2.t.cnt <- bwlabel(c2.t) # count the 'regions'

# show this as a coloured blobs 
display(colorLabels(c2.t.cnt), method = "raster")
display(colorLabels(c2.t.cnt))  # in a browser

# next step is to extract some of the features about these objects. 
# first extract the basic features (as defined by EBImage). 
# first of these is mean intensity (fluorescence per pixel for the object. )
# this function creates a matrix with each object in a row
# these are calculated using the thresholded image and the original image
ftb <- computeFeatures.basic(c2.t.cnt, c2)

# to get a vector of the mean intensities
m.intent <- ftb[,1]
m.intent # show in Console
# there is one value for each shape...

# next category is shapes
# these a calculated from the thresholded image
fts <- computeFeatures.shape(c2.t.cnt)
area <- fts[,1]  # select first column from object called fts
perimeter <- fts[,2] # select second colum from object called fts

# put the data into a dataframe (a different kind of R object)
df <- as.data.frame(m.intent)
df$area <- area
df$signal <- df$m.intent * df$area
df$regions <- seq(1:nrow(df))

# lots of ways to plot but one of the best is ggplot2
# plot the area of each region on the mask
ggplot(df, 
       aes(x = regions, y=area)) +
  geom_bar(stat="identity")

# plot the signal of each region on the mask
ggplot(df, 
       aes(x = regions, y=signal)) +
  geom_bar(stat="identity")

# plot mean intensity
ggplot(df, 
       aes(x = regions, y = m.intent)) +
  geom_bar(stat="identity")

# dot plot of :
ggplot(df, 
       aes(x=area, y=signal)) +
  geom_point()

# plot the signal of each region on the mask 
# in a slightly nicer way
ggplot(df, 
       aes(x = regions, y=signal)) +
  geom_bar(stat="identity") +
  ggtitle("Fluorescence within each region of the image") + 
  xlab("Regions from top right to bottom left of the image") +
  ylab("Total fluorescence in the region") +
  theme_bw()

# for some help on ggplot2, this R-Cookbook site is good. 
# http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/