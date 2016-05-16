# aim of this script is to identify and count cells....
library(EBImage)
library(RCurl)

# this is the link to the image
link <- "https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/images/Dros_c2.tif"
# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.tif", mode="wb")

# setwd("/Users/paulbrennan/Dropbox/ImageStuff/ImageAnalysis201605")

# import image of stained cells into R
c2 <- readImage("file.tif") 
# creates an object called c2

# show the image in the R graphics window
display(c2, method = "raster")  #"raster" method means within R

# display a brighter image
display(c2*4, method = "raster")
# because an image is numbers we just multiply to make it brighter

# check details by writing the name of the object
c2 # shows some information
# it's a greyscale image 

# make a brighter image by multiplying all the values by 2
c2.b <- c2*2

# gaussian blur
c2.b.blur <- gblur(c2.b, sigma = 5)
display(c2.b.blur, method = "raster")

## threshold using Otsu'smethod 
otsu(c2.b.blur) # gives a threshold value using Otsu algorithm
# value = 0.1777344
c2.b.blur.thres <- c2.b.blur > otsu(c2.b.blur) # apply this value 
display(c2.b.blur.thres, method = "raster")
# we see that the image is starkly black and white
# all pixels have been turned into either 0 or 1 - a binary image.

# generate an image with different values for each connected region
# key here is the bwlabel( ) function
c2.b.blur.thres.cnt <- bwlabel(c2.b.blur.thres)

# show this as a coloured blobs 
display(colorLabels(c2.b.blur.thres.cnt), method = "raster")

# count by giving us the max value in the bwlabel() function
nucNo <- max(bwlabel(c2.b.blur.thres))
# output this number to the Console
nucNo  # count = 41. 

# do we need to brighten the image (probably not in this case)?
c2.blur.thres <- gblur(c2, sigma = 5) > otsu(gblur(c2, sigma = 5))
display(c2.blur.thres, method = "raster")
max(bwlabel(c2.blur.thres))
# answer is 42 - so not a big difference - good staining. 

# what happens if we don't blur?
# we can calculate the Otsu threshold to the original image
otsu(c2)
c2.thres <- c2 > otsu(c2)
display(colorLabels(bwlabel(c2.thres)), method = "raster")
# doesn't look that different to the eye but...
# if we try counting....
max(bwlabel(c2.thres))
# answer is 1398 - lots of dots causing problems... 

# there are other ways to apply thresholds but that's for later
