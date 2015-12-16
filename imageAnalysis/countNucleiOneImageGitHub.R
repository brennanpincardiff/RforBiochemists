# to install use this:
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")

library(EBImage)  # you might need to install - see above

# the image is on Github 
# it is from a set of cells that are stained to detect the nuclei
# this is the link to the data

link <- "https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/seq/seq_z015_c003.tif"

# the download.file() function downloads and saves the file
download.file(url=link, destfile="file.tif", mode="wb")

# EBImage uses the readImage() function to load the file. 
img1 <- readImage("file.tif")

display(img1, method = "raster")  # shows the image within R. 

display(img1*4, method = "raster") # multiply the image to make brighter 


# I have written a function to count nuclei
# includes blurring the image, applying a threshold and counting....
# it displays the image as it is changed. 
# it's not perfect and overestimates the number of nuclei. 
# it's an example that can be done. 
# improving and customizing the various options is very feasible. 

countNuclei <- function(img1){   
  # blur the image
  w = makeBrush(size = 11, shape = 'gaussian', sigma = 5)  # makes the blurring brush
  img_flo = filter2(img1*2, w) # apply the blurring filter
  display(img_flo * 4, method = "raster") # display the blurred image - brighter for display only. 
  
  # apply a threshold 
  nmaskt = thresh(img_flo *2, w=10, h=10, offset=0.05) 
  display(nmaskt, method = "raster")
  
  # the bwlabel() function 'counts' the blobs
  nucNo <- max(bwlabel(nmaskt))
  
  # this outputs the count to us
  cat('Number of nuclei in this image =', max(bwlabel(nmaskt)),'\n')
  return(nucNo)
}

# this applies the function to the image
nucNo <- countNuclei(img1)