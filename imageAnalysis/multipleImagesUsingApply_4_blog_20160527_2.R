# install.packages("EBImage", "ggplot2", "RCurl")  # if required. 

library(EBImage)
library(ggplot2)
library(RCurl)

# this is a link to the list of files in our folider
link <- "https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/images/imageSeq/filenameslist"
# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="filenameslist", mode="wb")
load("filenameslist")

# how many files are in this list?
length(filelist)
# 48

# this creates the first function called downloadImages
downloadImages <- function(x){
  # assemble URLs
  url <- paste0("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/images/imageSeq/", x)
  cat("Image downloaded") # so that I know something is happening. 
  img <- readImage(url)
}

# read in all the files into a list using the lapply( ) function
# lapply - applies a function to a list or a vector. 
# in this case, it applies the function readImage() to each element in the 
# object filelist
# lapply( ) syntax: name of list, name of function
images <- lapply(filelist, downloadImages)

# it takes a while to download 48 images - be patient

# we now have all 48 images in a list 
# to show us that R knows this is a list we can use the function mode( )
mode(images) # output to console says "list"

# to access a single image in the list we use double square brackets
display(images[[2]])  # display the second image in the list

# we can display the image brighter
display(images[[2]]*4, method = "raster")

# it might be good to save this locally so that it doesn't need 
# to be downloaded again
# CHECK where you want to save it first
# setwd("SomeWhereYouWantToSaveTheData")
# setwd("/Users/paulbrennan/Desktop")  # example
save(images, file = "Dros_Images")

# this can then be reloaded using the load( ) function
load("Dros_Images")

# Next step to alter the list of images a a group
# make all the images greyscale
# http://stackoverflow.com/questions/20347025/convert-rgb-image-to-one-channel-gray-image-in-r-ebimage
# create a function first...
makeGrey <- function(img){
  print("One image grey")
  channel(img, mode = "grey")
}

# then apply the function to the list of images using lapply
# lapply( ) syntax: (name of list, name of function)
images.grey <-lapply(images, makeGrey)

# we have now created a list of grey scale images. 
display(images.grey[[2]]*4, method = "raster")  # allows to check. 


# write a function to create image masks
maskCells <- function(img1){   
  # blur the image
  img1 <- gblur(img1*2, sigma = 5)
  
  # apply a Otsu threshold 
  img1.mask <- img1 > otsu(img1)  
  
  # generate an image with different values for each connected region
  # key here is the bwlabel( ) function
  img1.mask <- bwlabel(img1.mask)
  
  # show this as a coloured blobs 
  display(colorLabels(img1.mask), method = "raster")
  
  
  # the bwlabel() function 'counts' the blobs
  count <- max(bwlabel(img1.mask))
  
  # this outputs the count to us
  cat('Number of cells in this image =', count,'\n')
  
  return(img1.mask)
}

# make the image masks using the function and the lapply
images.mask <- lapply(images.grey, maskCells)
# result is a list of image masks

# write function to extract number of objects masked
countRegions <- function(img1.mask){return(max(bwlabel(img1.mask)))}

# to extract numbers from the image masks use lapply and function again
numbers <- lapply(images.mask, countRegions)
# result is  a list of 48 numbers in this case

# use the numbers to make some plots
# in order to graph the numbers we put them into a data frame
numb <- unlist(numbers)
df <- data.frame(numb)
df$type <- c("memb", "st1", "nuc", "st2")
df$layer <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5,6,6,6,6,7,7,7,7,8,8,8,8,9,9,9,9,
              10,10,10,10,11,11,11,11,12,12,12,12)

df.st1 <- subset(df, df$type == "st1")
df.st2 <- subset(df, df$type == "st2")
df.nuc <- subset(df, df$type == "nuc")

df.2 <- as.data.frame(cbind(df.nuc$layer, df.nuc$numb, df.st1$numb, df.st2$numb))
colnames(df.2) <- c("layer", "nuc", "st1", "st2")


# using the dataframe, I am making plots of the number of stained cells across the various layers
# make the plot for the numbers of nuclei
nucHist <- ggplot(df.2, 
                   aes(x=as.factor(layer), 
                       y=nuc)) +
            geom_bar(stat="identity", fill = "blue") +
            ggtitle("Nuclei per layer") +
            ylab("Number of regions") +
            xlab("Layers from top to bottom") +
            theme_bw()
# show the plot
nucHist

# make the plot for the numbers of regions stained with stain number 1
stain1Hist <- ggplot(df.2, 
                     aes(x=as.factor(layer), 
                         y=st1)) +
              geom_bar(stat="identity", fill = "green") +
              ggtitle("Stain number 1") +
              ylab("Number of regions") +
              xlab("Layers from top to bottom") +
              theme_bw()

# make the plot for the numbers of regions stained with stain number 2
stain2Hist <- ggplot(df.2, 
                     aes(x=as.factor(layer),
                         y=st2)) +
              geom_bar(stat="identity", fill = "grey") + 
              ggtitle("Stain number 2") +
              ylab("Number of regions") +
              xlab("Layers from top to bottom") + 
              theme_bw()



# from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# run code above to make multiplot function first
multiplot(stain1Hist, nucHist, stain2Hist, cols = 3)
