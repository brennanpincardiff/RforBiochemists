#load the package flowCore which allows the import of FCS files. 
source("http://bioconductor.org/biocLite.R")
biocLite("flowCore")
library(flowCore)

# import the data 
# this read.FCS() function imports the flow data
setwd() # add the appropriate destination for your directory.
ntl<-read.FCS("NTLA01.fcs", alter.names = TRUE)
n <- as.data.frame(exprs(ntl))  # extract expression values and put into a data frame

#use ggplot2 to draw dot plots
library(ggplot2)

#with colours indicating density
colfunc <- colorRampPalette(c("white", "lightblue", "green", "yellow", "red"))
# this colour palette can be changed to your tast 

ggplot(n, aes(x=FSC.A, y=SSC.A)) +
  ylim(0, 500000) + xlim(0,5000000) +
  stat_density2d(geom="tile", aes(fill = ..density..), contour = FALSE) +
  scale_fill_gradientn(colours=colfunc(400)) + # gives the colour plot
  geom_density2d(colour="black", bins=5) # draws the lines inside