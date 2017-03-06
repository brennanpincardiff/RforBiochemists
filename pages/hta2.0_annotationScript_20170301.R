source("http://bioconductor.org/biocLite.R")
biocLite("oligo")
biocLite("genefilter")

# "switch on" the oligo and genefilter packages, using the library command
library(oligo)
library(genefilter)

setwd('/Users/paulbrennan/Documents/MelsMicroArrayLaptop/CELFiles')

# dir() lists all the files in the working directory, this is the input for the 
# read.celfiles() command
celfiles <- read.celfiles(dir())
# doesn't seem worth saving the celfiles object 
# as this is very large. 
# I tried saving it but it was very slow. 

dat <- rma(celfiles)
save(dat, file = "MelsDataNormalised")
# this rma() function does a lot!
# Background correcting
# Normalizing
# Calculating Expression


vals <- exprs(dat)

# these must be log values in some way. 
# max values about 13.6 - this implies a log 2 transformation at some point. 

# The data is now normalized and summarized
# Technical replicates on the array are now averaged to a single probeset value
## Question: what exactly do these values represent?


# First step 
# A cluster analysis 

plot(hclust(dist(t(vals))))
# interestingly (and worryingly?) there isn't four or five separate clusters. 

library(magrittr)

vals %>%         # take the object vals
  t() %>%        # transform it so that columns are rows
  dist() %>%     # calculate distance
  hclust() %>%   # do a hierarchial cluster
  plot()         # then plot the result. 


