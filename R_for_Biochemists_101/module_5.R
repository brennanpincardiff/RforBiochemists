# Getting data into R
## ----simple_data_creation1----------------------
### Making simple vectors in R
# here are some the lengths of some proteins
protein_aa_length <- c(157, 434, 312, 500, 745, 756, 419, 317, 551, 433)

# we can make a bar chart with this vectors
barplot(protein_aa_length)

## ----simple_data_creation2----------------------
# make a vector of characters
molecule_names <- c("TNF", "TNFR1", "TRADD", "TRAF2", "IKKa", "IKKb", "NEMO", "IkBalpha", "Rel A", "p50")

# we can add names to our a bar chart...
barplot(protein_aa_length,
        names.arg = molecule_names)

## ----ggplot_barplot-----------------------------
# create a dataframe
data <- as.data.frame(molecule_names)
data$protein_aa_length <- protein_aa_length

library(ggplot2)
ggplot(data,
       aes(x = molecule_names,
             y = protein_aa_length)) +
      geom_bar(stat = "identity")


# sometimes we want text or numbers as categories or factors
molecules <- as.factor(c("proteins", "DNA", "RNA"))

# note that the objects look different in the Global Environment
# check them out

## ----sequences
our_sequence <- rep(1:10, 2)  # the second argument is times. 
our_sequence
another_seq <- seq(from = 0, to = 4, by=0.5)
another_seq

# next we'll be handling files so these might be useful...
## ----choose_file_example -------------------------------------
data <- read.csv(file.choose())
## 
# two commands in case you've got this far without using the
getwd() # get working directory
setwd() # set working directory

# now there are a few exercises for you to do...

# Break video

# Demonstrate read.csv() and make another bar
## ----read_csv, include=TRUE, echo=TRUE-----------------------------------
data <- read.csv("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/numberOfResearchersFTEUNdata_Export_20151019_214919785.csv")

View(data) # works in R-Studio
str(data) # shows the type of object - in this case a dataframe
head(data) 
tail(data) 

## ----cleanup_scientist_data----------------------------------------------
# filter the data with dplyr for just one year. 
library(dplyr)
data <- filter(data, Time.Period == 2011)
# and then arrange in descending order by Observation.Value
data <- arrange(data, desc(Observation.Value))
# rename United Kingdom, US
data$Reference.Area <- gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", data$Reference.Area)
data$Reference.Area <- gsub("United States of America", "United States", data$Reference.Area)

## ----geom_bar_scientists-------------------------------------------------
library(ggplot2)
ggplot(data[1:20,],     # just top 20 values
       aes(x = Reference.Area,
           y = Observation.Value)) +
  geom_bar(stat="identity") +          # basic bar chart
  ylab("Number of Researchers (FTE)") +
  xlab("") +
  ggtitle("Who has the most Researchers? 2011") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))  # turns the text

# Demonstrate read.table() and make a heatmap
data <- read.table("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/R_for_Biochemists_101/data/nfkb_genes_herishanu.tsv")

# basic commands for looking at the data
head(data)      # view first six rows
str(data)       # shows it's a dataframe
colnames(data)  # column names

## ----heatmap -------------------------------------
# to make a heatmap the data needs to be all numbers as a matrix
# turn the data into a matrix
data.m <- as.matrix(data)

heatmap(data.m)


# Break video

## ------------------------------------------------------------------------
## required packages
## install.packages("sf")
library(sf)
library(ggplot2)

## step 1: find and download the map data
# identify the URL
link <- "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_GBR.gpkg"

# download the data
download.file(url=link, destfile="file.gpkg", mode="wb")
# it's a big file (411.6 MB) and takes a while to download

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


## layer 3
# this file contains a lot of data
# open and read one of the layers in the file
gb_3 <- st_read("file.gpkg", layer = "ADM_ADM_3")

# this is a big file with too much detail so don't try to plot.

# simplify the level of detail st_simplify()
gb_simpl <- st_simplify(gb_3,
                        dTolerance = 1000)

# simplify reduces the size of the file
# check size of file gb_3
round(c(object.size(gb_3)) / 1024)
# [1] 74251
# which is 74 Mbytes, I think

# to the size of gb_simpl
round(c(object.size(gb_simpl)) / 1024)
# [1] 736
# which is 0.7 Mbytes, I think

# plot the file
ggplot() + geom_sf(data = gb_simpl)
# plots nicely and looks fine.


# video break

# downloading a file and reading it in...

## ----cardiff_data_download-----------------------------------------------
# original data: http://pubs.acs.org/doi/suppl/10.1021/pr5002803/suppl_file/pr5002803_si_005.xlsx
# on Github for better download
link <- "https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/R_for_Biochemists_101/data/pr5002803_si_005.xlsx"

download.file(url=link, destfile="file.xlsx", mode="wb")
library(readxl)
data <- read_excel("file.xlsx")

## ----cardiff_data_download_2---------------------------------------------
data <- read_excel("file.xlsx", skip=2)


## ----cardiff_data_plot---------------------------------------------------
# extract the numbers - that's all we can use for distance calculations
data_s <- data[,8:18]

# make a hierarchial cluster of the patients using these functions
hc <- hclust(dist(t(data_s)))
# this code transforms the data, generates a distance matrix dist() and then uses the hclust() function to do the clustering

# plot the cluster dendrogram 
plot(hc, xlab = "Patient Samples")


## ----download_spss_file ------------------------

library(foreign)

data <-read.spss("https://figshare.com/ndownloader/files/9306997", to.data.frame = TRUE)


## ----download_EBImage --------------------------
## # EBImage - a Bioconductor package for analysing images
install.packages("BiocManager")
BiocManager::install("EBImage")

## ----import_image_file---------------------------------------------------
library("EBImage")
library(ggplot2)

# the image is on Github
img <- readImage("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/images/cllCell.jpg")
# the object created is a Large Image (24 Mb in this case!)

## ----show_image----------------------------------------------------------
display(img)
# opens a window in a web browser and allows you to look at the image

## ----dimensions----------------------------------------------------------
dim(img)

## ----extract_fluorescence------------------------------------------------
# we can extract the fluorescence values and plot the data
x <- imageData(img)[, 512, 3]

# this gives the values for colour 3 for the whole x line at the value of y = 512. 
# this is a list of 1024 numbers which can be plotted
plot(x)  # simple plot with base R

## ----plot_col1-----------------------------------------------------------
# colour 1
x10_col1 <- as.data.frame(imageData(img)[ ,425:525, 1])
plot(x10_col1$V5)  # plot one of the values

## ----plot_col1_mean------------------------------------------------------
# make a mean value for the extracted values
x10_col1$mean <- rowMeans(x10_col1[1:100])
plot(x10_col1$mean) # plot the mean

## ----extract_and_plot_col3-----------------------------------------------
# repeat for colour 3
x10_col3 <- as.data.frame(imageData(img)[ ,425:525, 3])
x10_col3$mean <- rowMeans(x10_col3[1:100])

# Plot both colours using Base R
plot(x10_col3$mean, col = "blue")
lines(x10_col1$mean, col = "red")

## ----plot_with_ggplot2---------------------------------------------------
# assemble a data frame to allow us to use ggplot to plot the data..
m <- as.data.frame(x10_col1$mean)
m$col3 <- x10_col3$mean

# change the column names in the data frame
colnames(m) <- c("col1", "col3")

# make the basic ggplot object
p <- ggplot(data=m, aes(x=seq(1, length(col3)))) + 
     geom_line(aes(y = col3), colour = "blue") + 
     geom_line(aes(y = col1), colour = "red")
p  # show the plot


## ----plot_with_ggplot2_2-------------------------------------------------
# focus on just the cell
p <- ggplot(data=m, aes(x=seq(1, length(col3)))) + 
     geom_line(aes(y = col3), colour = "blue") + 
     geom_line(aes(y = col1), colour = "red") + 
     xlim(350,850) # focus on just the cell
p  # have a look
# gives Warning messages but these can be ignored.

## ----plot_with_ggplot2_3-------------------------------------------------
# add some titles and an easy theme 
p <- p +  xlab("Distance (pixels)") +   #label x-axis
          ylab("Fluorescence signal") +    #label y-axis
          ggtitle("Histogram of fluoresence of CLL cell") +  #title
          theme_bw()     # a simple theme

p   # show the plot

## ----plot_with_ggplot2_4-------------------------------------------------
# Add titles and change the theme.
# plot points instead of lines...
q <- ggplot(data=m, aes(x=seq(1, length(col3)))) + 
  geom_point(aes(y = col3), colour = "blue") + 
  geom_point(aes(y = col1), colour = "red") + 
  xlim(350,850) +  
  xlab("Distance (pixels)") +   # label x-axis
  ylab("Fluorescence signal") +    # label y-axis
  ggtitle("Histogram of fluoresence of CLL cell") +  # add a title
  theme_bw()     # a simple theme
q   # show the plot

