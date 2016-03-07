# download the data from github
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/microArrayData.tsv")
data <- read.table(text = x, header = TRUE, sep = "\t")

# basic commands for looking at the data
head(data)      # view first six rows
str(data)       # shows it's a dataframe
colnames(data)  # column names
View(data)      # this works in R-Studio
 
# for first part of analysis
# subset of the data with just the numbers.
new.data <- data[2:16]
# these are log2 values 
# The data has been normalized


## FIRST visualisation of the data: a simple box plot
boxplot(new.data, 
        las =2, # las = 2 turns the text around to show sample names
        ylab = expression(bold("expression")),
        main="Boxplot of expression data")  

## SECOND visualisation: calculate and visualise distances
# turn into a matrix 
data.m <- as.matrix(new.data)
# transpose the data because a distance matrix works in rows
data.m.t <- t(data.m)
# calculate the distances and put the calculations into an object called distances
distances <- dist(data.m.t)
# convert this distances object into a matrix. 
distances.m <- data.matrix(distances)
# you can look at this object.
View(distances.m)
# we can extract the size of the object and the titles
dim <- ncol(distances.m)
names <- row.names(distances.m)
# now to create the visualisation of the difference matrix. 
# first the coloured boxes
image(1:dim, 1:dim, distances.m, axes = FALSE, xlab = "", ylab = "")
# now label the axis
axis(3, 1:dim, names, cex.axis = 0.8, las=3)
axis(2, 1:dim, names, cex.axis = 0.8, las=1)
# add the values of the differences
text(expand.grid(1:18, 1:18), sprintf("%0.1f", distances.m), cex=1)

## THIRD visualisation: use the distance matrix to do some clustering
plot(hclust(distances))
# three clusters - based on the distance matrix and shows the same thing really.


## FOURTH visualisation: principal component analysis
# Do a Principal Component Analysis  
# and widely-used technique for viewing patterns of gross variation
# in datasets
# We can see whether an array is substantially different to the others
pca <- princomp(data.m)  # function that does the PCA
summary(pca)  # one component accounts for 99% of the variance
plot(pca, type = "l")

names <- factor(gsub("\\.\\d", "", names)) # change names into factor
plot(pca$loadings, col = names, pch = 19)
text(pca$loadings, cex = 0.7, label = colnames(new.data), pos =3)
# some of the treatment cluster well together (e.g. Drug D) others not so much


# can be interesting to look at a subset of the data...
pca <- princomp(data.m[,1:12])
plot(pca$loadings, col = names, pch = 19, 
     main = "Just three drug treatments")
text(pca$loadings, cex = 0.7, label = colnames(new.data)[1:12], pos =3)

# basically happy with our data generally. 
# next step in next script is to look at differentially expressed transcripts.
