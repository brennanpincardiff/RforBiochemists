# install.packages("RCurl", "scatterplot3d")

library(RCurl)
library(scatterplot3d)

# A few year's ago a gene expression profile paper was published in Blood by Herishanu et al
# that gave us a new insight into chronic lymphocytic leukamia. 
# http://www.bloodjournal.org/content/117/2/563.long
# I have written this script to explore the data 
# and to reproduce the principal component analysis. 

# I have downloaded the original CEL files from GEO
# http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE21029
# they were placed in a folder and the expression values extracted 
# what's produced is Affymetrix IDs and log2 expression values 

# I have created two data sets
# we can reproduce the PCA workflow with a subset of the data is required (5000 genes selected randomly). 
# this will be faster to download and visualise. 
# get the data - needs connection to the internet
# to use remove the hashtag "#"
# x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/herishanuMicroArrayDataSubset.tsv")

# here is a link for all the data (>50000 probes) 
x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/herishanuMicroArrayData.tsv")

# turn the data into a useful data.frame
data <- read.table(text = x, header = TRUE, sep = "\t")

# read in a file with names of samples
# created this from cut and paste of web page combined with manipulation in Word & Excel
sampID <- read.csv("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/Herishanu_Samp_ID.csv", header = FALSE)

str(sampID)
sampID$V1 <- as.character(sampID$V1)

# is order of column names same as list in sampID?
data.col.name <- colnames(data)
data.col.name <- gsub(".CEL", "", data.col.name)
sampID.name <- sampID$V1
all.equal(data.col.name, sampID.name)
# TRUE so that is useful. 


# change column names from chip to PB, BM and LN
# for peripheral blood, bone marrow and lymph node respectively
sampID$Name <- paste0(as.character(sampID$V3), as.character(sampID$V5))
colnames(data) <- sampID$Name

# how do the sample cluster?
# I first need to find the "distances" between the arrays
# and show those distances in the hierarchical cluster plot
# I can pile up all the commands in one
# for more of a step by step approach, see these scripts:
# http://rforbiochemists.blogspot.co.uk/2016/02/visualising-some-cll-proteomic-data-for.html
# http://rforbiochemists.blogspot.co.uk/2016/03/gene-expression-analysis-and.html

plot(hclust(dist(t(data))))

# this is interesting because all the patient samples cluter together!
# with bone marrow and peripheral blood closer together than LN in all cases. 
# this is what it says in the paper and is shown in the supplementary data. 
# http://www.bloodjournal.org/content/117/2/563/tab-figures-only

# What's needed is a way to normalise by patient - HOW??
# Quote from the paper:
# "In the 12 patients in whom all 3 compartments had been arrayed, 
# the patient effect on gene expression was subtracted 
# by mean centering the expression value of each gene across the 3 compartments 
# for each patient separately."

# the twelve with all 3 compartments: #1, #2, #3, #4, #8, #9, #10, #11, #12, #13, #25, #26

# so if I understand this correctly, I need to extract a patient sample
# then mean centre all three samples, 
# and repeat for all 12 samples

# let's explore this concept with just two samples to see if it works
# pick out data for patient #26
samp26 <- data[,grep("#26", sampID$Name)]

# pick out data for patient #25
samp25 <- data[,grep("#25", sampID$Name)]

# put them together
twosamp <- cbind(samp26, samp25)

# visualise with a box plot - look normalised
boxplot(twosamp)
plot(hclust(dist(t(twosamp))))
# cluster by patient

# I think I understand what they have done.
# mean centred for each gene.... i.e. by row in the data
# For us this is really each probeset
# http://gastonsanchez.com/how-to/2014/01/15/Center-data-in-R/

# for each of the values substract the rowMeans
samp25.s <- samp25 - rowMeans(samp25)
samp26.s <- samp26 - rowMeans(samp26)
twosamp.s <- cbind(samp26.s, samp25.s)

plot(hclust(dist(t(twosamp.s))))

# This seems to work and gives a different cluster diagram
# with LN coming together nicely.

# apply this to the whole data set.
# extract each patient cohort
# these twelve were: #1, #2, #3, #4, #8, #9, #10, #11, #12, #13, #25, #26
# do #1 and # 2 first then do the other automatically...

# pat # 1
colnames(data)
pat_1 <- cbind(data[,1], data[,27], data[,46])
colnames(pat_1) <- c("PB#1", "BM#1", "LN#1")
pat_1.s <- pat_1 - rowMeans(pat_1)
# pat # 2
pat_2 <- cbind(data[,2], data[,28], data[,47])
colnames(pat_2) <- c("PB#2", "BM#2", "LN#2")
pat_2.s <- pat_2 - rowMeans(pat_2)

data.n <- cbind(pat_1.s, pat_2.s)

data.reqd <- c("#3", "#4", "#8", "#9", "#10", "#11", "#12", "#13", "#25", "#26")
for(i in 1:length(data.reqd)){
  samp <- data[,grep(data.reqd[i], sampID$Name)]
  samp <- samp - rowMeans(samp)
  data.n <- cbind(data.n, samp)
}

# now have a file called dat.exp.n - normalised within patients.
colnames(data.n)
plot(hclust(dist(t(data.n))))
# nice cluster by region with good separation of LN samples
# some overlap within PB and LN

# extract cell site to use for colours in PCA plots
names <- colnames(data.n)
colourby <- gsub("#\\d+", "", colnames(data.n))
colourby <- gsub("\\.", "", colourby)
colourby <- gsub("\\d+", "", colourby)

# this does cluster the LN samples distinctly but there is still some overlap 
# of Peripheral Blood and Bone Marrow. 
# PCA from the paper looks nice and convincing

# can we make this?
# http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/
# uses function princomp()

exp.pca <- princomp(data.n)  # this function does the PCA
print(exp.pca)
plot(exp.pca, type = "l")
plot(princomp(data.n)$loadings)


# http://www.r-bloggers.com/visualizing-principal-components/
p <- princomp(data.n)
loadings <- p$loadings[]
p.variance.explained <- p$sdev^2 / sum(p$sdev^2)

# plot percentage of variance explained for each principal component    
barplot(100*p.variance.explained, las=2, xlab='', ylab='% Variance Explained')

#*****************************************************************
# 2-D Plot
#******************************************************************         
x <- loadings[,1]
y <- loadings[,2]
z <- loadings[,3]
cols <- as.factor(colourby)
cols <- gsub("PB", "green", cols)
cols <- gsub("BM", "red", cols)
cols <- gsub("LN", "blue", cols)

# pch = 24: triangle point-up,
# pch = 22: square
# pch = 21: circle,
symbols <- as.factor(colourby)
symbols <- gsub("PB", 24, symbols)
symbols <- gsub("BM", 21, symbols)
symbols <- gsub("LN", 22, symbols)
symbols <- as.numeric(symbols)


# plot loadings on the first and second principal components 
# identify sample by body location
plot(x, y, type='p', 
     pch=symbols, 
     xlab='Comp.1', ylab='Comp.2', 
     col = cols, main = "Principal Component Analysis - CLL cell gene expression")

# add a legend to the top of the plot
legend("top",      # location
       bty="n",              # suppress legend box, shrink text 50%
       title="Body Location of sample",
       c("PB", "BM", "LN"), 
       pch=symbols, col = cols, horiz=TRUE)

# label up the points
text(x, y, colnames(data.n), col=cols, cex=.5, pos=4)

# Comment: lymph node samples separate nicely from other samples
# some overlap between bone marrow and peripheral blood 


#*****************************************************************
# 3-D Plot, for good examples of 3D plots
# http://statmethods.wordpress.com/2012/01/30/getting-fancy-with-3-d-scatterplots/
#******************************************************************                 

# plot all companies loadings on the first, second, and third principal components and highlight points according to the sector they belong
s3d = scatterplot3d(x, y, z, 
                    xlab='Comp.1', ylab='Comp.2', zlab='Comp.3', 
                    color=cols, pch = symbols,
                    main = "Principal Component Analysis in 3D \n CLL cell gene expression")

s3d.coords = s3d$xyz.convert(x, y, z)
text(s3d.coords$x, s3d.coords$y, 
     labels=colnames(data.n), col=cols, cex=.8, pos=4)

# change the order and the angle to make it look a bit more like the figure
s3d = scatterplot3d(y, x, z, 
                    xlab='PC2', ylab='PC1', zlab='PC3',                     
                    color=cols, pch = symbols,
                    angle=-25,
                    grid = FALSE,
                    main = "Principal Component Analysis in 3D \n CLL cell gene expression")

par(xpd=TRUE)    # allows legend outside the graph

# add legend
legend(10, -4.5,     # location
       bty="n",              # suppress legend box
       title="Site of sample",
       c("PB", "BM", "LN"), 
       pch=symbols, col = cols, horiz=TRUE)

# add source as text
text(4, -6.5, cex =0.7,
     "Source: Herishanu et al, Blood 2011 117:563-574; doi:10.1182/blood-2010-05-284984")
