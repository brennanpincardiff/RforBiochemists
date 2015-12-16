# draw the NF-kappaB subunit, Rel A (p65) with R.
# draw it as a series of rectanges
# going from left to right. 

# using the Uniprot webpage for the informaton
# http://www.uniprot.org/uniprot/Q04206
# cut and paste from the XML page to create two objects
# first object is a list containing accession number and names

## Step 1: a list containing names and details 
# list of names...
names <- list(
  accession = c("Q04206"),
  name = "TF65_HUMAN",
  protein.recommendedName.fullName = "Transcription factor p65",
  protein.recommendedName.alternativeName = "Nuclear factor NF-kappa-B p65 subunit",
  protein.fullName = "Nuclear factor of kappa light polypeptide gene enhancer in B-cells 3",
  gene.name.primary = "RELA",
  gene.name.synonym = "NFKB3",
  organism.name.scientific = "Homo sapiens"
)

## step 2: create the data frame with all the information. 
# cut and past from the XML page to make vectors
# features to plot 
types <- c("chain", "domain", "region of interest", "short sequence motif", "short sequence motif") 
description <- c("Transcription factor p65", "RHD", "Activation domain","Nuclear localization signal", "9aaTAD")
begin <- c(1,19, 415, 301, 536)
end <- c(551, 306, 459, 304, 544)
col <- c("white", "blue", "red", "black", "orange")

# assemble vectors into a data frame
features <- data.frame(types, description, begin, end, col)

# check the structure of the data frame
str(features)
# shows description and col to be factors - this will cause problems later...
# so change them now
features$description <- as.character(features$description)
features$col <- as.character(features$col)  

# it will be better if we sort features in order of where they begin
features <- features[order(features$begin),]

## step 3: draw the diagram
screen.width <- max(features$end)
screen.height <- 25  # this is a bit arbitary
plot(c(-10, screen.width), 
     c(0, screen.height), 
     type= "n", 
     xlab = "Number of amino acids", 
     ylab = "", yaxt='n')    # suppress the y label and y axis
# scaled.mol.sizes <- mol.sizes/screen.width

# make the rectangles in a loop
for (i in 1:length(features$types) ) {
  rect(xleft   = features$begin[i],
       ytop    = screen.height/2 + 2.5,
       ybottom = screen.height/2 - 2.5,
       xright  = features$end[i],
       col = features$col[i])
}

# add text to the top of the illustration with the recommended name
text(max(features$end)/2, screen.height-2.5, names$protein.recommendedName.fullName, cex=1.5)
# and the alternative name
text(max(features$end)/2, screen.height-5, names$protein.recommendedName.alternativeName, cex=1)

# add the descriptions of the features
pos.text.x <- features$begin[2:5] + (features$end[2:5] - features$begin[2:5])/2
pos.text.y <- c(screen.height/2 + 3.5, screen.height/2 - 3.5)
text(pos.text.x, pos.text.y, features$description[2:5], cex=1, col=features$col[2:5])

# add the accession number to the bottom smaller text and the source of the data
text(max(features$end)/2, 5 , paste("Uniprot Accession Number:", names$accession), cex=0.8)
text(max(features$end)/2, 3 , "Souce of data: http://www.uniprot.org/uniprot/Q04206", cex=0.8)