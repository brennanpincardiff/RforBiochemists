# As a biochemist, where would you like to work for from an Athena SWAN point of view?

library(RCurl)
library(readxl)

# this is the link to the data
link <- "https://raw.githubusercontent.com/brennanpincardiff/AthenaSWANBenchmarkData/master/ASAwards/AthenaSWANAwardList2012_201520160316.xlsx"

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

# read in the file with the read_excel() function
awards<- read_excel("file.xlsx", col_names = TRUE)

award.type <- c("Gold", "Silver", "Bronze")

# I have created a function to count the different types of Award - Gold, Silver & Bronze
# and then return a data.frame with specific names
countAwardType <- function(x){
  
  # to test this function  make x <- c("Medicine") and go through it line by line. 
  
  # list of the different types of Athena SWAN awards
  award.type <- c("Gold", "Silver", "Bronze")
  
  # find the pattern supplied in the function as x e.g. "Medicine"
  # this creates a data.frame. 
  x.Awards <- awards[grep(x, awards$org),]
  
  # use split() function which separates the data.frame into a list by award level
  # there is a data.frame within each list.
  # this can be called by the dollar sign - $. 
  x.split <- split(x.Awards, x.Awards$Level)
  
  # count the number of rows in each list 
  gold <- nrow(x.split$Gold)
  silver <- nrow(x.split$Silver)
  bronze <- nrow(x.split$Bronze)
  
  # if there are no entries then a NULL value will be returned 
  # zero is better therefore recode the NULL values
  if(is.null(gold) == TRUE) {gold <- 0}
  if(is.null(silver) == TRUE) {silver <- 0}
  if(is.null(bronze) == TRUE) {bronze <- 0}
  
  # combine these variables into a vector
  aW <- c(gold, silver, bronze)
  
  # combine the vectors into a data.frame
  awardsDepts.x <- data.frame(x, aW, award.type)
  
  # rename the columns
  colnames(awardsDepts.x) <- c("Dept.Type", "Award.Count", "Award.Type")
  
  # return the answer. 
  return(awardsDepts.x)
}

# with this function, I can answer the following questions....

# How many Departments of "Biochemistry" have Athena SWAN awards?
# I can answer this using the grep() function 
# this searches for the word "Biochemistry" in the org column from the awards data.frame
# it creates a data.frame called biochemAwards
biochemAwards <- awards[grep("Biochemistry", awards$org),]
nrow(biochemAwards)  # answer 5

# or
# I can use my function countAwardType()
countAwardType("Biochemistry")

# returns this output:
# Dept.Type Award.Count Award.Type
# 1 Biochemistry           0       Gold
# 2 Biochemistry           1     Silver
# 3 Biochemistry           4     Bronze


# As a biochemist you might be willing to work in any "Bio" department
# these include "Biochemistry" of course. 
# or a "Chem" Department 
# or a "Medic" Department like I do...
# or perhaps a Pharmacology department "Pharm"

# lets make a list. 
biochemistry.options <- c("Biochemistry", "Bio", "Chem", "Medic", "Pharm", "Immun")

# loop through the list - make a data.frame for plotting
awardsDepts <- NULL
for(i in 1:length(biochemistry.options)){
  awardsDepts <- rbind(awardsDepts, countAwardType(biochemistry.options[i])) 
}

# re-order the data.frame
awardsDepts$Award.Type <- factor(awardsDepts$Award.Type, levels = c("Bronze", "Silver", "Gold"))

# let's draw a graph
# make the first version of the plot
p <- ggplot(awardsDepts, 
            aes(x=Dept.Type, 
                y=Award.Count, 
                fill=Award.Type)) + 
    geom_bar(stat="identity") +  # makes the barplot
    scale_fill_manual(values=c("#956C3E", 
                               "#757576", 
                               "#A28D30")) + 
    # colours as per AS website - see below
    xlab("") +  # no need for the "Depts" x-axis
    theme_few() # nice clean theme  

# I want to add a y-axis label
# and increase the size of the text
p <- p + ylab("Number of Awards") +
  theme(axis.title.y = element_text(size = 14 )) + 
  theme(axis.text = element_text(size = 14))

# make the legend a bit bigger and move it 
p <- p + theme(legend.position=c(0.9,0.85), # top right
               legend.text=element_text(size = 12), # # inc title size
               legend.title=element_text(size = 12)) #  labels

# have a look at the bar chart
p + ggtitle("Departments Types with Athena SWAN Awards")

# Don't forget some of the quality Research Institutes: 
# Babraham and Institue of Cancer Research both have Silver Awards. 



# by way of contrast let's look at some other department types:
departments <- c("Bio", "Chem", "Medic", "Engin", "Comp", "Math", "Psych", "Physic")
awardsDepts <- NULL
for(i in 1:length(departments)){
  awardsDepts <- rbind(awardsDepts, countAwardType(departments[i])) 
}
awardsDepts$Award.Type <- factor(awardsDepts$Award.Type, levels = c("Bronze", "Silver", "Gold"))

# push in the new data
d <- p %+% awardsDepts
d + ggtitle("Departments Types with Athena SWAN Awards")


# we can also compare Institutions
russell <- c("University of Birmingham", "University of Bristol", "University of Cambridge", 
             "Cardiff University", "Durham University", "University of Edinburgh",
             "University of Exeter", "University of Glasgow", "Imperial College London",
             "King’s College London", "University of Leeds", "University of Liverpool",
             "London School of Economics and Political Science", "University of Manchester",
             "Newcastle University", "University of Nottingham", "University of Oxford",
             "Queen Mary, University of London", "Queen’s University Belfast", 
             "University of Sheffield", "University of Southampton", "University College London",
             "University of Warwick", "University of York")

awardsDepts <- NULL
for(i in 1:length(russell)){
  awardsDepts <- rbind(awardsDepts, countAwardType(russell[i])) 
}
awardsDepts$Award.Type <- factor(awardsDepts$Award.Type, levels = c("Bronze", "Silver", "Gold"))

# push in the new data
inst <- p %+% awardsDepts
inst + ylim(0,50) +  # extend the y axis
    theme(legend.position=c(0.1,0.85)) +   # move the legend
    theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=0.5 )) + # vertical names
    ggtitle("Russell group Universities with Athena SWAN Awards")

# finally Who's got Gold?
awards[grep("Gold", awards$Level),]
Type New_Renew Level Year Month                                                       org
# 70  Dept   Renewal  Gold 2015   Apr               University of York, Department of Chemistry
# 360 Dept       N/A  Gold 2013   Nov          Queen’s University Belfast, School of Psychology
# 361 Dept       N/A  Gold 2013   Nov            University of Cambridge, Department of Physics
# 362 Dept       N/A  Gold 2013   Nov                 University of York, Department of Biology
# 452 Dept       N/A  Gold 2013   Apr          Imperial College London, Department of Chemistry
# 539 Dept       New  Gold 2012   Nov Queen’s University Belfast, School of Biological Sciences
# 540 Dept       New  Gold 2012   Nov              School of Chemistry, University of Edinburgh


# colours from AthenaSWAN website
# https://athena-swan.medschl.cam.ac.uk/wp-content/uploads/2014/03/Athena-SWAN_style-guide_October-2012.pdf
# Bronze: R149 G108 B62
# Silver: R117 G117 B118
# Gold: R162 G141 B48
# Convert RGB to hex http://www.rgbtohex.net/