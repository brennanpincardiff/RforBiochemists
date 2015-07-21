# activate the required packages
library(ggplot2)
library(reshape2)
library(ggthemes)

# do search in pubmed on the web
# pubmed - "chronic lymphocytic leukaemia" or "chronic lymphocytic leukemia" or "CLL"
# get an option to download a csv file
# open the file and remove the top line to prevent an error 
# add other data if you want and change the column titles to something useful
setwd("/Users/paulbrennan/Documents/Work Documents/Staff & Collaborations/RforBiochemists/RforBiochemists/data")
data <- read.csv("cllPublicationsformatted.csv")

# reshape the data from wide to long format
data.melt <- melt(data, id.vars = "year", value.name = "pubs", variable.name = "pub.type")

# draw the graph
p <- ggplot(data.melt, aes(x=year, 
                           y= pubs, 
                           colour = factor(pub.type, labels = c("Articles", "Reviews")))) + 
# colour = factor and the labels allows us to customize the legend
    geom_line(size=1) +
    labs(color = "Publication Type") + # customizes the legend title
    scale_colour_manual(values=c("black","red")) +
    ylab("Number Publications") + # y-label
    xlab("Year") + # x-label
    ggtitle("Chronic Lymphocytic Leukaemia Publications by Year") +
    scale_x_continuous(breaks=c(1960,1970,1980,1990, 2000, 2010)) +
    theme_bw()

p <- p + theme(legend.position=c(0,1), # moves to the top left
               legend.justification=c(0,1), # moves in a bit
               legend.text=element_text(size = 12), # increase size of text
               legend.title=element_text(size = 12)) # and the title
               
p <- p + theme(axis.title.y = element_text(size = 14 )) + 
         theme(axis.text = element_text(size = 12))

p
