# activate the required packages
library(ggplot2)
library(reshape2)
library(ggthemes)

# here is the data - gathered from various awards booklets
# and the ECU press release
years <- c("2009","2010","2011","2012","2013", "2014")
bronze <- c(19,  13,	25,	66,	135,	152)
silver <- c(16, 16,	14,	26,	40,	43)
gold <- c(0,1,0,2,4,0)

# create the data frame required for ggplot
swan.df <- as.data.frame(years)
swan.df$Bronze <- bronze
swan.df$Silver <- silver
swan.df$Gold <- gold

# reshape the data from long into short format
as.melt <- melt(swan.df, id.vars = "years", value.name = "number", variable.name = "Awards")

# make the first version of the plot
p <- ggplot(as.melt, aes(x=years, y=number, fill=Awards)) + 
     geom_bar(stat="identity") +  # this bit makes the barplot
     scale_fill_manual(values=c("#956C3E", "#757576", "#A28D30")) + # colours as per AS website - see below
     xlab("") + #no need for the "years" x-axis
     theme_few() # nice clean theme  

# I want to add a nice y-axis label
# and increase the size of the text
p <- p + ylab("Number of Awards") +
     theme(axis.title.y = element_text(size = 14 )) + 
     theme(axis.text = element_text(size = 12))

# make the legend a bit bigger and move it to top left 
p <- p + theme(legend.position=c(0,1), # moves it to the top left
               legend.justification=c(0,1), # moves it in a bit
               legend.text=element_text(size = 12), # increase the size of the title
               legend.title=element_text(size = 12)) # and the labels

# have a look at the bar chart
p + ggtitle("The Rise and Rise of Athena SWAN")

# save the graph....
p + ggsave("AthenaSWANawards09_14.pdf")


# colours from AthenaSWAN website
# https://athena-swan.medschl.cam.ac.uk/wp-content/uploads/2014/03/Athena-SWAN_style-guide_October-2012.pdf
# Bronze: R149 G108 B62
# Silver: R117 G117 B118
# Gold: R162 G141 B48
# Convert RGB to hex http://www.rgbtohex.net/

