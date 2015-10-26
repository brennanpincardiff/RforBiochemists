library(dplyr)
library(ggplot2)
library(RCurl)

setwd("/Users/paulbrennan/Dropbox/R for Biochemists/UNData201510")

# who has the most researchers in the world?
# download the data
x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/numberOfResearchersFTEUNdata_Export_20151019_214919785.csv")
data <- read.csv(text = x)

# extract the data for a particular year - this is a decision - 2011 in this case 
# lose some countries as they don't have 2011 data e.g. Australia and India
# option exists to change the year and run again...
year <- 2011
data <- subset(data, data$Time.Period == year) 

# rename United Kingdom, US, Rep of Korea and Venezuela
data$Reference.Area <- gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", data$Reference.Area)
data$Reference.Area <- gsub("United States of America", "United States", data$Reference.Area)
data$Reference.Area <- gsub("Republic of Korea", "Korea, Rep.", data$Reference.Area)
data$Reference.Area<- gsub("Venezuela (Bolivarian Republic of)", "Venezuela", data$Reference.Area)



# sort the data
data.sort <- data[order(-data$Observation.Value),]
data.sort$toPlot <- data.sort$Observation.Value

# plot the top 20 in a bar chart....
p <- ggplot(data.sort[1:20,], 
       aes(x = Reference.Area,
           y = toPlot)) +
  geom_bar(stat="identity") +
  ylab("Number of Researchers (FTE) ") +
  xlab("") +
  ggtitle(paste("Who has the most Researchers?", year)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1))
p

# normalise to population
# get the data (it's on github)
x  <- getURL("https://raw.githubusercontent.com/datasets/population/master/data/population.csv")
pops <- read.csv(text = x)

# extract year = 2011 and countries that we have data from above.
pops <- subset(pops, pops$Year == year)

# join together the two data sets using full_join() function from dplyr
data.Join <- full_join(data, pops, by = c('Reference.Area'='Country.Name'))

# calculate Researchers per 1,000 population
data.Join$res.per.thou <- data.Join$Observation.Value/data.Join$Value*1000

# sort the data
data.Join.sort <- data.Join[order(-data.Join$res.per.thou),]
data.Join.sort$toPlot <- data.Join.sort$res.per.thou 

# this %+% command is useful. 
# It allows us to force a different data.frame into an existing ggplot graph object 
p1 <- p %+% data.Join.sort[1:20,]
# After doing that we need to change the title and the ylab. 
p1 + ylab("Researchers (FTE) per 1,000 population ") +
     ggtitle(paste("Who has the highest density of Researchers? ", year))  

# Explore the proportion of GDP spent on R&D?
x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/grossDomesticExpR&D_GERD_percent_GDP_UNdata_Export_20151019_215514491.csv")
data.GDP <- read.csv(text = x)
# extract the 2011 data
data.GDP <- subset(data.GDP, data.GDP$Time.Period == year)

# rename United Kingdom, US, Rep of Korea and Venezuela
data.GDP$Reference.Area <- gsub("United Kingdom of Great Britain and Northern Ireland", "United Kingdom", data.GDP$Reference.Area)
data.GDP$Reference.Area <- gsub("United States of America", "United States", data.GDP$Reference.Area)
data.GDP$Reference.Area <- gsub("Republic of Korea", "Korea, Rep.", data.GDP$Reference.Area)
data$Reference.Area<- gsub("Venezuela (Bolivarian Republic of)", "Venezuela", data$Reference.Area)


# sort the data
data.GDP.sort <- data.GDP[order(-data.GDP$Observation.Value),]
data.GDP.sort$toPlot <- data.GDP.sort$Observation.Value

# again use %+% command is to force a different data.frameinto an existing ggplot graph object
p2 <- p %+% data.GDP.sort[1:20,]
# and give it the appropriate titles
p2 + ylab("Expenditure on R&D as a Percentage of GDP") +
  ggtitle(paste("Relative spending on R&D - ", year))  


# how do the two things relate?
data.Join.again <- full_join(data.Join, data.GDP, by = c('Reference.Area'='Reference.Area'))

p3 <- ggplot(data.Join.again, 
            aes(x = res.per.thou,
                y = Observation.Value.y,
                label = Reference.Area)) +
            geom_point(aes(size = sqrt(Value/pi)), pch = 21, show_guide = FALSE) +
            ylab("Expenditure on R&D (Percentage of GDP)") +
            xlab("Researchers Per 1000 of the population") +
            ggtitle(paste("Who is serious about the knowledge Economy? ", year)) +
            theme_bw() + 
            scale_size_continuous(range=c(1,40))
p3  

# label selected circles
wanted <- c("Finland", "United States", "United Kingdom", "China", 
            "Denmark", "Israel", "Japan", "Iceland", "Singapore", 
            "France", "Russian Federation", "Korea, Rep.", "Norway")

labels <- NULL
for( i in 1:length(wanted)) { 
 labels<- rbind(labels, data.Join.again[data.Join.again$Reference.Area == wanted[i], ])
}

p3 + geom_text(data=labels,
              aes(x = res.per.thou,
                  y = Observation.Value.y,
                  label = Reference.Area),
              colour = "darkgrey",
              size = 5, hjust=1, vjust=-0.5)

# Colour by continent inspired by Gapminder
# Data adapted from http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderCountryColors.txt
x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/countryColors20151026")
countryColors <- read.csv(text = x)

data.Join.again2 <- full_join(data.Join.again, countryColors, by = c('Reference.Area'='country'))

p4 <- ggplot(data.Join.again2, 
             aes(x = res.per.thou,
                 y = Observation.Value.y,
                 label = Reference.Area, )) +
  geom_point(aes(size = sqrt(Value/pi)), pch = 21, show_guide = FALSE) +
  ylab("Expenditure on R&D (Percentage of GDP)") +
  xlab("Researchers Per 1000 of the population") +
  ggtitle(paste("Who is serious about the knowledge Economy?  ", year)) +
  theme_bw() + 
  scale_size_continuous(range=c(1,40))

p4 <- p4 + aes(fill = continent)

labels <- NULL
for( i in 1:length(wanted)) { 
  labels<- rbind(labels, data.Join.again2[data.Join.again2$Reference.Area == wanted[i], ])
}


p4 + geom_text(data=labels,
              aes(x = res.per.thou,
                  y = Observation.Value.y,
                  label = Reference.Area),
              colour = "black",
              size = 5, hjust=1, vjust=-0.5)

# the facet_wrap is interesting...
p4 + facet_wrap(~ continent)
