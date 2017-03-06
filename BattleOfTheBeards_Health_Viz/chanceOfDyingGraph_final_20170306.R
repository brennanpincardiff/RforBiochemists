# I am preparing a session for the Battle of the Beards
# It's an IT conference with difference 
# Fight male suicide at “The Battle of the Beards”
# Talking tech & making a difference
# Cardiff | 29 March 2017
# http://battleofthebeards.info/

library(readxl)
library(ggplot2)

# my preparation involves exploring the UK National Life Tables
# these are available through the Office of National Statistics
# as an excel file from this page: 
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables
# the can be downloaded from here:
ons_url <- c("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables/current/nltuk1315reg.xls")
# to guarantee undistrubed access, I've put the file on github 
github_url <- "https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/nltuk1315reg.xls"

# the download.file() function downloads and saves the file with the name given
download.file(url=ons_url,destfile="file.xls", mode="wb")
# if this doesn't work try replacing the url with the github_url
# then we can open the file and extract the data using the read_excel() function. 
data<- read_excel("file.xls", sheet = 5, skip = 6)

# need to remove four bottom rows - these are all blank. 
data <- data[1:101,]

colnames(data)

# x = age in years
# first set is males and second set is females
# mx is the central rate of mortality, defined as the number of deaths at age x last birthday in the three year period to which the National Life Table 
# relates divided by the average population at that age over the same period.

# qx is the mortality rate between age x and (x +1), that is the probability that a person aged x exact will die before reaching age (x +1).	

# lx is the number of survivors to exact age x of 100,000 live births of the same sex who are assumed to be subject throughout their lives to the 	
# mortality rates experienced in the three year period to which the National Life Table relates.	

# dx is the number dying between exact age x and (x +1) described similarly to lx, that is dx=lx-lx+1.	

# ex is the average period expectation of life at exact age x, that is the average number of years that those aged x exact will live thereafter	
# based on the mortality rates experienced in the three year period to which the National Life Table relates.	

# to allow us to separate the male and female data, I am relabelling the column names

colnames(data) <- c("age", 
                    "m.mx", "m.qx", "m.lx", "m.dx", "m.ex",
                    "f.mx", "f.qx", "f.lx", "f.dx", "f.ex")



# the life table allow us to draw some interesting curves
# the first one I want to explore is my chance of dying this year. 

# qx is the key value. 
# the chance of my dying before my next birthday. 

# here is the graph in ggplot
ggplot(data = data,
       aes(x = age, y = m.qx)) +
  geom_point()

# The log 10 plots give an interesting pattern. 
# first shown to me by Professor David Spiegelhalter
# Here it is in my blog https://understandinguncertainty.org/what-your-effective-age
ggplot(data = data,
       aes(x = age, y = log10(m.qx))) +
  geom_point() 


# Another way to change the scale on the y-axis using scale_y_log10()
ggplot(data = data,
       aes(x = age, y = m.qx)) +
  geom_point() +
  scale_y_log10()

# let's make this look a bit nicer with lines, titles and a theme...
p <- ggplot(data = data,
            aes(x = age, y = m.qx)) +
     geom_line(colour = "red") +
     geom_point(colour = "red", size = 0.75) +
     ylab("Chance of dying") +
     xlab("Current Age") +
     ggtitle("Chance of Dying before next Birthday") + 
     scale_y_log10() +
     theme_bw()     

p

# label on y axis is not the most useful, I think. 
# good learning point here on customising the y-axis label.
# added as arguements in the scale_y_log10()

p <- ggplot(data = data,
       aes(x = age, y = m.qx)) +
  geom_line(colour = "red") +
  geom_point(colour = "red", size = 0.75) +
  ylab("Chance of dying") +
  xlab("Current Age") +
  ggtitle("Chance of Dying before next Birthday") + 
  scale_y_log10(
    breaks = c(0.0001, 0.001, 0.01, 0.1, 0.5),      # says where to put the labels
    labels = c("1/10,000", "1/1000", "1/100", "1/10", "1/2")) +   # the labels. 
  theme_bw() 

p

# want add source too!!!
source <- paste("Source: Office of National Statistics\n filename: nltuk1315reg.xls\n accessed:", Sys.Date())
# at bottom outside of the graph... or as a subtitle

p + annotate("text",  x = 70, y = 0.0001, label=source, size=3 )

# more info available from the Office of National Statistics here:
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies

