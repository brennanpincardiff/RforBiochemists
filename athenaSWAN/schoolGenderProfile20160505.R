# overview of the School in terms of gender for our Athena SWAN application
library(readxl)
library(ggplot2)
library(reshape2)

# the data is here
link <- "https://raw.githubusercontent.com/brennanpincardiff/AthenaSWANBenchmarkData/master/asStaffOverview_comparedtype_20160505.xlsx"

# the download.file() function downloads and saves the file with the name given
download.file(url=link, destfile="file.xlsx", mode="wb")

# read in the data...
data <- read_excel("file.xlsx")

# calculate Female and Male as a percentage
data$tot <- data$f + data$m
data$Female <- (data$f / data$tot) *100 
data$Male <- (data$m / data$tot) *100

# first a simple plot of the Cardiff School of Medicine Data
# plot the number of females of each type of member of the School
# pull out the Cardiff data
data.c <- data[data$uni == "Cardiff",2:5]

# make the object with the Cardiff subset 
g <- ggplot(data.c,
       aes(x = type,
           y = f)) +
     geom_bar(stat="identity") +
     theme(axis.text.x = element_text(size = 14))

# show the graph
g

# The order of the bars is not what I want.
# take the order from the data file
types <- data.c$type

# apply this order to the graph
# http://stackoverflow.com/questions/5208679/order-bars-in-ggplot2-bar-graph
g + scale_x_discrete(limits = types)

# I want to put female and male on the same graph so...
# pull out and melt the Cardiff data so that have f and m for each type in long format
data.melt <- melt(data[data$uni == "Cardiff",2:5], 
                  id.var = c("type", "year"))

# put the data in an object
g <- ggplot(data.melt, 
       aes(x = type, 
           y = value,
           fill = variable)) + 
      scale_x_discrete(limits = types) +
      theme(axis.text.x = element_text(size = 14))

# show with geom_bar
# this is a stacked bar chart showing both female and male numbers
g + geom_bar(stat="identity")

# show female and male in separate bars using position arguement
g + geom_bar(stat="identity", position="dodge")

# have as stacked bar chart but normalised to one. 
g +  geom_bar(stat="identity", position="fill")


# I really want a percentage of each stacked 
# to allow different parts of the School to be compared
# remove the raw numbers
data.s <- data[, -c(3:6)]
data.melt.s <- melt(data.s[data.s$uni == "Cardiff",2:4], 
                  id.var = c("type"))  # id = or id.var are the same!
colnames(data.melt.s) <- c("type", "Gender", "Percent")

# make a new graph
p <- ggplot(data.melt.s, 
           aes(x = type, 
               y = Percent,
               fill = Gender)) +
      geom_bar(stat="identity") + 
      scale_x_discrete(limits = types) +
      ylab("Percentage of staff") +
      xlab("") +
      theme_bw() +
      theme(axis.text.x = element_text(size = 14))
p

# adjust the colours using scale_fill_brewer()
p + scale_fill_brewer(palette = 16, direction=-1) +
  # andd add a title
    ggtitle("Gender breakdown of different members of the School of Medicine") +
    theme(axis.title.y = element_text(size = 14 ))

# use data from Imperial & Leeds as comparison
# use the facet_wrap function to make all looks the same. 
# remove Prof & Support as we only have for Cardiff
data.s <- data.s[-5,]
types <- types[-5]

# melt the data into the format for the bar chart. 
data.melt.s <- melt(data.s, 
                    id.var = c("uni", "type"))  # id = or id.var are the same!
colnames(data.melt.s) <- c("uni", "type", "Gender", "Percent")

# this is a final plot with three Medical Schools (it's a bit big). 
p <- ggplot(data.melt.s, 
            aes(x = type, 
                y = Percent,
                group = uni,
                fill = Gender)) +
  geom_bar(stat="identity") +
  facet_wrap(~uni) +   # this function gives three plots
  scale_x_discrete(limits = types) +
  ylab("Percentage of staff") +
  xlab("") +
  theme_bw() +
  scale_fill_brewer(palette = 16, direction=-1) +
  ggtitle("Gender breakdown of three Schools of Medicine") +
  theme(strip.text.x = element_text(size = 14, colour = "black")) +
  theme(axis.text.x = element_text(size = 11))

p