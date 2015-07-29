# This data file has a list of the PubMed IDs, the year and the citation data 
data <- read.csv("http://science2therapy.com/data/cllCitation2010to2014_20150722.csv", header=T)

str(data)
cit <- data$cit

# not very useful but good practice to plot the data first...
plot(density(cit))
plot(density(cit), log='x')
hist(cit)

# not very useful in ggplot either. 
p <- ggplot(data=data,          # specify the data frame with data
            aes(x=cit)) +       # specify the x and y for the graph
  geom_bar(binwidth = 10)    # it's a bar plot
p      # show the plot

# so lots of the publications with relatively few citations. 

# Do some subsetting to identify highly cited papers. 
# http://www.statmethods.net/management/subset.html

# calculate the mean number of citations 
mean.cit <- mean(data$cit)   # 4.1 for this data set. 

# data frame of publications with no citations
newdata.zero <- subset(data, cit == 0)

# data frame of publications with one citation
newdata.one <- subset(data, cit == 1)

# make a data frame of publications with more than one citations upto the mean 
newdata.greater1 <- subset(data, cit > 1)
newdata.mean <- subset(newdata.greater1, cit < mean.cit)

# make a data frame of publications with more than the mean citations
# up to the mean squared
newdata.greatermean <- subset(data, cit > mean.cit)
newdata.meanSq <- subset(newdata.greatermean, cit < (mean.cit^2))

# make a data frame of publications with more than the mean squared citations
# up to the mean cubed
newdata.greatermeanSq <- subset(data, cit > (mean.cit^2))
newdata.SqtoCube <- subset(newdata.greatermeanSq, cit < (mean.cit^3)) 

# make a data frame of publications with more than the mean cubed citations
newdata.greaterCube <- subset(data, cit > (mean.cit^3))

# assemble these numbers into a vector
count<-c(nrow(newdata.zero), nrow(newdata.one), nrow(newdata.mean), 
         nrow(newdata.meanSq), nrow(newdata.SqtoCube), nrow(newdata.greaterCube))

# simple barplot
barplot(count)

# create a list of labels
lab=c("0","1","2-4","5-16","17-64", ">64")

# assemble a new data frame to plot with ggplot
df <- as.data.frame(count)
df$label <- lab
df$labfac <- factor(df$label, as.character(df$label))


# do a nice histogram of citation frequency in ggplot
p <- ggplot(data=df, aes(y=count)) + 
  geom_bar(aes(x=labfac), data=df, stat="identity") + 
  xlab("Number of citations") +   # label x-axis
  ylab("Number of Papers") +    # label y-axis
  ggtitle("Chronic Lymphocytic Leukemia Papers published 2010 to 2014") +  # add a title
  theme_bw() +      # a simple theme
  expand_limits(y=c(0,2000)) +   # customise the y-axis
  theme(axis.title.y = element_text(size = 14 )) + 
  theme(axis.title.x = element_text(size = 14 )) + 
  theme(axis.text = element_text(size = 12))

p    #show us the plot