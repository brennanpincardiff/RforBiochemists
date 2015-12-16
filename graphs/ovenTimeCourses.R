library(reshape2)
library(ggplot2)
library(RCurl) # allows us to download data through urls 

# URL for May data:
May <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/ovenTempMay.tsv")

# URL for Jun data:
Jun <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/ovenTempJun.tsv")

# URL for Aug data: 
Aug <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/ovenTempAug.tsv")

# put the URLs together in a vector
urls <- c(May, Jun, Aug)
months <- c("May", "Jun", "Aug")

# do it in a loop to apply to each of the three data sets
for(i in 1:3){
  
  # read in the data for the relevant month - it's a tab separated file 
  data <- read.table(text = urls[i], stringsAsFactors=FALSE, sep = "\t", header = TRUE)
  
  
  # convert the data and time into a format that R will understand. make it into a POSIXct object. 
  # so as not to mess with the original data put it in a new column called time.P 
  # because my dates are separted by slashes "/", I need to tell R about the format. 
  data$time.P <- as.POSIXct(data$time, format = "%d/%m/%Y %H:%M")
  
  # the first value is the start.time for this temperature profile. 
  start.time <- data$time.P[1]
  
  # calculate the elapsed time 
  # substract the start time from each value. 
  # This returns the time in seconds
  data$e.time.sec <- data$time.P - start.time
  # convert this into an hour time and change back into a number for graphing purposes. 
  data$e.time.hour.num <- as.numeric(data$e.time.sec/3600)
  
  
  # take out the data we need for the plot
  data.subset <- as.data.frame(data$e.time.hour.num)
  colnames(data.subset)[1] <- "e.time.hour.num"
  data.subset$top <- data$top
  data.subset$bot <- data$bot
  
  # melt it into a format for ggplot using melt() function
  data.subset.melt <- melt(data.subset, 
                           id.vars = "e.time.hour.num")
  colnames(data.subset.melt) <- c("elapsed.time", "place", "temp")
  
  # make the graph object
  p <- ggplot(data.subset.melt, aes(x=elapsed.time, 
                                    y= temp, 
                                    colour = factor(place, labels = c("Top", "Bottom")))) + 
    # colour = factor and the labels allows us to customize the legend
    geom_line(size=1) +
    geom_point() +
    labs(color = "Place") + # customizes the legend title
    scale_colour_manual(values=c("black","red")) +
    ylab("Temperature") + # y-label
    xlab("Elapsed time (hours)") + # x-label
    ylim(0,400) +
    scale_x_continuous(limits=c(0, 48), 
                       breaks=c(0,1,4,8,12, 24, 48)) +
    theme_bw()
  
  # position the legend  
  p <- p + theme(legend.position=c(1,1), # move to the top right
                 legend.justification=c(1,1), # move it in a bit
                 legend.text=element_text(size = 12), # increase size of text
                 legend.title=element_text(size = 12)) # and the title
  
  p <- p + theme(axis.title.y = element_text(size = 14 )) + 
    theme(axis.text = element_text(size = 12))
  
  # add the appropriate title
  p <- p + ggtitle(paste0("Oven Temp (", months[i], " Firing)")) 
  
  # print the object - you have to do this because of the loop
  print(p)
}
