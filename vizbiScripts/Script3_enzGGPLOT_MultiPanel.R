# The plot with the six lines is a bit busy. 
# Let's make a ggplot equivalent to the base R multiplot.
# Here we do it using faceting.
# To make the script free standing the reshaping code is included. 

setwd("/Users/paulbrennan/Documents/RforBiochemistsScripts/VizBiMar2016")

library(reshape2)
library(ggplot2)
library(ggthemes)

# some data:
enzdata <- matrix(c(0, 17.36667, 31.97143, 52.68889, 61.95385, 74.2, 77.97143, 84.28, 99.91429, 93.66667, 
                    0, 15.7, 29.42286, 45.64, 62.60615, 75.78118, 69.88, 75.256, 89.59429, 86.84, 
                    0, 27.10667, 42.12, 63.48, 69.56, 74.26857, 79.44444, 83.29091, 87.1, 82.08571, 
                    0, 24.72, 39.07, 47.4, 57.928, 67.6, 71.35556, 67, 75.79375, 70.86667, 
                    0, 5.723636, 11.48, 17.697143, 28.813333, 37.567273, 42.483077, 40.68, 52.81, 56.92, 
                    0, 2.190476, 5.254545, 8.95, 15.628571, 20.8, 25.355556, 26.55, 32.44, 33.333333),
                  nrow = 10, ncol = 6)
no.Exp <- c("Exp.1", "Exp.2", "Exp.3", "Exp.4", "Exp.5", "Exp.6")
Sub <- c(0, 1, 2, 4, 8, 12, 16, 20, 30, 40)	

enzdata <- as.data.frame(enzdata) # converted to a data.frame for plotting
colnames(enzdata) <- rep("Exp", times=6)  # add a column name
enzdata <- cbind(Sub, enzdata) # add the Substrate data

# so we have a data.frame and we want to draw some graphs. 
# as is always the case with R, there are many ways to do this. 

# using our dataframe we can draw the graphs one by one...

p <-  ggplot(data = enzdata, aes(x = Sub, y = Exp)) +
  geom_point() +
  theme_few() +
  ylab("velocity (nmol/s)") + xlab("substrate (mM)") +
  ggtitle("Experiment 1")
p

# one of the useful feature of ggplot is that you can force new data into an old plot
# we have the data from Exp 6 in an object. 
# if we make the same shaped dataframe with another set of data we can force that in.
new.data <- data.frame(enzdata$Sub, enzdata[,3])
colnames(new.data) <- c("Sub", "Exp")  # col names must be the same
p <- p %+% new.data   # this %+% is what forces in the new data...
p <- p + ggtitle("Experiment 2")
p


# we can use this feature and put making and saving the graphs inside a loop
for(i in 1:6) {  
  new.data <- data.frame(enzdata$Sub, enzdata[,i+1])
  colnames(new.data) <- c("Sub", "Exp")  # col names must be the same
  p <- p %+% new.data
  p <- p + ggtitle(paste("Experiment", i))
  filename <- paste0("Experiment_", i, ".pdf")
  p + ggsave(filename)
}
# it would probably be better style to write a function and vectorise it!


## there is also an 'even better' way to do it using facets - a useful feature of ggplot
# requires us to change the structure of the dataframe
# melt the data from wide to long:
colnames(enzdata) <- c("Sub", "Exp.1", "Exp.2", "Exp.3", "Exp.4", "Exp.5", "Exp.6")

melted_data <- melt(enzdata, id.vars = "Sub", value.name = "v", variable.name = "Exp")

# Check out the differences between these two files:
view(enzdata)
view(melted_data)

# the way ggplot draws colours depends on the required number of colours. The way it does it is just with equally spaced hues around the color wheel, starting from 15:
gg_color_hue <- function(n){
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
# we have 6 experiments (so we want to create our own 6 colour "mini-palette")
cols <- gg_color_hue(6)  
# this gets the default ggplot colours when we want to plot 6 different variables.


# 1st, create an object with the ggplot 
# It has all the data simply as points, shaped and colour-coded by expt. of origin (as above). 
# We store it in an object called 'x':
x <- ggplot(data = melted_data, aes(x = Sub, y = v)) +
  geom_point(aes(colour = Exp, shape = Exp)) +
  theme_few() +
  ylab("velocity (nmol/s)") + xlab("substrate (mM)") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        plot.title = element_blank(),            # we don't want individual plot titles as the facet "strip" will give us this
        legend.position = "none",                # we don't want a legend either
        panel.border = element_rect(fill = NA, color = "darkgrey", size = 1.25, linetype = "solid"),
        axis.ticks = element_line(colour = 'darkgrey', size = 1.25, linetype = 'solid'))     # here, I just alter to colour and thickness of the plot outline and tick marks. You generally have to do this when faceting, as well as alter the text sizes (= element_text() in theme also)

# have a quick look at x
x
# see all the data in different colours

# Next, let's modify 'x' by faceting based on the Experiment Name, 
# We also specify that we want a panel 3 plots wide x 2 plots high.
# You can change this, obviously. Can also 'facet_grid' too:
x <- x + facet_wrap( ~ Exp, ncol = 3)

# Finally, let's apply the Michaelis Menten fitting to our faceted data, adding the best fit line in each case:
x <- x + geom_smooth(method = "nls",                 
                     method.args = list(formula = y ~ Vmax * x / (Km + x), 
                                        start = list(Vmax = 50, Km = 2)),
                     se = F, colour = 'black', size = 0.5)

# Finally, just call 'x' to show the plots:
x

# if you want to save it as a .pdf, just add + ggsave("GGPLOT_FACET_EnzKin.pdf"), when you call 'x'
x + ggsave("GGPLOT_FACET_EnzKin.pdf")

