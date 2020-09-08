## Code for module 1

prot_conc <- c(0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000, 
               0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000) 

abs <- c(0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956, 
         0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905)

plot(prot_conc, abs)
 
install.packages("ggplot2") 
# don't want to run everytime. 
library(ggplot2)

# ggplot2 requires us to reformat the data into a data.frame
data <- as.data.frame(prot_conc)

# the str() function allows us to find out about the structure of the data.
str(data)

data$abs <- abs

View(data)

### Make a plot with ggplot2

## Step 1: add the data and then the 'aesthetics'
  
p <- ggplot(data=data,          # specify the data frame with data
            aes(x=prot_conc, y=abs)) # specify x and y for the graph
p  # show the plot

## Step 2: add a type of graph
p <- p + geom_point()

p # show the plot again

# shows the graph but doesn't modify it
p + stat_smooth(method = "lm", formula = y ~ x)

p # show the plot - no line...

# add some labels. 
p +  xlab("[Protein] (microg/ml)") +   # label x-axis
  ylab("Absorbance (570nm)") +      # label y-axis
  ggtitle("Protein Assay")          # add a title

p <- p + stat_smooth(method = "lm", formula = y ~ x) +
         xlab("[Protein] (microg/ml)") +   # label x-axis
         ylab("Absorbance (570nm)") +      # label y-axis
         ggtitle("Protein Assay")          # add a title

p

# save your output wtih Export or
p + ggsave("ProteinStandardCurve.pdf")


