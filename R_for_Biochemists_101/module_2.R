# Protein Concentrations
data <- as.data.frame(c(0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000, 
                        0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000))
colnames(data) <- c("prot_conc")

# Absorbances
data$abs <- c(0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956,
              0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905)

# Generate a simple plot
plot(data$prot_conc,      # x-axis
     data$abs,            # y-axis
     pch=19)        # use big black circles

# We will use the linear model function, lm()
# Calculate the line using lm()
line <- lm(data$abs ~ data$prot_conc)

abline(line,         
       col = "red",  # provides the colour of the line
       lwd = 2)      # provides the weight of the line 

# show some information about the object
line

# show more details of the object
summary(line)

# look at the structure of this object with str() function
str(line)

# view the details of the object using $ sign:
line$coefficients

#view the details of the object using a number:
line[1]  # because the first element of the list is the coefficients

line[2]

# extract intercept and slope from our line object
int <- line$coefficients[1]

int

slope <- line$coefficients[2]


# now calculate some unknown protein concs from absorbances
# put the unknowns into a vector
absUkns <- c(0.554, 0.568, 0.705)

protUkns <- (absUkns - int)/slope

protUkns


# improve the graph...
library(ggplot2)

# create the object with the graph in it. 
p <- ggplot(data=data,          # specify the data frame with data
            aes(x=prot_conc, y=abs)) +   # specify the x and y for the graph
  geom_point() +          # make a scatter plot
  stat_smooth(method = "lm", formula = y ~ x) +  # add a linear model line
  xlab("[Protein] (microg/ml)") +   # label x-axis
  ylab("Absorbance (570nm)") +    # label y-axis
  ggtitle("Protein Assay")  # add a title

p # show the plot

# Apply a themes

p + theme_bw()    # a simple theme

p + theme_classic()    # a theme with just lines and no grid...


# More themes are available with the ggthemes package

# install.packages("ggthemes") # remove hash mark to run, if necessary
library(ggthemes)
p + theme_economist_white()    # a theme with just lines and no grid...

  
# apply this simple theme
p <- p +  theme_bw()      # a simple theme
  
# Customise the y-axis to show zero
p <- p +  expand_limits(y=c(0,1))    # customise the y-axis
p # show the plot

# Add the headings
p <- p +  annotate(geom="text",    # this says add text
                   x=0.85,         # the x position of the text on the graph
                   y= 0.6,         # the y position of the text on the graph
                   label="Abs           Prot",     # the text you want to add
                   color="red")    # the colour of the text


# add values of each sample 
#put the answers on the graph using a for loop
for (i in 1:length(absUkns)){
  p <- p + annotate(geom="text", 
                    x = 0.8, y = (0.6 - i/20), 
                    label=absUkns[i])
  p <- p + annotate(geom="text", 
                    x = 0.92, y = (0.6 - i/20), 
                    label=round(protUkns[i], 3))
}

p # show us the graph...
