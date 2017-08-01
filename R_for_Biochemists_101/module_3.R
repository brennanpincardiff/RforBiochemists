### Module 3, Code Demo 1 illustates this code

#### Create the data in a dataframe 
# This is the data
Enz <- c("WT","WT","WT","WT","WT",
         "WT","WT","WT","WT","WT",
         "WT","WT","WT",
         "H297F","H297F","H297F",
         "H297F","H297F","H297F",
         "H297F","H297F")
S <- c(2.00, 1.00, 0.60, 0.50, 0.40, 
       0.30, 0.20, 0.10, 0.09, 0.08, 
       0.06, 0.04, 0.02, 
       0.05, 0.10, 0.20, 
       0.30, 0.40, 0.50, 
       1.00, 2.00)
v <- c(59.01, 58.29, 54.17, 51.82, 49.76, 
       45.15, 36.88, 26.10, 23.50, 22.26, 
       16.45, 13.67, 6.14, 
       11.8, 19.9, 30.3, 
       36.6, 40.2, 42.1, 
       47.8, 50.0)

# assemble the data into a data.frame
enzdata <- as.data.frame(Enz)
enzdata$S <- S
enzdata$v <- v

View(enzdata)

# this data is in what's usually called a 'long' format...

# let's plot our data
library(ggplot2)
library(ggthemes)
# plot the data with ggplot
# most of the syntax seems relatively easy to understand...
ggplot(data=enzdata,         # give the ggplot() function the data
       aes(x=S,            # data.frame col with values for x-axis
           y=v,            # and the y-axis
           colour = Enz)) +  # colour by WT or H297F **NOTE the "+"  
  geom_point() +             # key function that show points
  xlab("Substrate (mM)") +   # label x-axis
  ylab("Velocity (uM/min/mg.enzyme)") +    # label y-axis
  ggtitle("Glucose Dehydrogenase \n wild type and mutant") 

# a 'new' thing to note is the colour argument in the aes( ) function

# we can create an object with the plot in it... 
enz_plot <- ggplot(data=enzdata,         
                   aes(x=S,            
                       y=v,            
                       colour = Enz)) +  
  geom_point() +            
  xlab("Substrate (mM)") +  
  ylab("Velocity (uM/min/mg.enzyme)") +    
  ggtitle("Glucose Dehydrogenase \n wild type and mutant")

# then we can apply a different theme to change the style of the plot. 
enz_plot + theme_bw()

# I want to introduce the concept of 'faceting'....
# this allows us to separate the plots
# we use the facet_wrap( ) function and the tilda with a title from our data

enz_plot + facet_wrap(~Enz) + theme_bw()    # this function gives two plots

# note there is a title in each plot and the y-axis is the same for both plots. 
# the order is alphabetical but we can change it...
# ... if we re-format the data in our dataframe
enzdata$Enz <- factor(enzdata$Enz, levels = c("WT", "H297F"))
  
# It's best to overwrite our plot... 
enz_plot <- ggplot(data=enzdata,         
                   aes(x=S,            
                       y=v,            
                       colour = Enz)) +  
  geom_point() +            
  xlab("Substrate (mM)") +  
  ylab("Velocity (uM/min/mg.enzyme)") +    
  ggtitle("Glucose Dehydrogenase \n wild type and mutant")

# and then show with the facet_wrap( ) function
enz_plot + facet_wrap(~Enz) + theme_bw()    # this function gives two plots


### That's enough for this code demo...
### Module 3, Code Demo 2 will involve adding lines to our plot

## next we can add the enzyme kinetic lines using the geom_smooth() function
enz_plot_lines <- enz_plot + 
  geom_smooth(data = subset(enzdata, Enz=="WT"),
              method = "nls", 
              method.args = list(formula = y ~ Vmax * x / (Km + x), 
                                 start = list(Vmax = 50, Km = 0.2)),
              se = F, size = 0.5)  + 
  geom_smooth(data = subset(enzdata, Enz=="H297F"),
              method = "nls", 
              method.args = list(formula = y ~ Vmax * x / (Km + x), 
                                 start = list(Vmax = 50, Km = 0.2)),
              se = F, size = 0.5)  +
  theme_few()

enz_plot_lines
# I would like to explain the arguments within the geom_smooth() function 
# first, we subset the data - as we are fitting two separate lines
# then we have a method - we're using nls - non-linear least squares
# then we have method.args (method arguments)
# this provides the formula and start values for the formula 
# then we have the se - set to FALSE - essential for this line
# then we have a size for the line - you can change this...
# and we have applied the theme_few() at the end. 

# we can facet_wrap() this again...
enz_plot_lines + facet_wrap(~Enz)

### Module 3, Code Demo 3 will show how to extract Km and Vmax with the nls() function
#### Get Km and Vmax
# graphical output is an essential step and very useful. 
# However, it's difficult to extract the numbers out of the plot. 
# Thus, it's best to do this separately. 

# we create our equation - based on the Michealis Menton enzymatic equation.
# We do that using the formula() function. 
MMcurve <- formula(v~Vmax*S/(Km+S))

# we generate a subset of our data which is from the "wild type" enzyme using the subset function. 
WT <- subset(enzdata, Enz=="WT")

# Third, we create the fit using the nls() function. 
fitWT <- nls(MMcurve, WT, start=list(Vmax=50,Km=0.2))

# look at the info using the summary() function
summary(fitWT)

# we extract our Km and Vmax constants from the fit. 
# make them look a bit nicer with the round() function
Vmax_WT <- summary(fitWT)$coefficients[1]
Km_WT <- summary(fitWT)$coefficients[2]
paste("Vmax: ", round(Vmax_WT, 1))
paste("Km: ", round(Km_WT, 3))

## Your task - try repeating these steps for the H297F mutant
# and try annotating the values of the graph
# this should build on and embed the skills you've learned in modules 1 and 2

