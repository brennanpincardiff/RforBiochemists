# As a biochemist, one of the first data types we play with is enzymology data. 
# Here is an example of some enzyme simple kinetic data
# Eight values for substrate concentration with corresponding enzyme velocities. 
# The goals are to plot the data, in different formats, and also, find the best fit values of Km and Vmax
# all of this is complete in base R, so no packages should be needed

# Entered as vectors 
S <- c(0,1,2,5,8,12,30,50)
v <- c(0,11.1,25.4,44.8,54.5,58.2,72.0,60.1)

# simple plot, rather ugly
plot (S,v)

#  A better plot: Michaelis Menten hyperbola
plot (S,v, 
      xlab="Subtrate (mM)", 
      ylab="Velocity (nmol/s)", 
      main="Michaelis-Menten", 
      pch=17, col="red")

# Tranform the data to plot a Lineweaver-Burk plot (1/v as a function of 1/S)
# This plot is not recommended becuase it distorts the error structure of the data
plot (1/S,1/v, 
      xlab="Subtrate (mM)", 
      ylab="Velocity (nmol/s)", 
      main="Lineweaver-Burk", 
      pch=17, col="blue", cex=1.5)

# Tranform the data to plot a Eadie-Hostee plot (v as a function of v/S)
plot (v,v/S, 
      xlab="velocity/Subtrate(nmol.s-1/mM)", 
      ylab="Velocity (nmol.s-1)", 
      main="Eadie-Hofstee", 
      pch=17, col="blue", cex=1.5)

# We can also build a simple data frame from (S,v) data
kinData <- data.frame(S,v)

# simple plot of the dataframe
plot (kinData)

# Store some colours as variables for later
c1<-"red"
c2<-"blue"

# More complex plot, adding axis labels, changing symbol and colour
# Simple Michaelis-Menten plot
plot (kinData, 
      xlab="Subtrate (mM)", 
      ylab="Velocity (nmol/s)", 
      pch=17, col=c1, cex=2)

# And this shows how simple it is to plot a Lineweaver-Burk plot 
# (1/v as a function of 1/S)
plot (1/kinData, 
      xlab="1/Subtrate (mM)", 
      ylab="1/Velocity (nmol/s)", 
      pch=17, col=c1, cex=2)

# Next step - how do we get the values of Km and Vmax?

# This is the theoretical formula
# "velocity = Vmax times S divided by (Km plus S)", stored in MMcurve
MMcurve<-formula(v~Vmax*S/(Km+S))

# nls is non-linear least squares optimiser.
# If you're not sure why a Michael-Menten equation is non-linear, ask
# (Hint: it is not because it is a curve when plotted!)

# start sets the initial guess - do not have to be very good
# should be possible to make reasonable guesses 
# from a quick inspection - this is why we visualise the data first
bestfit <- nls(MMcurve, kinData, start=list(Vmax=50,Km=2))

# Build a theoretical line defining the best fit curve
# First, make a finely detailed set of points between 0 and 50, at 0.1 intervals
# These will be the substrate concentrations that are used to calculate 
# the predicted velocities
SconcRange <- seq(0,50,0.1)

# Then, calculate the predicted velocities using the predict function
theorLine <- predict(bestfit,list(S=SconcRange))

# Best fit values of Km and Vmax obtained by coef function, stored in bestFitVals
bestFitVals <- coef(bestfit)

# Now plot the data, the best fit line, and put the best fit coefficients in the plot
plot (kinData, 
      xlab="Subtrate (mM)", 
      ylab="Velocity (nmol/s)", 
      title(main="Fitted MM data"), 
      pch=17, col=c2, cex=2)

# Draw the line
# lines() function adds to the existing plot as does points()
# here, we want a line of best fit, not points, so we use lines()
lines(SconcRange,theorLine,col=c1)

# Add text with the calculated values
# This is a fudge, there must be better ways, also adding errors on parameter values.

text(30,30, "Vmax=")
text(35,30,round(bestFitVals[1],2))
text(30,27, "Km=")
text(35,27,round(bestFitVals[2],2))

# END OF SCRIPT










