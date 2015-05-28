# Following on from Prof. Beynon's example with enzymatic data... 
# For multiplotting 6 enzymology data-sets, using base R 
# Data from six experiments

no.Exp <- c("Exp 1","Exp 2", "Exp 3", "Exp 4","Exp 5", "Exp 6")

# Substrate concentrations:
Sub <- c(0, 1, 2, 4, 8, 12, 16, 20, 30, 40)

# Data in a matrix - 2D object with data of the same class (numeric)
enzdata <- matrix(c(0, 17.36667, 31.97143, 52.68889, 61.95385, 74.2, 77.97143, 84.28, 99.91429, 93.66667, 
                    0, 15.7, 29.42286, 45.64, 62.60615, 75.78118, 69.88, 75.256, 89.59429, 86.84, 
                    0, 27.10667, 42.12, 63.48, 69.56, 74.26857, 79.44444, 83.29091, 87.1, 82.08571, 
                    0, 24.72, 39.07, 47.4, 57.928, 67.6, 71.35556, 67, 75.79375, 70.86667, 
                    0, 5.723636, 11.48, 17.697143, 28.813333, 37.567273, 42.483077, 40.68, 52.81, 56.92, 
                    0, 2.190476, 5.254545, 8.95, 15.628571, 20.8, 25.355556, 26.55, 32.44, 33.333333),
                  nrow=10,
                  ncol=6)

# specify plotting parameters for our multiplot page:
par(mfrow = c(3, 2),   # 6 plots in a 2 column x 3 row format
    oma = c(1,1,1,1),  # oma = outer margin in lines, of each plots (bottom, left, top, right)
    mar = c(3,3,2,1),  # mar = no. of lines to be specified on the four sides of each plot
    cex.main = 0.9,    # main text size
    las = 1)           # all axis labels horizontal


# here's a for loop 
# to plot the data for each enzymatic reaction (Expt), 
# get the values of Km and Vmax from the theoretical formula. 
# Then, build a theoretical line defining the best fit curve.
for(i in 1:length(no.Exp)){   # for every experiment - one col
  v <- enzdata[, i]          # get the velocity ('v') data
  data <- cbind(Sub, v)      # create a data-set for each expt  
  fit <- nls(v ~ Vmax * (Sub / (Km + Sub)),
             start = list(Vmax = 50, Km = 2))
  
  # write a title for each peptide plot, based on colnames: 
  title = paste(no.Exp[i])    # use exp name as a plot title
  
  SconcRange <- seq(0, 50, 0.1)
  theorLine <- predict(fit, list(Sub = SconcRange))
  
  # draw each plot, with points, 
  #applying the correct title adjusting font sizes accordingly:
  plot(Sub, v, main = title,
       col = 'red', pch = 16,
       cex.lab = 0.6, cex.axis = 0.8,
       xlab = NA, ylab = NA)             # omit drawing axis labels
  
  # add the theoretical lines: 
  lines(SconcRange, theorLine, lwd = 1.5, col = 'blue')
}  # this curly bracket is the end of the for loop. 

# use mtext to add single x- and y-axis labels for all plots, close to the margin:
mtext("Velocity (nmol/s)", side = 2, las = 0, outer = TRUE, line = -1)
mtext("Substrate (mM)", side = 1, outer = TRUE, line = -0.5)