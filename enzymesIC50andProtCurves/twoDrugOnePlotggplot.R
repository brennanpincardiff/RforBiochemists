###  I have detailed the code separately for each set of data
###  to make it more obvious what I'm doing. 
### The code could be made shorter and less repetitive.

###  This is the data   ###
# Data from multiple experiments

conc <- c(1.00E-08, 1.00E-07, 5.00E-07, 1.00E-06, 1.00E-05, 
          1.00E-08, 1.00E-07, 5.00E-07, 1.00E-06, 1.00E-05, 
          1.00E-08, 1.00E-07, 5.00E-07, 1.00E-06, 1.00E-05, 
          1.00E-07, 5.00E-07, 1.00E-06, 5.00E-06, 1.00E-05, 2.00E-05, 
          1.00E-07, 5.00E-07, 1.00E-06, 5.00E-06, 1.00E-05, 2.00E-05, 
          1.00E-07, 5.00E-07, 1.00E-06, 5.00E-06, 1.00E-05, 2.00E-05, 
          1.00E-07, 5.00E-07, 1.00E-06, 2.50E-06, 5.00E-06, 1.00E-05, 
          1.00E-07, 5.00E-07, 1.00E-06, 2.50E-06, 5.00E-06, 1.00E-05, 
          1.00E-07, 5.00E-07, 1.00E-06, 2.50E-06, 5.00E-06, 1.00E-05, 
          1.00E-07, 5.00E-07, 1.00E-06, 2.50E-06, 5.00E-06, 1.00E-05) 

# First drug is called Drug 49
dead.cells49 <- c(28.6, 30.1, 34.6, 47.7, 81.7, 
                  27.1, 27.2, 35, 47.9, 80.6, 
                  27.8, 27.9, 34.2, 47.9, 82.4, 
                  31.1, 37.6, 55.7, 89.1, 84.3, 85.2, 
                  30.8, 35.6, 55.9, 90.5, 85.2, 86.3, 
                  32.6, 34.7, 54.1, 89.8, 84.2, 87.1, 
                  32.2, 34.4, 46.1, 76.2, 84.3, 84.1, 
                  31.4, 37.4, 50.5, 75, 85.3, 84.8, 
                  27.6, 37.3, 46.3, 74.1, 81.6, 82.7, 
                  26.7, 28.4, 27.9, 59.6, 87, 86.1)

# Second Drug is called Drug 11
dead.cells11 <- c(28.4, 28.9, 28.6, 30.1, 90, 
                  27.8, 28.5, 29.2, 29.3, 89.9, 
                  28.6, 26.6, 27.3, 29, 89.7, 
                  25.8, 28.7, 31, 80.7, 85, 85.2, 
                  25.3, 28.2, 30, 81.4, 83.5, 87.2, 
                  25.9, 25.4, 28.4, 81.2, 81.4, 85.6, 
                  36.4, 36.2, 33.4, 52.3, 87.2, 97, 
                  33.9, 29.5, 32, 44.8, 84.9, 97.2, 
                  30.1, 30.1, 31, 46.5, 83.5, 96.1, 
                  22.8, 23.7, 25.8, 37.9, 73.8, 86.7) 

# Put the data into data frames and 
# transform the data to make it postive for fitting
data49 <- data.frame(conc,dead.cells49)
data49$nM <- data49$conc * 1000000000
data49$log.nM <- log10(data49$nM) 

data11 <- data.frame(conc,dead.cells11)
data11$nM <- data11$conc * 1000000000
data11$log.nM <- log10(data11$nM) 


# transform the data to make it postive and put into a data frame for fitting 
data <- as.data.frame(conc)   # create the data frame
data$drug11 <- dead.cells11
data$drug49 <- dead.cells49
data$nM <- data$conc * 1000000000
data$log.nM <- log10(data$nM) 

data.sub <- data
data.sub <- data.sub[2:5]
data.sub <- data.sub[-3]
library(reshape2)

# melt the data from wide to long:
melted_data <- melt(data, id.vars = "conc", value.name = "v", variable.name = "Exp")
melted_data <- melt(data.sub, id.vars = "log.nM")


##################  fit the data  ##################
# make sure logconc remains positive, otherwise multiply to keep positive values
# (such as in this example where the iconc is multiplied by 1000

fit49 <- nls(dead.cells49 ~ bot+(top-bot)/(1+(log.nM/LD50)^slope),
             data = data49,
             start=list(bot=20, top=95, LD50=4, slope=-12))

fit11 <- nls(dead.cells11 ~ bot+(top-bot)/(1+(log.nM/LD50)^slope),
             data = data11,
             start=list(bot=20, top=95, LD50=4, slope=-12))


##################  Plot the results  ##################
#this lets you graph your calculated equations nice and pretty
x <- seq(0,6, length=100)

y49 <- (coef(fit49)["bot"]+(coef(fit49)["top"]-coef(fit49)["bot"])/(1+(x/coef(fit49)["LD50"])^coef(fit49)["slope"]))
m49 <- coef(fit49)  #this extracts the fitted LD50 for drug 49

y11 <- (coef(fit11)["bot"]+(coef(fit11)["top"]-coef(fit11)["bot"])/(1+(x/coef(fit11)["LD50"])^coef(fit11)["slope"]))
m11 <- coef(fit11)



# plot the first set of points
plot(data49$nM, data49$dead.cells49, 
     log="x", 
     main="Comparing LD50 of two drugs ", 
     xlab="Drug concentration (microM)", 
     ylab="Dead cells (% of cells)",
     xlim= c(500, 10000),
     ylim= c(20,100),
     xaxt = "n")   # suppresses the labels on the x-axis

# add the points for the second set of data
points(data11$nM, data11$dead.cells11, col = "red")

# add the axis they way we want it to look
axis(1, at=c(500, 1000, 2500, 5000, 10000),  lab=c("0.5","1","2.5","5","10"))

# adds the two non-linear fitted lines 
lines(10^x,y49, lty="dotted", col="black")
lines(10^x,y11, lty="dotted", col="red")

# adds the legend
legend("topleft", 
       inset=.02,
       c("Drug 49","Drug 11"),
       lty=c("dotted","dotted"),
       lwd=c(2.5,2.5),
       col=c("black","red"),
       cex = 0.75)

# add the LD50s in the legend which allows nice positioning. 
rp = vector('expression',2)
rp[1] = substitute(expression(Drug49-LD50(microM) == MYVALUE), 
                   list(MYVALUE = format((10^m49[3])/1000,dig=3)))[2]
rp[2] = substitute(expression(Drug11-LD50(microM) == MYVALUE), 
                   list(MYVALUE = format((10^m11[3])/1000,dig=3)))[2]
legend('bottomright', legend = rp, bty = 'n', cex = 0.75)





###  ggplot the results  ###
p <- ggplot(data=melted_data,          # specify the data frame with data
            aes(x=log.nM, 
                y=value,
                colour = variable)) +   # specify x and y
  geom_point(aes(colour = variable, shape = variable))  # make a scatter plot
  

# Add the line to graph using methods.args (New: Jan 2016)
p <- p +  geom_smooth(method = "nls", 
                      method.args = list(formula = y ~ bot+(top-bot)/(1+( x / LD50)^slope), 
                                         start=list(bot=20, top=95, LD50=3, slope=-12)),
                      se = F, size = 0.5,
                      data = subset(melted_data, variable=="drug49"))

p <- p +  geom_smooth(method = "nls", 
                      method.args = list(formula = y ~ bot+(top-bot)/(1+( x / LD50)^slope), 
                                         start=list(bot=20, top=95, LD50=4, slope=-12)),
                      se = F, size = 0.5,
                      data = subset(melted_data, variable=="drug11"))


p + scale_x_log10(breaks = c(500, 1000, 2500, 5000, 10000, 20000))
p +  xlab("Drug concentration (nM)") +   # label x-axis
  ylab("Dead cells (% of cells)") +    # label y-axis
  ggtitle("Drug Dose Response and LD50") +  # add a title
  theme_bw() +      # a simple theme
  expand_limits(y=c(20,100))   # customise the y-axis






# Add the text with the LD50 to the graph. 
p <- p+ annotate(geom="text", x=7000, y= 60, label="LD50(nM): ",  color="red") +
  annotate(geom="text", x=9800, y= 60, label=val,  color="red")

p # show the plot


p + 


  enz.plot  + geom_smooth(method = "nls", 
                          formula = y ~ Vmax * x / (Km + x), 
                          start = list(Vmax = 50, Km = 0.2),
                          se = F, size = 0.5, 
                          data = subset(enzdata, Enz=="WT")) +
  geom_smooth(method = "nls", 
              formula = y ~ Vmax * x / (Km + x), 
              start = list(Vmax = 50, Km = 0.2),
              se = F, size = 0.5, 
              data = subset(enzdata, Enz=="H297F")) +
  theme_few()




