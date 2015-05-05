###  I have detailed lots of things separately so that it's obvious what I'm doing. 
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

# Put the data into a data frame and transform the data to make it postive for fitting
data49 <- data.frame(conc,dead.cells49)
data49$nM <- data49$conc * 1000000000
data49$log.nM <- log10(data49$nM) 

data11 <- data.frame(conc,dead.cells11)
data11$nM <- data11$conc * 1000000000
data11$log.nM <- log10(data11$nM) 

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
m49 <- coef(fit49)

y11 <- (coef(fit11)["bot"]+(coef(fit11)["top"]-coef(fit11)["bot"])/(1+(x/coef(fit11)["LD50"])^coef(fit11)["slope"]))
m11 <- coef(fit11)

# This is the key bit of code that divides the plot area in two
par(mfrow=c(1,2))

# plot the first graph - goes on the left side
plot(data49$nM, data49$dead.cells49, 
     log="x", 
     main="Drug 49", 
     xlab="Drug concentration (microM)", 
     ylab="Dead cells (% of cells)",
     xlim= c(500, 10000),
     ylim= c(20,100),
     xaxt = "n")   # suppresses the labels on the x-axis

# order here is important
# you have to complete the plot before you start the next one
# adds the axis they way we want it to look
axis(1, at=c(500, 1000, 2500, 5000, 10000),  lab=c("0.5","1","2.5","5","10"))

# adds the non-linear fitted line to the first graph
lines(10^x,y49, lty="dotted", col="blue")

# add the LD50s in the legend which allows nice positioning. 
rp = vector('expression',1)
rp[1] = substitute(expression(LD50(microM) == MYVALUE), 
                   list(MYVALUE = format((10^m49[3])/1000,dig=3)))[2]
legend("topleft", legend = rp, bty = 'n', cex = 0.75)


# plot the second graph - goes on the right side
plot(data11$nM, data11$dead.cells11, 
     log="x", 
     main="Drug 11", 
     xlab="Drug concentration (microM)", 
     ylab="Dead cells (% of cells)",
     xlim= c(500, 10000),
     ylim= c(20,100),
     xaxt = "n")   # suppresses the labels on the x-axis

# adds the axis they way we want it to look
axis(1, at=c(500, 1000, 2500, 5000, 10000),  lab=c("0.5","1","2.5","5","10"))

# adds the non-linear fitted line to the second graph
lines(10^x,y11, lty="dotted", col="blue")

# add the LD50s in the legend which allows nice positioning. 
rp = vector('expression',1)
rp[1] = substitute(expression(LD50(microM) == MYVALUE), 
                   list(MYVALUE = format((10^m11[3])/1000,dig=3)))[2]
legend("topleft", legend = rp, bty = 'n', cex = 0.75)