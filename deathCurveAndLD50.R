###  this is the data   ### 
# data from four experiments
conc <- c(5.00E-07, 1.00E-06, 1.00E-05, 
          5.00E-07, 1.00E-06, 5.00E-06, 1.00E-05, 2.00E-05, 
          5.00E-07, 1.00E-06, 2.50E-06, 5.00E-06, 1.00E-05, 
          5.00E-07, 1.00E-06, 2.50E-06, 5.00E-06, 1.00E-05)
dead.cells <- c(34.6, 47.7, 81.7, 
                37.6, 55.7, 89.1, 84.3, 85.2, 
                34.4, 46.1, 76.2, 84.3, 84.1, 
                24.5, 26.1, 60.6, 82.7, 87)

# transform the data to make it postive and put into a data frame for fitting 
data <- as.data.frame(conc)   # create the data frame
data$dead.cells <- dead.cells
data$nM <- data$conc * 1000000000
data$log.nM <- log10(data$nM) 

###  fit the data  ###
# make sure logconc remains positive, otherwise multiply to keep positive values
# (such as in this example where the iconc is multiplied by 1000

fit <- nls(dead.cells ~ bot+(top-bot)/(1+(log.nM/LD50)^slope),
             data = data,
             start=list(bot=20, top=95, LD50=3, slope=-12))

###  Plot the results  ###
#this lets you graph your calculated equations nice and pretty

x <- seq(0,6, length=100)
y <- (coef(fit)["bot"]+(coef(fit)["top"]-coef(fit)["bot"])/(1+(x/coef(fit)["LD50"])^coef(fit)["slope"]))
m <- coef(fit)

# plot the points first
plot(data$nM, data$dead.cells, 
     log="x", 
     main="Drug Dose Response and LD50", 
     xlab="Drug concentration (microM)", 
     ylab="Dead cells (% of cells)",
     xlim= c(500, 10000),
     ylim= c(20,100),
     xaxt = "n")   # suppresses the labels on the x-axis

# adds the axis they way I want it to look
axis(1, at=c(500, 1000, 2500, 5000, 10000),  lab=c("0.5","1","2.5","5","10"))

# adds the fitted non-linear line 
lines(10^x,y, lty="dotted", col="red", lwd =2)

# add the LD50 in the legend which allows nice positioning. 
rp = vector('expression',1)
rp[1] = substitute(expression(LD50(microM) == MYVALUE), 
                   list(MYVALUE = format((10^m[3])/1000,dig=3)))[2]
legend('topleft', legend = rp, bty = 'n')






