###  this is the data   ###
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

x <- seq(0,4, length=100)
y <- (coef(fit)["bot"]+(coef(fit)["top"]-coef(fit)["bot"])/(1+(x/coef(fit)["LD50"])^coef(fit)["slope"]))
m <- coef(fit)


#### THESE ARE THE SCRIPTS THAT WERE USED TO GENERATE THE GRAPHS

# Default Code:
plot(data$nM, data$dead.cells, 
    )

# Add log axis, x and y limits and change the symbols
plot(data$nM, data$dead.cells, 
     log="x", 
     pch=20, # gives small black dots
     xlim= c(500, 10000),
     ylim= c(0,100),
    ) # suppresses the labels on the x-axis

# suppress everything except the position of the points. 
plot(data$nM, data$dead.cells, 
     log="x", 
     pch=20,
     xlim= c(500, 10000),
     ylim= c(0,100),
     axes=FALSE, # suppresses the titles on the axes
     ann=FALSE, # and the box   
  ) 

# adds the axes they way I want them to look
axis(1, at=c(200, 500, 1000, 2500, 5000, 10000), # x-axis 
     lab=c("","0.5","1","2.5","5","10"), 
     lwd=2,  # thicker lines
     pos=0)  # puts the x-axis at zero rather than floating. 
axis(2, at=c(0, 20, 40, 60, 80,100), # y-axis
     lab=c("0", "20", "40", "60", "80", "100"), 
     lwd=2, 
     las=1) # turns the numbers so easier to read

# add new titles 
title("Figure 1A", 
      xlab= expression(paste("Drug concentration (", mu, "M)")), 
      # not the word "micro"
      ylab= "Dead cells (% of cells)")


# add the fitted non-linear line 
lines(10^x,y, lty=1, lwd =2)

# add the LD50 in the legend which allows nice positioning. 
rp = vector('expression',1)
rp[1] = substitute(expression(LD50(microM) == MYVALUE), 
                   list(MYVALUE = format((10^m[3])/1000,dig=3)))[2]
legend('topleft', legend = rp, bty = 'n')



