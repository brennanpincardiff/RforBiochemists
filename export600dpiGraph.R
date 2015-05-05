# Publication quality graphs require 600dpi
dpi=600    #pixels per square inch
tiff("output.tif", width=6*dpi, height=5*dpi, res=dpi)  # key function
#the tiff() function creates the output file and adds information about size and resolution 

# data
prot <- c(0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000, 0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000) 
abs <- c(0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956, 0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905)

#draw the graph:
plot(abs~prot, 
     xlab = "[Protein] (microg/ml)",
     ylab = "Absorbance (570nm)",
     main = "Protein Assay")

line <- lm(abs~prot) # Calculate the line using the linear model function
abline(line) # Draw the line

r2 <- round(summary(line)$r.squared, 3)
mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
text(x = 0.2, y = 0.9, labels = mylabel)

dev.off()  #very important command that tells R that you are finished plotting
# otherwise your graph will not show up.