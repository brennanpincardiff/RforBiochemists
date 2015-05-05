# Publication quality graphs require 600dpi
ppi=600    #pixels per square inch
png("output.png", width=6*ppi, height=5*ppi, res=ppi)  
#this creates the output file and adds information about size and resolution 

dev.off()