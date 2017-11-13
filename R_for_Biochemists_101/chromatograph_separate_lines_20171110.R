library(readxl)
library(ggplot2)

# Hatziioanou, D, Mayer MJ, Duncan, SH, Flint, HJ & Arjan Narbad, A 
# (2013) Anaerobe 23:5-8
# http://dx.doi.org/10.1016/j.anaerobe.2013.07.006

link <- "https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/20100212roseburiab.xls"
download.file(url=link, destfile="file.xls", mode="wb")

data_2 <- read_excel("file.xls", skip = 2)

colnames(data_2) <- c("vol_ml", "UV_mAU", "Cond_ml_11_1", "conduct_mS_cm",
  "Cond_11_%_ml", "percent_%", "Conc_11_ml", "percentB_1", "percentB_2",
  "Pressure_ml", "Pressure_ MPa", "Flow_ml", "Flow_ml_min", "Temp_ml", 
  "Temp_C")

# published data is column 2 vs column 7
ggplot(data_2, aes(x = Conc_11_ml, y = UV_mAU)) + geom_line()
# Warning message - missing values

# row 602 gives a volume of almost 20 mls so limit the plot to that
data_2[602,7]
# 19.993

# subset the data - just 602 rows
ggplot(data_2[1:602,], aes(x = Conc_11_ml, y = UV_mAU)) + geom_line()
# output looks good 
# very much like in the paper. 

# manual overlay and text in lots of layers to create a nice graph
# make a canvas and set the x axis
p <- ggplot(data_2[1:602,], aes(x = Conc_11_ml)) 

# add our first line with y = UV which is a measure of protein 
p <- p + geom_line(aes(y = UV_mAU)) 
p

# add our second line with %B - this is a chromatography concept
# we change the scale of the line by multiplying by 10
# define colour as red
p <- p + geom_line(aes(y = percentB_2*10), colour = "red") 
p

# add conductivity as our third line in blue
# we change the scale of the line by multiplying by 4
# define colour as blue
p <- p + geom_line(aes(y = conduct_mS_cm*4), colour = "blue")
p

# add coloured text to explain the lines
# x and y define the places the text arrive and label gives the text
p <- p +  geom_text(mapping = aes(x = 2, y = 400, label = "UV (mAU)"))
p <- p + geom_text(mapping = aes(x = 4, y = 50, label = "0% B"), colour = "red")
p <- p + geom_text(mapping = aes(x = 17.5, y = 1050, label = "100% B"), colour = "red")
p <- p + geom_text(mapping = aes(x = 7, y = 950, label = "Conductivity (mS/cm)"), colour = "blue")
p

# this gives the secondary axis on the right hand side
# the function sec_axis() with a correction factor to change the number
# in this case the correction factor is "/4" meaning divide by four
# we multiplied by four when we plotted the blue line. 
# we also give the scale a name - "Conductivity" and units. 
p <- p +   scale_y_continuous(sec.axis = sec_axis(~./4, 
    name="Conductivity (mS/cm)"))
p


# add a theme to change the background
p <- p + theme_bw()
p

# add labels, titles with a source for the data...
p <- p + labs(x = "Volume (ml)",
    y = "[Protein] (UV mAU)",
    title = "Chromatogram of FPLC purification",
    subtitle = "Source: Hatziioanou et al, (2013) Anaerobe 23:5e8")
p


