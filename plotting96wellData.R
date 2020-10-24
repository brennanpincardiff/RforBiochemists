# Import the data
data <- read.csv("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/wellsDataSimp.csv")

# Have a quick look at it
View(data)

# Simple plot
plot(data[5:7])

# Install and enable two graphics packages
install.packages("lattice")
install.packages("ggplot2")
library("lattice")
library("ggplot2")


# Plot the data in the lattice package first. 

#plot ppErk against Number of S-phase cells. 
# Colour the plots by presence of FBS (0 or 10%)
xyplot(data$S.phase.cnt ~ data$P.Erk,
       group=data$FBS,
       main="Number of S-phase cells Vs pErk staining",
       xlab ="Phospho-Erk",
       ylab = "S-phase cell count",
       auto.key=list(space="top", columns=2, 
                     title="FBS", cex.title=1))

# Colour the plots by presence of PPase in host cell
xyplot(data$S.phase.cnt ~ data$P.Erk,
       group=data$Host.cell,
       main="Number of S-phase cells Vs pErk staining",
       xlab ="Phospho-Erk",
       ylab = "S-phase cell count",
       auto.key=list(space="top", columns=2, 
                     title="Host Cell", cex.title=1))

# Colour the plots by presence of the virus (no virus = Ctl, Q61 = ras mutant, V600F = raf mutant)
xyplot(data$S.phase.cnt ~ data$P.Erk,
       group=data$Virus,
       main="Number of S-phase cells Vs pErk staining",
       xlab ="Phospho-Erk",
       ylab = "S-phase cell count",
       auto.key=list(space="top", columns=3, 
                     title="Virus", cex.title=1))


# plot wells data with ggplot
qplot(data$P.Erk, data$S.phase.cnt, data=data, geom=c("point", "smooth"), 
      method="lm", formula=y~x, color=data$Virus, 
      main="Number of S-phase cells Vs pErk staining", 
      xlab="ppErk", ylab="Number of S-phase cells")
