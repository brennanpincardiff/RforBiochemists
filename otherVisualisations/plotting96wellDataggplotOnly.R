# Import the data
data <- read.csv("http://science2therapy.com/data/wellsDataSimp.csv")

# Have a look at the data
View(data)

library("ggplot2")

# plot wells data with ggplot
ggplot(data = data,
       aes(x = P.Erk, 
           y = S.phase.cnt, 
           color = Virus)) +
       geom_point() +               # key function that tells ggplot to show points
       stat_smooth(method = "lm") +  # add a linear model line
       xlab("ppErk") +               # label x-axis
       ylab("Number of S-phase cells") +    # label y-axis
       ggtitle("Number of S-phase cells Vs pErk staining") +
       theme_bw() +
       theme(legend.position="top")
       