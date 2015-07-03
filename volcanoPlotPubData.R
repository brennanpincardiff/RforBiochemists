library(ggplot2)

data <- read.csv("http://science2therapy.com/data/mcp.M114.044479.csv", header=TRUE)

##Identify the genes that have a p-value < 0.05
data$threshold = as.factor(data$P.Value < 0.05)

##Construct the plot object
g <- ggplot(data=data, 
            aes(x=Log2.Fold.Change, y =-log10(P.Value), 
            colour=threshold)) +
  geom_point(alpha=0.4, size=1.75) +
  xlim(c(-6, 6)) +
  xlab("log2 fold change") + ylab("-log10 p-value") +
  theme_bw() +
  theme(legend.position="none")

g
# The script gives a warning message: Removed 1 rows containing missing values (geom_point).
# but it still works....
