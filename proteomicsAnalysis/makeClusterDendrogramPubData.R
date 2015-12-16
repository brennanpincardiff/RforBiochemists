# import the data
data2 <- read.csv("http://science2therapy.com/data/iTRAQPatientforCluster.csv", header=TRUE)
attach(data2)
head(data2)

# calculate the distances
distances2 <- dist(rbind(P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12))
distances2
summary(distances2)

# make the cluster dendrogram object
hc <- hclust(distances2)

# plot the object
plot(hc,
     xlab=expression(bold("Patient Samples")), 
     ylab = expression(bold("Distance")))

# save the file in png format  suitable for publication
ppi=600    #pixels per square inch
png("cluster_20140602.png", width=6*ppi, height=5*ppi, res=ppi)
plot(hc,xlab=expression(bold("Patient Samples")), ylab = expression(bold("Distance")))
dev.off()
