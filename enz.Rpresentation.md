The chemical logic of glycolysis
========================================================
author: Sergio Martínez Cuesta, EMBL-EBI
date: 24th June 2015
rtl: false
width: 1440
height: 900
#autosize: true


Aim
========================================================

"Just as methods for comparing **protein and DNA sequences and structures** have improved our basic understanding of evolution, a method allowing quantitative comparison of **enzyme reactions** should provide a firm foundation from which to explore the evolution of enzyme function and to facilitate the design of new enzymes." (Rahman et al., 2014)

**Learn by example:** Visualise chemical similarities between enzymes reactions in glycolysis

Use R to explore data, run PCA, clustering analysis and plot heatmaps


Glycolysis
========================================================

![alt text](Keller2014_f1_2x.png)


Overview
========================================================

- Data input
- Explore data
- Principal component analysis
- Clustering analysis
- Heatmaps


Set working directory!
========================================================


```r
> getwd()
> setwd("~/Desktop/glycolysis")
```


Data input
========================================================


```r
> data<-read.csv("data/bc.csv", header=TRUE, row.names=1, sep="\t", check.names=FALSE)
```

**bc.csv** contains the frequency counts of chemical bond changes of the 10 reactions in glycolysis

**header=TRUE** and **row.names=1** allow the first row and column to be the column and row names, **sep="\t"** indicates that bc.csv is tab-separated and **check.names=FALSE** disables the check of variable names


Explore data
========================================================

```r
> data
```

Type of object and dimensions:


```r
> class(data)
```


```r
> dim(data)
```

Types of bond changes and glycolysis enzyme IDs:


```r
> colnames(data)
```


```r
> rownames(data)
```


Barplot
========================================================
Which glycolytic enzyme catalyses more bond changes?


```r
> freqs_bc<-sort(rowSums(data), decreasing=TRUE)
> barplot(freqs_bc, col="white", ylab="Frequency of bond changes", xlab="Enzyme name")
```

Which bond changes are more common in glycolysis?


```r
> freqs_enz<-sort(colSums(data), decreasing=TRUE)
> barplot(freqs_enz, col="white", ylab="Frequency of bond changes", xlab="Type of bond change")
```

What is the average number of bond changes by glycolytic reaction?


```r
> mean(rowSums(data))
```


Barplot
========================================================
title:false

![plot of chunk unnamed-chunk-11](Rpresentation-figure/unnamed-chunk-11-1.png) 

***

![plot of chunk unnamed-chunk-12](Rpresentation-figure/unnamed-chunk-12-1.png) 


How can we represent chemical distance between reactions?
========================================================


```r
> print(data)
```


|      | P-O| C-C| C-O| O-H| C-H| C-O<->C=O| C-C<->C=C| C(R/S)|
|:-----|---:|---:|---:|---:|---:|---------:|---------:|------:|
|HK    |   2|   0|   0|   2|   0|         0|         0|      0|
|PGI   |   0|   0|   2|   0|   2|         0|         0|      1|
|PFK-1 |   2|   0|   0|   2|   0|         0|         0|      0|
|ALDO  |   0|   1|   1|   3|   1|         2|         0|      2|
|TPI   |   0|   0|   0|   2|   2|         2|         0|      1|
|GAPDH |   0|   0|   1|   1|   2|         0|         0|      0|
|PGK   |   2|   0|   0|   2|   0|         0|         0|      0|
|PGM   |   2|   0|   0|   2|   0|         0|         0|      0|
|ENO   |   0|   0|   1|   1|   1|         0|         1|      1|
|PK    |   2|   0|   0|   1|   1|         1|         1|      0|


Principal Component Analysis (PCA)
========================================================
Method to discover structure in large multidimensional datasets.


```r
> print(head(mtcars)[,1:6])
```


|                  |  mpg| cyl| disp|  hp| drat|    wt|
|:-----------------|----:|---:|----:|---:|----:|-----:|
|Mazda RX4         | 21.0|   6|  160| 110| 3.90| 2.620|
|Mazda RX4 Wag     | 21.0|   6|  160| 110| 3.90| 2.875|
|Datsun 710        | 22.8|   4|  108|  93| 3.85| 2.320|
|Hornet 4 Drive    | 21.4|   6|  258| 110| 3.08| 3.215|
|Hornet Sportabout | 18.7|   8|  360| 175| 3.15| 3.440|
|Valiant           | 18.1|   6|  225| 105| 2.76| 3.460|


***

Reduce dimensionality

To find a small number of linear combinations of the variables (bond changes) in order to capture most of the variation in the dataset as a whole.

![alt text](pca.gif)


How can we represent chemical distance between reactions?
========================================================


```r
> print(data)
```


|      | P-O| C-C| C-O| O-H| C-H| C-O<->C=O| C-C<->C=C| C(R/S)|
|:-----|---:|---:|---:|---:|---:|---------:|---------:|------:|
|HK    |   2|   0|   0|   2|   0|         0|         0|      0|
|PGI   |   0|   0|   2|   0|   2|         0|         0|      1|
|PFK-1 |   2|   0|   0|   2|   0|         0|         0|      0|
|ALDO  |   0|   1|   1|   3|   1|         2|         0|      2|
|TPI   |   0|   0|   0|   2|   2|         2|         0|      1|
|GAPDH |   0|   0|   1|   1|   2|         0|         0|      0|
|PGK   |   2|   0|   0|   2|   0|         0|         0|      0|
|PGM   |   2|   0|   0|   2|   0|         0|         0|      0|
|ENO   |   0|   0|   1|   1|   1|         0|         1|      1|
|PK    |   2|   0|   0|   1|   1|         1|         1|      0|


Principal Component Analysis (PCA)
========================================================


```r
> fit <- princomp(data)
```

Check variance explained by top components:


```r
> summary(fit)
```

Plot first two components:


```r
> biplot(fit, main="PCA")
```

How do bond changes relate to PCA components?


```r
> loadings(fit)
```

How do enzymes relate to PCA components?


```r
> fit$scores
```


Principal Component Analysis (PCA)
========================================================
title:false

![plot of chunk unnamed-chunk-24](Rpresentation-figure/unnamed-chunk-24-1.png) 


Hierarchical clustering
========================================================

Group objects with related variables in a tree

E.g. enzyme reactions with similar patterns of bond changes or cars with similar technical features

![alt text](hclust_cut.png)


Chemical distance between glycolysis reactions
========================================================

For example, HK, PFK-1, PGK and PGM catalyse exactly the same bond changes - they are chemically identical. As a result, they are separated by a distance of 0 bond changes.


```r
> d <- dist(data, method = "manhattan")
> print(d)
```


```
      HK PGI PFK-1 ALDO TPI GAPDH PGK PGM ENO
PGI    9                                     
PFK-1  0   9                                 
ALDO  10   9    10                           
TPI    7   6     7    5                      
GAPDH  6   3     6    8   5                  
PGK    0   9     0   10   7     6            
PGM    0   9     0   10   7     6   0        
ENO    7   4     7    7   6     3   7   7    
PK     4   9     4   10   7     6   4   4   5
```


Hierarchical clustering
========================================================

We need the distance matrix in order to perform hierarchical clustering.


```r
> fit <- hclust(d, method="ward")
```

Plot hierarchical tree:


```r
> plot(fit)
```

Define clusters:


```r
> groups <- cutree(fit, k=4)
```

Plot clusters:


```r
> rect.hclust(fit, k=4, border="red") 
```


Hierarchical clustering
========================================================
title:false

![plot of chunk unnamed-chunk-31](Rpresentation-figure/unnamed-chunk-31-1.png) 

***

![alt text](Keller2014_f1_2x.png)


Heatmaps
========================================================

Visualise hierarchical clustering

![alt text](heatmap.png)


Heatmaps
========================================================
Display frequencies in the input matrix using colours

Reactions vs. bond changes:


```r
> heatmap(as.matrix(data))
```

Just reactions:


```r
> heatmap(as.matrix(d))
```


Heatmaps
========================================================
title:false

![plot of chunk unnamed-chunk-34](Rpresentation-figure/unnamed-chunk-34-1.png) 

***

![plot of chunk unnamed-chunk-35](Rpresentation-figure/unnamed-chunk-35-1.png) 


Pretty heatmaps
========================================================

Install and load pheatmap package:


```r
> install.packages("pheatmap")
> library(pheatmap)
```

Run pheatmap:


```r
> pheatmap(data, fontsize=13, cellwidth = 15, cellheight = 15, color = colorRampPalette(c("white", "red"))(4), clustering_distance_rows=d, legend_breaks=c(0,1,2,3))
```

**fontsize**, **cellwidth** and **cellheight** defines the size of the font and cell, **color** selects four colours from white to red, **clustering_distance_rows** uses the distance matrix calculated previously and **legend_breaks** defines the numbers in the legend.


Pretty heatmaps
========================================================
title:false

![plot of chunk unnamed-chunk-38](Rpresentation-figure/unnamed-chunk-38-1.png) 


References
========================================================

1- Quick-R: http://www.statmethods.net/

2- Crawley, M. J. (2007). The R Book. Chichester, UK: John Wiley & Sons, Ltd. doi:10.1002/9780470515075

3- Husson, F., Lê, S., and Pagès, J. (2011) Exploratory multivariate analysis by example using R, [online] http://books.google.com/books?hl=en&lr=&id=EDnNBQAAQBAJ&oi=fnd&pg=PP1&dq=Exploratory+Multivariate+Analysis+by+Example+Using+R&ots=I4bnnmgPJw&sig=ENW39pnO00fOOSO_F01cQ_Iv-UA (Accessed February 6, 2015).

4- Rahman, S. A., Martínez Cuesta, S., Furnham, N., Holliday, G. L., & Thornton, J. M. (2014). EC-BLAST: a tool to automatically search and compare enzyme reactions. Nature methods, 11(2), 171–174. doi:10.1038/nmeth.2803. url: http://www.ebi.ac.uk/thornton-srv/software/rbl/

5- Keller, M. A., Turchyn, A. V., & Ralser, M. (2014). Non-enzymatic glycolysis and pentose phosphate pathway-like reactions in a plausible Archean ocean. Molecular systems biology, 10, 725. Retrieved from http://www.ncbi.nlm.nih.gov/pubmed/24771084



Thanks!
========================================================
scuesta@ebi.ac.uk
