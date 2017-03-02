# START 
# download the data from github
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/microArrayData.tsv")
data <- read.table(text = x, header = TRUE, sep = "\t")

# when we have a workflow that we like there is a tendency to pile up our functions

# here is an example:
plot(hclust(dist(t(data[2:15]))))

# the introduction of the magrittr piping function into R..x
# allows us to do this in a way that make a work flow easier to view and easier to comment

library(magrittr)
# https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

data[2:15] %>%   # subset the object (columns 2:15 of the dataframe)
  t() %>%        # transform it so that columns are rows
  dist() %>%     # calculate distance
  hclust() %>%   # do a hierarchical cluster
  plot()         # then plot the result. 

# this approach really makes life easier when we have arguments in our functions
# we can change the method for the dist() function
# and we can add a title and some colour with the plot() function
# using pipes, this looks like this:

data[2:15] %>%                            # create a subset of object data (cols 2:15)
  t() %>%                                 # transform it so that columns are rows
  dist(method = "manhattan") %>%          # calculate distance using manhattan method
  hclust() %>%                            # do a hierarchial cluster
  plot(main="Cluster Diagram of Drug Treatments\n(2 Mar 2017)",     # add a title
       lwd=2, col="blue", cex = 1.1)      # thicker line and change colours  


# this code the other way looks like this:
plot(hclust(dist(t(data[2:15]), method = "manhattan")),
     main="Cluster Diagram of Drug Treatments\n(2 Mar 2017)", # add a title
     lwd=2, col="blue", cex = 1.1)

# it's a little bit difficult to separate the functions, objects and arguments in my opinion

# N.B. three key points to remember about using pipes:
# (1) brackets remain to allow us to identify functions()
# (2) the arguments go within the brackets - not the objects
# (3) objects are now 'piped' into the functions using %>%

