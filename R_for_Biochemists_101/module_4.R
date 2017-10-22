### Code for Video 1 ###
library(dplyr)       # package for subsetting & manipulating dataframe
library(ggplot2)     # graphing package

data <- read.csv("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/R_for_Biochemists_101/data/drug_data_4_volc.csv")

View(data)
str(data)

# the dplyr select() function pulls out columns from our object data
drug_d <- select(data, Comment.gene_symbols., Drug_D.diff.logFC, d_p_val)

colnames(drug_d) <- c("names", "logFC", "p_val")

## Draw our first volcano plot
ggplot(data=drug_d, 
            aes(x=logFC, 
                y =-log10(p_val))) +
  geom_point() 
# we are generating the y values on the fly using -log10(p_val) ...


# defaults mean that we can leave out some of the code...
ggplot(drug_d,
        aes(logFC,
            -log10(p_val))) +
   geom_point()


## using alpha to address overplotting
ggplot(drug_d, 
            aes(logFC, 
                -log10(p_val))) +
  geom_point(alpha=0.3) 


# it's useful to add a colour at a threshold to p-value to indicate the number of transcripts that have changed significantly. 
# add another column to our data data as a factor 
drug_d$threshold = as.factor(drug_d$p_val < 0.001)

# look at the dataframe
View(drug_d)

# plot again...
ggplot(drug_d, 
            aes(logFC, -log10(p_val), 
                colour=threshold)) +
  geom_point(alpha=0.3)

### END of Video one ###

### Start of Video two ###
### We've made a plot so now we want to make and reuse our plot object

##Construct the plot object
volc_d <- ggplot(drug_d, 
            aes(x=logFC, y =-log10(p_val), 
                colour=threshold)) +
  geom_point(alpha=0.3)

# Add some labels for x and y axis
volc_d <- volc_d + 
  xlab("log2 fold change") + 
  ylab("-log10 p-value") +
  theme(legend.position="none")
volc_d     # shows us the object - the graph

## Explore themes (again)
library(ggthemes)    # expand the themes available
volc_d + theme_bw()
volc_d + theme_dark()   # from ggthemes
volc_d + theme_few()    # from ggthemes

# when you have one you like, then modify the object...
volc_d <- volc_d + theme_bw()


## get rid of the legend - it doesn't really help. 
volc_d <- volc_d + 
  theme(legend.position="none")
volc_d     # shows us the object - the graph

## add a title
volc_d <- volc_d + ggtitle("Drug D")



# pull out data for drug A and C & change names...
drug_a <- select(data, Comment.gene_symbols., Drug_A.diff.logFC, a_p_val)
colnames(drug_a) <- c("names", "logFC", "p_val")
drug_a$threshold = as.factor(drug_a$p_val < 0.001)

drug_c <- select(data, Comment.gene_symbols., Drug_C.diff.logFC, c_p_val)
colnames(drug_c) <- c("names", "logFC", "p_val")
drug_c$threshold = as.factor(drug_c$p_val < 0.001)


# this %+% allows us to create a new object
# with new data based on the setting of the existing ggplot object 
# It overwrites the data
# It must contain the same column names otherwise it won't work.
volc_c <- volc_d %+% drug_c

# then we need to give it a new title
volc_c <- volc_c + ggtitle("Drug C")

# do the same for the other Drug A
volc_a <- volc_c %+% drug_a
volc_a <- volc_a + ggtitle("Drug A")

# show our three plots:
volc_a
volc_c
volc_d
# Note that the axis are not the same... but you could make them the same...

## END of Video 2 ##

## Start of Video 3 ##
# Labelling some points on our plots
# find points with the biggest changes
# want both up and down genes so generate an absolute Fold change
drug_d$absFC <- abs(drug_d$logFC)
# using the filter() of the dplyr package
drug_d_2label <- filter(drug_d, absFC>1.5, p_val <0.0001)
# need better gene names!!!

# now add the text using geom_text
volc_d +  geom_text(data = drug_d_2label, 
                    aes(x = logFC, 
                        y = -log10(p_val), 
                        label = names))



# I find it a little difficult to know which points goes with which. 
# so overplot with colour
# make a simple palette of colours
# same length as the number of points - 9
my_palette <- c("black", "red", "black", "red", "black", "red",
                "black", "red", "black")

volc_d +  geom_point (data = drug_d_2label, 
                      aes(x = logFC, 
                          y = -log10(p_val)), 
                      colour = my_palette) +
  geom_text(data = drug_d_2label, 
            aes(x = logFC, 
                y = -log10(p_val), 
                label = names), 
            colour = my_palette)



# nudge the labels so that just above the points
volc_d +  geom_point (data = drug_d_2label, 
                      aes(x = logFC, 
                          y = -log10(p_val)), 
                      colour = my_palette) +
  geom_text(data = drug_d_2label, 
            aes(x = logFC, 
                y = -log10(p_val), 
                label = names), 
            colour = my_palette,
            vjust = 0, nudge_y = 0.2)


# other pallettes can be made of any length
# the library RColorBrewer has lots of pallettes
library(RColorBrewer)
our_palette <- brewer.pal(nrow(drug_d_2label), "Set1")

volc_d +  geom_point (data = drug_d_2label, 
                      aes(x = logFC, 
                          y = -log10(p_val)), 
                      colour = our_palette) +
  geom_text(data = drug_d_2label, 
            aes(x = logFC, 
                y = -log10(p_val), 
                label = names), 
            colour = our_palette,
            vjust = 0, nudge_y = 0.2)


# geom_label gives another method 
# here we have drawn the points after the labels...
volc_d  + geom_label(data = drug_d_2label, 
                     aes(x = logFC, 
                         y = -log10(p_val), 
                         label = names),
                     colour = my_palette,
                     vjust = 0, nudge_y = 0.2) +
         geom_point (data = drug_d_2label, 
                aes(x = logFC, 
                    y = -log10(p_val)), 
                colour = my_palette)

## END

