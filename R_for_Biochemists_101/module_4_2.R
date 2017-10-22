## ----activate_packages, include=TRUE, echo=TRUE--------------------------
library(ggplot2)     # graphing package
library(ggthemes)    # expand the themes available
library(dplyr)       # package for subsetting & manipulating dataframe

## ---- include=TRUE, echo=TRUE--------------------------------------------
data <- read.csv("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/R_for_Biochemists_101/data/drug_data_4_volc.csv")

View(data)
str(data)


## ---- include=TRUE, echo=TRUE--------------------------------------------
# the dplyr select() function pulls out columns from our object data
drug_d <- select(data, Comment.gene_symbols., Drug_D.diff.logFC, d_p_val)
colnames(drug_d) <- c("names", "logFC", "p_val")

## ---- include=TRUE, echo=TRUE--------------------------------------------
## Draw our first plot
ggplot(data=drug_d, 
            aes(x=logFC, 
                y =-log10(p_val))) +
  geom_point() 


## ---- include=TRUE, echo=TRUE, eval=FALSE--------------------------------
## ## Draw our first plot
## ggplot(drug_d,
##        aes(logFC,
##            -log10(p_val))) +
##   geom_point()
## 

## ---- include=TRUE, echo=TRUE--------------------------------------------
## using alpha to address overplotting
ggplot(drug_d, 
            aes(logFC, 
                -log10(p_val))) +
  geom_point(alpha=0.3) 

## ---- include=TRUE, echo=TRUE--------------------------------------------

# it's useful to add a colour at a threshold to p-value to indicate the number of transcripts that have changed significantly. 

drug_d$threshold = as.factor(drug_d$p_val < 0.001)

# plot again...
ggplot(drug_d, 
            aes(logFC, -log10(p_val), 
                colour=threshold)) +
  geom_point(alpha=0.3)


## ----download_data_exercise_1, include=TRUE, echo=TRUE-------------------
# this is the link to the data
link <- "http://www.mcponline.org/content/suppl/2015/02/02/M114.044479.DC1/mcp.M114.044479-2.xls"

# the download.file() function downloads and saves the file with the name given
download.file(url=link,destfile="file.xls", mode="wb")

library(readxl)
# then we can open the file and extract the data using the read_excel() function. 
data2 <- read_excel("file.xls", col_names=TRUE)


## ----ans_exercise_1, include=TRUE, echo=TRUE-----------------------------
library(dplyr)
# select the columns you want...
data_s <- select(data2, "Protein Name", "SwissProt Acc. No.",
                 "Log2 Fold Change", "P Value")

colnames(data_s) <- c("names", "swissprot_acc", "logFC", "p_val")


##Identify the genes that have a p-value < 0.01
data_s$threshold = as.factor(data_s$p_val < 0.01)

library(ggplot2)
## Just plot the data
ggplot(data=data_s, 
       aes(x=logFC, y =-log10(p_val), 
           colour=threshold)) +
  geom_point(alpha=0.4, size=1.75)

## N.B. there is a Warning message.

## ---- include=TRUE, echo=TRUE--------------------------------------------
##Construct the plot object
volc_d <- ggplot(drug_d, 
            aes(x=logFC, y =-log10(p_val), 
                colour=threshold)) +
  geom_point(alpha=0.3)

## ---- include=TRUE, echo=TRUE--------------------------------------------
volc_d <- volc_d + 
  xlab("log2 fold change") + 
  ylab("-log10 p-value") +
  theme(legend.position="none")
volc_d     # shows us the object - the graph

## ---- include=TRUE, echo=TRUE--------------------------------------------
volc_d + theme_bw()
volc_d + theme_dark()   # from ggthemes
volc_d + theme_few()    # from ggthemes

# when you have one you like, then modify the object...
volc_d <- volc_d + theme_bw()

## ---- include=TRUE, echo=TRUE--------------------------------------------
volc_d <- volc_d + 
  theme(legend.position="none")
volc_d     # shows us the object - the graph

## ---- include=TRUE, echo=TRUE--------------------------------------------
volc_d <- volc_d + ggtitle("Drug D")
# add source? more details?


## ---- include=TRUE, echo=TRUE--------------------------------------------
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

## ----ans_exercise_2, include=TRUE, echo=TRUE-----------------------------
liv_volc <- volc_d %+% data_s

# then we need to give it a new title and subtitle
liv_volc <- liv_volc  + 
  labs(title = "CLL Proteomics Data from Liverpool",
       subtitle = "Comparing two patient cohorts (unmutated vs mutated)
source: Eagle et al (2015) Mol Cell Proteomics, 14, 933-945")
  

##Construct the plot object
liv_volc_2 <- ggplot(data=data_s, 
            aes(x=logFC, y =-log10(p_val), 
                colour=threshold)) +
  geom_point(alpha=0.4, size=1.75) +
  xlim(c(-6, 6)) +
  labs(x = "log2 fold change",         # label x-axis
         y = "-log10 p-value",  # label y-axis
       title = "CLL Proteomics Data from Liverpool",
       subtitle = "Comparing two patient cohorts (unmutated vs mutated)
source: Eagle et al (2015) Mol Cell Proteomics, 14, 933-945") +
  theme_bw() +
  theme(legend.position="none")

liv_volc_2
## N.B. there is a Warning message.

## ---- include=TRUE, echo=TRUE--------------------------------------------
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


## ---- include=TRUE, echo=TRUE--------------------------------------------
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


## ---- include=TRUE, echo=TRUE--------------------------------------------

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

## ---- include=TRUE, echo=TRUE--------------------------------------------
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

## ----demo_geom_label, include=TRUE, echo=TRUE----------------------------
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

## ----ans_exercise_3, include=TRUE, echo=TRUE-----------------------------
## Exercise 3
# label some of the points on the data set...
data_s$absFC <- abs(data_s$logFC)

data_s_2label <- filter(data_s, absFC>1.5, p_val <0.001)

# now add the text using geom_text
liv_volc +  geom_text(data = data_s_2label, 
                    aes(x = logFC, 
                        y = -log10(p_val), 
                        label = swissprot_acc))

# colour them with Color Brewer pallette
library(RColorBrewer)
our_palette <- brewer.pal(nrow(data_s_2label), "Set1")

liv_volc +  geom_point (data = data_s_2label, 
                      aes(x = logFC, 
                          y = -log10(p_val)), 
                      colour = our_palette) +
  geom_text(data = data_s_2label, 
            aes(x = logFC, 
                y = -log10(p_val), 
                label = swissprot_acc), 
            colour = our_palette,
            vjust = 0, nudge_y = 0.2)

liv_volc +  geom_point (data = data_s_2label, 
                 aes(x = logFC, 
                     y = -log10(p_val)), 
                 colour = our_palette) +
  geom_text(data = data_s_2label, 
            aes(x = logFC, 
                y = -log10(p_val), 
                label = names), 
            colour = our_palette,
            vjust = 0, nudge_y = 0.1)


