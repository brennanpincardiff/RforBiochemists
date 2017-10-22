# module 4 script containing answers

## ----download_data_exercise_1-------------------
# this is the link to the data
link <- "http://www.mcponline.org/content/suppl/2015/02/02/M114.044479.DC1/mcp.M114.044479-2.xls"

# the download.file() function downloads and saves the file with the name given
download.file(url=link,destfile="file.xls", mode="wb")

library(readxl)
# then we can open the file and extract the data using the read_excel() function. 
data2 <- read_excel("file.xls", col_names=TRUE)


## ----ans_exercise_1-----------------------------
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
