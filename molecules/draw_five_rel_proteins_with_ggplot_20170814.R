# START
# working with rel proteins as I have been studying these for 10 year
# using ggplot2 to draw the plots.
# proteins are aligned to the N-termini
# alternative alignment options would be worth considering

library(httr)
# devtools::install_github("brennanpincardiff/drawProteins")
library(drawProteins)
library(ggplot2)

# Uniprot accession numbers of five human NF-kappaB family members
proteins_acc <- c("Q04206 Q01201 Q04864 P19838 Q00653")

# accession numbers need to be combined with "%2C" for Uniprot API
proteins_acc_url <- gsub(" ", "%2C", proteins_acc)

# this is the baseurl for a multiple features query
baseurl <- "https://www.ebi.ac.uk/proteins/api/features?offset=0&size=100&accession="

# make url to GET features for multiple accession numbers
url <- paste0(baseurl, proteins_acc_url)

# basic function is GET() which accesses the API
# accept_json() argument gives a JSON object
prots_feat <- GET(url,
                  accept_json())

status_code(prots_feat)  # returns a 200 - good

# extract content() - gives a list with length = number of acc no
prots_feat_red <- httr::content(prots_feat)


################
# loop to work through the API object and convert to data.frame
# probably there is a better way to do this
features_total_plot <- NULL
for(i in 1:length(prots_feat_red)){
  # the extract_feat_acc() function takes features into a data.frame
  features_temp <- drawProteins::extract_feat_acc(prots_feat_red[[i]])
  features_temp$order <- i  # this order is needed for plotting later
  features_total_plot <- rbind(features_total_plot, features_temp)
}

# look at the data.frame
View(features_total_plot)  # 319 observations


### use ggplot2::geom_rect to draw moleucles
### in this script each type of feature is drawn separately.

## step 1: set up the canvas in ggplot (no data added)
# want to put text on the left in the area of negative numbers
plot_start <- -max(features_total_plot$end)*0.2

# need it to be longer than the longest molecule plus a bit...
plot_end <- max(features_total_plot$end) + max(features_total_plot$end)*0.1

p <- ggplot() +
  ylim(0.5, max(features_total_plot$order)+0.5) +
  xlim(plot_start, plot_end)
p # show the object

## step 2: draw the CHAINS in grey (narrower)
p <- p + geom_rect(data= features_total_plot[features_total_plot$type == "CHAIN",],
                   mapping=aes(xmin=begin,
                               xmax=end,
                               ymin=order-0.2,
                               ymax=order+0.2),
                   colour = "black",
                   fill = "grey")
p

## step 3: annotate with names
p <- p +  annotate("text", x = -10, y = features_total_plot$order,
                   label = features_total_plot$entryName,
                   hjust = 1,
                   size = 2)
# check the object
p

## step 4: plot domains fill by description
p <- p + geom_rect(data= features_total_plot[features_total_plot$type == "DOMAIN",],
                   mapping=aes(xmin=begin,
                               xmax=end,
                               ymin=order-0.25,
                               ymax=order+0.25,
                               fill=description))
p

# label domains with descriptions
feat_domain <- features_total_plot[features_total_plot$type == "DOMAIN", ]
p <- p + geom_label(data = feat_domain,
                    aes(x = begin + (end-begin)/2,
                        y = order,
                        label = description,
                        size = 1.75))
p
# this works well with geom_label()

## step 5 plot motifs fill by description
p <- p + geom_rect(data= features_total_plot[features_total_plot$type == "MOTIF",],
                   mapping=aes(xmin=begin,
                               xmax=end,
                               ymin=order-0.25,
                               ymax=order+0.25,
                               fill=description))

p
# not going to label these as they're in the description and they are small

## step 6 plot repeats fill by description
p <- p + geom_rect(data= features_total_plot[features_total_plot$type == "REPEAT",],
                   mapping=aes(xmin=begin,
                               xmax=end,
                               ymin=order-0.25,
                               ymax=order+0.25),
                   fill = "lightblue")
p

# label repeats (for this they are ANK but remove digits)
p <- p + geom_text(data = features_total_plot[features_total_plot$type == "REPEAT",],
                   aes(x = begin + (end-begin)/2,
                       y = order,
                       label = gsub("\\d", "", description)),
                   size =1)
p

# add titles
p <- p + labs(x = "Amino acid number",         # label x-axis
              y = "",  # label y-axis
              title = "Schematic of five human NF-kappaB proteins",
              subtitle = "circles = phosphorylation sites\nRHD: Rel Homology Domain\nsource:Uniprot")
p

# white background and remove y-axis
p <- p + theme_bw(base_size = 2) +  # white background
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
  theme(axis.ticks = element_blank(), axis.text.y = element_blank()) +
  theme(panel.border = element_blank())
p

# add phosphorlation sites
# extract info about sites using phospho_site_info() function
phospho_sites <- drawProteins::phospho_site_info(features_total_plot)

# draw phosphorylation sites on the protein with geom_point()
p <- p + geom_point(data = phospho_sites,
                    aes(x = begin,
                        y = order+0.25),
                    shape = 21,
                    colour = "black",
                    fill = "yellow", size = 0.25, stroke = 0.25)

# to see it properly you need to zoom...

# I have to admit that I am pleased with this diagram.
# The colours are better, the labels seem to work.

p + ggsave("five_human_nfkappab_proteins_20170815_2.png",
           width = 4, height = 2, units = "in",
           dpi = 600)

p + ggsave("five_rect_with_text_2.png",
           width = 10, height = 5, units = "cm",
           dpi = 600)

# END




