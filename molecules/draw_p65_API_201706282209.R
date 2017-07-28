# OK so I have to write some kind of a script to use the new 
# Uniprot API
# This is R-UserGroup homework for Thursday....

# references: https://www.ebi.ac.uk/proteins/api/doc/#!/proteins/search
# references: https://academic.oup.com/nar/article-lookup/doi/10.1093/nar/gkx237

# needs the package httr
# install.packages("httr")
library(httr)

# my package with functions for extracting and graphing
# might require: install.packages("devtools")
devtools::install_github("brennanpincardiff/drawProteins")
library(drawProteins)

# so I want to use the Protein API 
uniprot_acc <- c("Q04206")

# Get UniProt entry by accession 
acc_uniprot_url <- c("https://www.ebi.ac.uk/proteins/api/proteins?accession=")

comb_acc_api <- paste0(acc_uniprot_url, uniprot_acc)

# basic function is GET() which accesses the API
# requires internet access
protein <- GET(comb_acc_api,
               accept_json())

status_code(protein)  # returns a 200 means it worked

# use content() function from httr to give us a list 
protein_json <- content(protein) # gives a Large list
# with 14 primary parts and lots of bits inside

# function from my package to extract names of protein
names <- drawProteins::extract_names(protein_json)

# I like a visualistion so I want to draw the features of this molecule
# https://www.ebi.ac.uk/proteins/api/features?offset=0&size=100&accession=Q04206

# Get features 
feat_api_url <- c("https://www.ebi.ac.uk/proteins/api/features?offset=0&size=100&accession=")

comb_acc_api <- paste0(feat_api_url, uniprot_acc)

# basic function is GET() which accesses the API
prot_feat <- GET(comb_acc_api,
                 accept_json())

status_code(prot_feat)  # returns a 200. 

# so to process this into a data.frame
prot_feat %>%
  content() %>%
  flatten() %>%
  drawProteins::extract_feat_acc() -> 
  features

# clean up for plotting
# focus on those that we want to plot...
features_plot <- features[features$length > 0, ]  
features_plot <- features_plot[complete.cases(features_plot),]
features_plot <- features_plot[features_plot$type!= "CONFLICT",]

# add colours 
library(randomcoloR)   # might need install
colours <- distinctColorPalette(nrow(features_plot)-1)
features_plot$col <- c("white", colours)

# now draw this...  with BaseR
# here is a function that does that....
drawProteins::draw_mol_horiz(names, features_plot)

# add phosphorylation sites
# get sites first. 
features %>%
  drawProteins::phospho_site_info() %>%
  drawProteins::draw_phosphosites(5, "yellow")  # 5 circle radius
text(250,0, "Yellow circles = phosphorylation sites", cex=0.8)
