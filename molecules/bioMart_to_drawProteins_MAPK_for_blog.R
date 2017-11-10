# install bioMart if you haven't used it before
# remove the hash tags...
# source("http://www.bioconductor.org/biocLite.R ")
# biocLite()
# biocLite("biomaRt")
library(biomaRt)

# steps
# 1 choose a Mart with useMart()
# 2 choose a dataset with useMart()

# 3. make query with getBM()
# with
# A. filter  -  restriction on the query
# e.g. Interpro ID(s) [e.g. IPR000001]
# e.g. chromosome_name
# B. attributes  -  values we are interested in to retrieve.
# C. values - values you want to

listMarts()
# from https://bioconductor.org/packages/release/bioc/vignettes/biomaRt/inst/doc/biomaRt.html

# chosing a database = MART and a dataset - gets more focussed each step...
ensembl = useMart("ensembl",
                  dataset="hsapiens_gene_ensembl")

# Retrieve all entrezgene identifiers and HUGO gene symbols of genes which have
# a “MAP kinase activity” GO term associated with it.
# this is the GO:0004707
getBM(attributes = c('entrezgene','hgnc_symbol'),
      filters = 'go',
      values = 'GO:0004707',
      mart = ensembl)

# this is 14 proteins....

# create output in a dataframe and add uniprotswissprot
# which is the UniProt ID
output <- getBM(attributes = c('unigene',
                               'uniprotswissprot',
                               'hgnc_symbol'),
                filters = 'go',
                values = 'GO:0004707',
                mart = ensembl)


# returns a dataframe... pull out uniprotIDs for drawing...
uniprotIDs <- output$uniprotswissprot
# get rid of blank entries - turn into NA
uniprotIDs[uniprotIDs==""] <- NA
# remove NA
uniprotIDs <- na.omit(uniprotIDs)
# make the IDs characters
uniprotIDs <- as.character(uniprotIDs)
# just the unique ones
uniprotIDs <- unique(uniprotIDs)
# combine into one element
uniprotIDs <- paste(uniprotIDs, collapse = " ")
# this can now be used in drawProteins

# now get features from Uniprot
library(magrittr)

# install drawProteins from Github
# devtools::install_github("brennanpincardiff/drawProteins")
library(drawProteins)

uniprotIDs %>%
  drawProteins::get_features() %>%
  drawProteins::feature_to_dataframe() ->
  prot_data
# data frame with 722 observations

library(ggplot2)
# basic drawing
p <- draw_canvas(prot_data)
p <- draw_chains(p, prot_data)
p <- draw_domains(p, prot_data)
draw_repeat(p, prot_data)
p <- draw_motif(p, prot_data)
p <- draw_phospho(p, prot_data, size = 4)

# background and y-axis
p <- p + theme_bw(base_size = 20) +  # white background and change text size
  theme(panel.grid.minor=element_blank(),
    panel.grid.major=element_blank()) +
  theme(axis.ticks = element_blank(),
    axis.text.y = element_blank()) +
  theme(panel.border = element_blank())

# add titles
p <- p + labs(title = "Schematic of human MAP kinases",
  subtitle = "circles = phosphorylation sites\nsource:Uniprot")


# move legend to top
p <- p + theme(legend.position="top") + labs(fill="")

p
# AND it works!!
