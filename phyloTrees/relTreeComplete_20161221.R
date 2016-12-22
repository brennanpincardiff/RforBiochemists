### generating a phylogenetic tree of human Rel homology containing proteins

## packages required
￼source("http://www.bioconductor.org/biocLite.R")
biocLite("msa")

library(seqinr)  
library(msa)
library(ape)

### Get the sequences into R. 
# these are the accession numbers of the 10 proteins. 
# for these 10 proteins I found them manually by searching the Uniprot database
accNo <- c("AC=P19838 OR AC=Q00653 OR AC=Q01201 OR AC=Q04206 OR AC=Q04864 OR AC=O95644 OR AC=Q13469 OR AC=Q12968 OR AC=Q14934 OR AC=O94916")

# using the seqinr package
# From Chapter 4 of the seqinr handbook. 
# http://seqinr.r-forge.r-project.org/seqinr_3_1-5.pdf
### step 4.1 Choose a bank
choosebank()  # show's available data bases
mybank <- choosebank(bank = "swissprot")
str(mybank)
# UniProt Knowledgebase Release 2016_08 of 07-Sep-2016 Last Updated: Oct  4, 2016

### step 4.2 Make the query
mybank <- choosebank(bank = "swissprot")
rel_seq <- query("relSeq", accNo)
# N.B. protein info NOT returned in the same order as requested

# returns list object with six parts 
rel_seq$nelem # with 10 elements - thus 10 proteins
rel_seq$req  # gives the five necessary names

###  4.3 Extract sequences of interest
rel_seqs <- getSequence(rel_seq)
# check the names of the sequences
getName(rel_seq)

# it is necessary to put the sequences in fasta format
# for the multiple sequence alignment.
# useful to store them locally so that's what this does
write.fasta(sequences = rel_seqs, 
            names = getName(rel_seq),
            nbchar = 80, file.out = "relseqs")

### with sequences in a file we can open the file in the package msa

### read in Rel sequences from the file
mySeqs <- readAAStringSet("relseqs")   # from package Biostrings
length(mySeqs)
# 10 sequences... which is correct

### Perform a multiple sequence alignment
myAln <- msa(mySeqs)  
# this uses all the default settings and the CLUSTALW algorithm 
myAln
print(myAln, show="complete")

### Turn your alignment into a tree
# convert the alignment for the seqinr package
myAln2 <- msaConvert(myAln, type="seqinr::alignment")
# this object is a list object with 4 elements

# generate a distance matrix using seqinr package
d <- dist.alignment(myAln2, "identity")
# From the manual for the seqinr package
# This function computes a matrix of pairwise distances from aligned sequences
# using similarity (Fitch matrix, for protein sequences only) 

# have a look at the output
as.matrix(d)

# generate the tree with the ape package
# the nj() function allows neighbor-joining tree estimation
myTree <- nj(d)

# plot the tree
plot(myTree, main="Phylogenetic Tree of Human Rel Homology Domain Sequences")


# since all the species are human, 
# it might be easier to read without the "_HUMAN"
# with the sub() function
myAln2$nam <- sub("_HUMAN", "", myAln2$nam)

# pile up the functions to make a new tree
tr <- nj(dist.alignment(myAln2, "identity"))

# plot the new tree
plot(tr, main="Phylogenetic Tree of Human Rel Homology Domain Sequences")

# selected other ways to draw trees
# from http://ape-package.ird.fr/ape_screenshots.html
plot(tr, "c")
plot(tr, "u")   # unrooted tree 
plot(tr, "c", FALSE)  # ignores edge length
plot(tr, "u", FALSE)
plot(tr, "f", FALSE, cex = 0.5)


plot(tr,               # object with the tree information
     "u",              # unrooted - draws from the centre
     font = 2,         # makes font bold  
     edge.width = 2,   # makes thicker lines
     cex = 1.25,       # increase font size a little
     main = "Phylogenetic Tree of Human Rel Homology Domain Sequences")       

# looks quite stylish but I'm not sure about ignoring edge length
plot(tr, "u", 
     use.edge.length = FALSE, 
     font = 2, 
     edge.width = 2,
     main = "Phylogenetic Tree of Human Rel Homology Domain Sequences")

