# Import the Excel spreadsheets from published JPR paper
# This is the URL: 
# http://pubs.acs.org/doi/suppl/10.1021/pr5002803/suppl_file/pr5002803_si_003.xlsx
# only has one sheet

# Going to try new readxl package:
install.packages("readxl")  # only necessary 1st time
library(readxl)

# would be good if I could get this directly from the webpage 
# but I need to use local file at the moment. 
data <- read_excel("pr5002803_si_003.xlsx")

# some need to cleaning of the files...
data2 <- data[4:731,2:6] #just the columns I need now
names <- c("Prot.no", "Id.prot","Ac.No", "Pep.Cnt", "Tot.Ion.Scr")
colnames(data2) <- names
rownames(data2) <- NULL #not necessary or useful

# this is now a list of 728 proteins with peptide count and total ion score
write.csv(data2, file = "jprProtList.csv")

