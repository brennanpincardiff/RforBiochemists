dataMCP <- read_excel("mcp.M114.044479-2.xls")
dataJPR <- read.csv("jprProtList.csv")
# Id.prot has been imported as a factor

listMCP <- dataMCP[,1]
listJPR <- as.character(dataJPR$Id.prot) # convert to character

# get rid of trailing white space with this function
# from: http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r#
trim.trailing <- function (x) sub("\\s+$", "", x)
listJPR<-trim.trailing(listJPR)

# http://stackoverflow.com/questions/17598134/compare-two-lists-in-r
overlap <- intersect(listMCP, listJPR)  # easy/useful - 625 proteins
MCP.unique <- setdiff(listMCP, listJPR) # in 1st NOT 2nd
JPR.unique <- setdiff(listJPR, listMCP)
full.list <- unique(c(listMCP,listJPR))

#package VennDiagram
install.packages("VennDiagram")
library(VennDiagram)

#we just have overlaps etc...
grid.newpage()
venn.plot <- draw.pairwise.venn(area1 = length(listMCP), #size of MCP set
                                area2 = length(listJPR), #size of JPR set
                                cross.area = length(overlap),  # Overlap
                                c("MCP Data", "JPR Data"), scaled = TRUE,
                                fill = c("green", "blue"),
                                cex = 1.5,
                                cat.cex = 1.5,
                                cat.pos = c(320, 25),
                                cat.dist = .05) 
