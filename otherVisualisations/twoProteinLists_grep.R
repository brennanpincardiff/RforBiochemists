dataMCP <- read_excel("mcp.M114.044479-2.xls")
dataJPR <- read.csv("jprProtList.csv")

listMCP <- dataMCP[,1]
listJPR <- as.character(dataJPR$Id.prot)

overlap <- intersect(listMCP, listJPR)
# Look for an abundant protein that should be present in both
# Actin - cytoplasmic 2
# find where it is and count the characters
i <- grep("Actin, cytoplasmic 2", listJPR)
nchar(listJPR[i])
# 21 characters...

i <- grep("Actin, cytoplasmic 2", listMCP)
nchar(listMCP[i])
# 20 characters...

# different number of characters!
# data from JPR paper has a space at the end.... arghh!
