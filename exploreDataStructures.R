# Exploring Data Dtructures

## objects are made up of various types. 
## I want to discuss objects that contain data

## Data goes into objects
### Use the assignment function "<-"
### Protein Concentrations
prot <- c(0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000, 
          0.000, 0.016, 0.031, 0.063, 0.125, 0.250, 0.500, 1.000) 

### Absorbance from my protein assay
abs <- c(0.329, 0.352, 0.349, 0.379, 0.417, 0.491, 0.668, 0.956, 
         0.327, 0.341, 0.355, 0.383, 0.417, 0.446, 0.655, 0.905)
### these appear in the R-Studio environment as Values

## These objects are vectors - all the data elements must be the same type
### A vector is the simplist type of object
### can be numeric, character, logical, factors
class(prot)  #### numeric

### Some other types of vectors
protein <- "albumin"
class(protein) #### character

truth <- c(TRUE, FALSE, TRUE, TRUE)
class(truth) #### logical

### you can identify things inside the objects
prot[2]

### and parts of objects
prot[1:8]

### functions can be applied to whole objects (particularly arrays)
### the plot function puts the first element of each object against each other
plot(abs~prot)


# More Complicated structures
## <b>lists</b> are another type of object
## the lm() function makes an object called line which is a list. 
## lists contain a mixture of data types. 
line <- lm(abs~prot)
### the R-Studio environment says a "List of 12"

## there are various ways of getting information from this object 
## type the name of the object
line

## use the summary() function
summary(line)

## use the $ 
summary(line)$r.squared

### we used this to extract the r2
### we created the object r2 using the function summary()
r2 <- summary(line)$r.squared
### and the function round() - gives us three decimal points
r2 <- round(summary(line)$r.squared, 3)
r2
class(r2)
### from the list we have extracted a number. 

# <b>matrices</b> are two dimensional structures 
##  the data types are all the same

# <b>data frames</b> are two dimensional structures
##  contains different types of data

# often when we import data, it gets imported as a data frame.
## here is an example:
data <- read.csv("http://science2therapy.com/data/wellsDataSimp.csv")

## the R-Studio environment puts it in "data" and gives us some info

## Have a quick look at it
View(data)
str(data)

## we have names of columns and we have the class of the data within the column
## note: Factors, num, int
data$Virus

# Simple plot from this data frame
plot(data[5:7])

## Another plot from this data frame
plot(data$P.Erk, data$S.phase.cnt)

## we can manipulate objects including data frames 
## which is the subject of the next tutorial.

