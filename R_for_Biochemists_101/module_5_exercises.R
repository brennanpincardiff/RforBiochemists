## ----ans_exercise_1, include=TRUE, echo=TRUE-----------------------------
healthy_foods <- c("apples", "oranges", "carrots", "lettuce", "cucumber")

trees <- as.factor(c("Oak", "Baobab", "Eucalyptus", "Maple", "Ginkgo biloba",
                     "Cherry blossom", "Magnolia", "Horse chestnut"))
fours <- seq(from = 0, to = 100, by=4)

protein_names <- c("EGF", "EGF_receptor", "Grb2", "Ras", "Raf", "Erk")

protein_amounts <- c(100, 200, 300, 400, 500, 600)

# make the data frame
data <- data.frame(protein_names, protein_amounts)

# plot the data
ggplot(data,
       aes(x = protein_names,
           y = protein_amounts)) +
  geom_bar(stat = "identity")




## ----ans_exercise_2 -----------------------------------------------------
library(sf)
## Repeat the workflow for Argentina

arg_link <- "https://geodata.ucdavis.edu/gadm/gadm4.1/gpkg/gadm41_ARG.gpkg"

# download the data
download.file(url=arg_link, destfile="arg_file.gpkg", mode="wb")
# only 19.5 MB so faster than United Kindgom file

# check layers within the data
st_layers("arg_file.gpkg")

# this function is from the package sf

# output
# Driver: GPKG
# Available layers:
#  layer_name geometry_type features fields crs_name
# 1  ADM_ADM_0 Multi Polygon        1      2   WGS 84
# 2  ADM_ADM_1 Multi Polygon       24     11   WGS 84
# 3  ADM_ADM_2 Multi Polygon      502     13   WGS 84

## layer 2, maybe
# open one of the layers in the file
arg_2 <- st_read("arg_file.gpkg", layer = "ADM_ADM_1")

# simplify the level of detail st_simplify()
arg_simpl <- st_simplify(arg_2,
                        dTolerance = 1000)

# check size of arg_2
round(c(object.size(arg_2)) / 1024)
# [1] 6098

# to the size of arg_simpl
round(c(object.size(arg_simpl)) / 1024)
# [1] 147

# plot the file
ggplot() + geom_sf(data = arg_simpl)
# plots faster...
