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
library(sp)
library(rmapshaper)
link <- "http://biogeo.ucdavis.edu/data/gadm2.8/rds/ARG_adm1.rds"
download.file(url=link, destfile="file.rda", mode="wb")
region_map <- readRDS("file.rda")
region_map <- ms_simplify(region_map, keep = 0.01)
library(broom)
region_map <- tidy(region_map, region="NAME_1")
library(ggplot2)
ggplot(data = region_map,
       aes(x = long, y = lat, group = group)) + 
         geom_path() +
  coord_map()
