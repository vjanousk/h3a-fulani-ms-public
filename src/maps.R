#########################################################################
# Functions for plotting populations on the map
# Author: Vaclav Janousek
# Date: 20/04/2022
#########################################################################

library(tidyverse)
library(cowplot)

library(ggmap)
library(maps)

library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

library(RColorBrewer)

#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")

## Load Data
## =========

setwd("~/h3a-fulani/")

read_tsv("data/samples/comparative_data_all_populations.tsv")[c(1:17),] -> populations

shape_names <- c(
  "circle", paste("circle", c("filled", "cross")),
  "square", paste("square", c("filled", "cross" )),
  "diamond", paste("diamond", c("filled", "plus")),
  "triangle", paste("triangle", c("filled")),
  paste("triangle down", c("filled")),
  "plus", "cross", "asterisk"
)


populations$population_label <- reorder(populations$population_label, populations$longitude)

cols = c(terrain.colors(17)[c(1:16)], terrain.colors(17)[16])

## maps & map_data

world = map_data("world")

ggplot() +
  geom_polygon(data = world, 
               aes(x=long, y = lat, group = group), fill="white", color="dark grey") + 
  coord_fixed(xlim = c(-20, 50),  ylim = c(0, 35), 1) +
  geom_point(data = populations, 
             aes(x=longitude, y=latitude, color=population_label, shape=population_label), size=5, stroke = 1) +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = rep(shape_names,2)[c(1:17)],name="Population") +
  theme_bw() +
  ylab("Latitude") +
  xlab("Longitude")

## rnaturalearth & sf

world <- ne_countries(scale = "medium", returnclass = "sf")

m = ggplot(data = world) +
    geom_sf(color = "black", fill="white") +
    xlab("Longitude") + ylab("Latitude") +
    theme_bw() +
    coord_sf(xlim = c(-20.0, 50.0), ylim = c(0.0, 35.0), expand = TRUE) +
    geom_point(data = populations, 
               aes(x=longitude, y=latitude, color=population_label, shape=population_label), 
               size=2, stroke = 1) +
    scale_colour_manual(values = cols,name="Population") +
    scale_shape_manual(values = rep(shape_names,2)[c(1:17)],name="Population") +
    theme(legend.text = element_text(size=6),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.3,"cm"))


jpeg("src/rmd-reports/maps/Fulani-all.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
m
dev.off()

## All African populations

read_tsv("data/samples/comparative_data_all_populations.tsv") %>%
  filter(str_detect(region,"Africa")) %>%
  filter(!str_detect(population,fixed("Fulani (Triska et al. 2015)")))  -> populations

populations %>%
  mutate(fulani=case_when(grepl("Fula", population_label) ~ "Fulani",
                          grepl("Halpularen", population_label, ignore.case = TRUE) ~"Fulani",
                          TRUE ~ region)) %>%
  mutate(fulani = factor(fulani, levels=c("Fulani","West Africa","Central Africa","East Africa","North Africa"))) %>%
  arrange(fulani, population_label) %>%
  mutate(id = row_number())-> populations

brewer.pal(12, "Paired")

cols = c(rep("#FB9A99",9),rep("#E31A1C",9),rep("#33A02C",10),
         rep("#B2DF8A",9),rep("#B15928",9),rep("#FF7F00",5),rep("#1F78B4",4))

m = ggplot(data = world) +
  geom_sf(color = "black", fill="white") +
  xlab("Longitude") + ylab("Latitude") +
  theme_bw() +
  coord_sf(xlim = c(-20.0, 50.0), ylim = c(0.0, 35.0), expand = TRUE) +
  geom_point(data = populations, 
             aes(x=longitude, y=latitude, color=reorder(population_label,id), shape=reorder(population_label,id)), 
             size=2, stroke = 0.5) +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values =  rep(shape_names,4),name="Population") +
  theme(legend.text = element_text(size=6),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.3,"cm"))


jpeg("src/rmd-reports/maps/Africa-all.jpg",
     units="in",width = 10, height = 5, res = 300, quality=100)
m
dev.off()
