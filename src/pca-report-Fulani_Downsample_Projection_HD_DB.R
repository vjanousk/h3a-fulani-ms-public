#########################################################################
# Functions for PCA report (pca-report-Fulani_Downsample_Projection_HD_DB.Rmd)
# Author: Vaclav Janousek
# Date:19/04/2023
#########################################################################

# devtools::install_github("AckerDWM/gg3D")

library(tidyverse)
library(umap)
library(cluster)
library(RColorBrewer)

library("gg3D")

setwd("~/h3a-fulani/")

data="data/pca-results/Fulani_Downsample_Projection_HD_DB-165300-190423/Fulani_Downsample_Projection_HD_DB.pca.evec"

## Load Data
## =========

pc <- as.character(paste0("PC",c(1:10)))

read_table(data, 
           col_names = FALSE, skip=1) %>%
  select(c(1:11)) %>%
  rename_at(vars(as.character(paste0("X",c(1:11)))),
            ~c("id",pc)) %>% 
  separate(id, into=c("population_label","sample_id"), sep=":") %>%
  mutate(population_label=str_remove(population_label,"_p")) -> pca

read_tsv("data/samples/comparative_data_all_populations.tsv") -> populations

pca %>% 
  inner_join(populations %>% select(population_label, longitude, latitude, region, country, lifestyle3), 
             by=c("population_label" = "population_label")) -> pca_with_pop

## Eigen Values
## ============

read_table(data, 
           col_names = FALSE, n_max=1)[2:11] %>% unlist(., use.names=FALSE) -> eigen

eigen <- tibble(pc = c(1:10), eigen=eigen)

p <- ggplot(eigen, aes(x=pc, y=eigen)) +
      geom_line(col="grey") +
      geom_point(col="dark blue") +
      theme_bw() +
      ylab("Eigen Value") +
      xlab("Principal Component")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-eigen-value-Fulani_Downsample_Projection_HD_DB.jpg",
     units="in",width = 8, height = 4, res = 300, quality=100)
p
dev.off()

## Read PCA vectors
## ================

shape_names <- c(
  "circle", paste("circle", c("filled", "cross")),
  "square", paste("square", c("filled", "cross" )),
  "diamond", paste("diamond", c("filled", "plus")),
  "triangle", paste("triangle", c("filled")),
  paste("triangle down", c("filled")),
  "plus", "cross", "asterisk"
)


pca_with_pop %>%
  mutate(fulani=case_when(grepl("Fula", population_label) ~ "Fulani",
                          grepl("Halpularen", population_label, ignore.case = TRUE) ~"Fulani",
                          TRUE ~ region)) -> pca_with_pop

pca_with_pop$fulani <- factor(pca_with_pop$fulani, 
                              levels=c("Fulani","West Africa","Central Africa","East Africa","North Africa","Middle East","South Europe","Central Europe","North Europe"))

## PCA by the main geographies

p <- ggplot(pca_with_pop, aes(x=PC2, y=PC1, color=fulani,shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC2")

jpeg("src/rmd-reports/pca-report-GroupL_and_Reference_HD_DB/pca-pc1-pc2.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC3, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC3")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc3.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC2, y=PC3, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names)[c(1:33)],name="Population") +
  theme_bw() +
  ggtitle("PC2 x PC3")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc2-pc3.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC4, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC4")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc4.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC5, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC5")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc5.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC6, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC6")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc6.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC7, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC7")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc7.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC8, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC8")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc8.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC9, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC9")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc9.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC10, color=fulani, shape=fulani)) +
  geom_point() +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = shape_names[c(1:9)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC10")

jpeg("src/rmd-reports/pca-report-2Fulani_and_Reference_HD_DB/pca-pc1-pc10.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

## PCA - resolution by ethnicities (Fulani Downsample Projection)

cols = c(brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"),
        brewer.pal(8,"Dark2"))

p <- ggplot(pca, aes(x=PC1, y=PC2, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC2")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc2-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC3, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC3")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc3-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC4, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC4")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc4-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC5, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC5")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc5-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC6, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC6")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc6-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC7, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC7")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc7-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC8, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC8")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc8-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC9, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC9")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc9-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()


p <- ggplot(pca_with_pop, aes(x=PC1, y=PC10, color=population_label,shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = cols,name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names,shape_names,shape_names)[c(1:66)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC10")

jpeg("src/rmd-reports/pca-report-Fulani_Downsample_Projection_HD_DB/pca-pc1-pc10-by-pop.jpg",
     units="in",width = 16, height = 8, res = 300, quality=100)
p
dev.off()




############################

theta=150
phi=40

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC2, z=PC3, color=fulani, shape=fulani)) + 
  theme_void() +
  labs_3D(
    labs=c("PC1", "PC2", "PC3"),
    hjust=c(0,1,1), vjust=c(1, 1, -0.2), angle=c(0, 0, 90)) +
  axes_3D(theta=theta, phi=phi) +
  stat_3D(theta=theta, phi=phi) +
  scale_colour_manual(values = brewer.pal(9, "Paired"),name="Population") +
  scale_shape_manual(values = c(shape_names,shape_names,shape_names)[c(1:33)],name="Population")

jpeg("src/rmd-reports/pca-report-Fulani_and_Reference_HD_DB/pca-pc1-pc2-pc3.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

## UMAP analysis - Test different parameters
## =========================================

plot_umap <- function(data, n_neighbors=10, min_dist=1, spread=2){
  
  umap.config = umap.defaults
  
  umap.config$n_neighbors=n_neighbors
  umap.config$min_dist=min_dist
  umap.config$spread=spread
  
  pca.umap = umap(data[3:12],config=umap.config)
  
  tibble(sample_id=data$sample_id, 
         population_label=data$population_label, 
         longitude=data$longitude,
         latitude=data$latitude,
         region=data$region,
         country=data$country,
         x=pca.umap$layout[,1], 
         y=pca.umap$layout[,2]) -> pca.umap.tibble
  
  pca.umap.tibble %>%
    mutate(fulani=case_when(grepl("Fula", population_label) ~ "Fulani",
                            grepl("Halpularen", population_label, ignore.case = TRUE) ~"Fulani",
                            TRUE ~ region)) -> pca.umap.tibble
  
  pca.umap.tibble$fulani <- factor(pca.umap.tibble$fulani, 
                                   levels=c("Fulani","West Africa","Central Africa","East Africa","North Africa","Middle East","South Europe","Central Europe","North Europe"))
  
  p <- ggplot(pca.umap.tibble, aes(x=x, y=y, color=fulani, shape=fulani)) +
    geom_point(alpha=0.8,size=2) +
    scale_colour_manual(values = rep(brewer.pal(10, "Paired"),3)[c(1:9)],name="Legend") +
    scale_shape_manual(values = rep(shape_names,3)[c(1:9)],name="Legend") +
    ggtitle(paste0("UMAP: n_neighbors=",n_neighbors,"; min_dist=",min_dist,"; spread=",spread)) +
    theme_bw() +
    theme(axis.title = element_text(size = 12,face="bold"),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size=8),
          legend.title=element_text(size=8,face="bold"),
          legend.text=element_text(size=8))
  
  jpeg(paste0("src/rmd-reports/pca-report-Fulani_and_Reference_HD_DB/umap-",n_neighbors,"-",min_dist,"-",spread,"-Fulani_and_Reference_HD_DB.jpg"),
       units="in",width = 8, height = 6, res = 300, quality=100)
  plot(p)
  dev.off()
}

plot_umap(pca_with_pop, n_neighbors=5, min_dist=1, spread=2)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=1, spread=2)
plot_umap(pca_with_pop, n_neighbors=20, min_dist=1, spread=2)
plot_umap(pca_with_pop, n_neighbors=40, min_dist=1, spread=2)

plot_umap(pca_with_pop, n_neighbors=5, min_dist=1, spread=3)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=1, spread=3)
plot_umap(pca_with_pop, n_neighbors=20, min_dist=1, spread=3)
plot_umap(pca_with_pop, n_neighbors=40, min_dist=1, spread=3)

plot_umap(pca_with_pop, n_neighbors=5, min_dist=2, spread=3)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=2, spread=3)
plot_umap(pca_with_pop, n_neighbors=20, min_dist=2, spread=3)
plot_umap(pca_with_pop, n_neighbors=40, min_dist=2, spread=3)

plot_umap(pca_with_pop, n_neighbors=5, min_dist=2, spread=4)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=2, spread=4)
plot_umap(pca_with_pop, n_neighbors=20, min_dist=2, spread=4)
plot_umap(pca_with_pop, n_neighbors=40, min_dist=2, spread=4)

plot_umap(pca_with_pop, n_neighbors=60, min_dist=1, spread=3)
plot_umap(pca_with_pop, n_neighbors=80, min_dist=1, spread=3)
plot_umap(pca_with_pop, n_neighbors=100, min_dist=1, spread=3)
plot_umap(pca_with_pop, n_neighbors=120, min_dist=1, spread=3)


plot_umap(pca_with_pop, n_neighbors=10, min_dist=0.1, spread=1)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=0.2, spread=1)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=0.5, spread=1)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=0.7, spread=1)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=0.9, spread=1)
plot_umap(pca_with_pop, n_neighbors=10, min_dist=0.99, spread=1)

plot_umap(pca_with_pop, n_neighbors=15, min_dist=0.1, spread=1)
plot_umap(pca_with_pop, n_neighbors=15, min_dist=0.2, spread=1)
plot_umap(pca_with_pop, n_neighbors=15, min_dist=0.5, spread=1)
plot_umap(pca_with_pop, n_neighbors=15, min_dist=0.7, spread=1)
plot_umap(pca_with_pop, n_neighbors=15, min_dist=0.9, spread=1)
plot_umap(pca_with_pop, n_neighbors=15, min_dist=0.99, spread=1)


plot_umap(pca_with_pop, n_neighbors=20, min_dist=0.9, spread=1)
plot_umap(pca_with_pop, n_neighbors=25, min_dist=0.9, spread=1)
plot_umap(pca_with_pop, n_neighbors=30, min_dist=0.9, spread=1)
plot_umap(pca_with_pop, n_neighbors=50, min_dist=0.9, spread=1)

plot_umap(pca_with_pop, n_neighbors=50, min_dist=0.3, spread=1)
plot_umap(pca_with_pop, n_neighbors=100, min_dist=0.3, spread=1)
plot_umap(pca_with_pop, n_neighbors=200, min_dist=0.3, spread=1)
plot_umap(pca_with_pop, n_neighbors=300, min_dist=0.3, spread=1)

## UMAP analysis - Visualise subset of the data
## ============================================

## Fulani, West Africa, Central Africa

pca_with_pop %>% filter(region=="West Africa" | region == "Central Africa") -> pca_with_pop_subset

umap.config = umap.defaults

umap.config$n_neighbors=10
umap.config$min_dist=0.8

pca.umap = umap(pca_with_pop_subset[3:12],config=umap.config)

tibble(sample_id=pca_with_pop_subset$sample_id, 
       population_label=pca_with_pop_subset$population_label, 
       longitude=pca_with_pop_subset$longitude,
       latitude=pca_with_pop_subset$latitude,
       region=pca_with_pop_subset$region,
       country=pca_with_pop_subset$country,
       lifestyle=pca_with_pop_subset$lifestyle3,
       x=pca.umap$layout[,1], 
       y=pca.umap$layout[,2]) -> pca.umap.tibble

pca.umap.tibble %>%
  mutate(fulani=case_when(grepl("Fula", population_label) ~ "Fulani",
                          grepl("Halpularen", population_label, ignore.case = TRUE) ~"Fulani",
                          TRUE ~ region)) -> pca.umap.tibble

pca.umap.tibble$fulani <- factor(pca.umap.tibble$fulani, 
                                 levels=c("Fulani","West Africa","Central Africa"))

p <- ggplot(pca.umap.tibble, aes(x=x, y=y, color=fulani, shape=lifestyle)) +
  geom_point(alpha=0.8,size=1) +
  scale_colour_manual(values = c("#E31A1C","#33A02C","#B2DF8A"),name="Legend") +
  scale_shape_manual(values = c("circle filled","circle"),name="Legend") +
  theme_bw() +
  theme(axis.title = element_text(size = 12,face="bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size=8),
        legend.title=element_text(size=8,face="bold"),
        legend.text=element_text(size=8))

jpeg("src/rmd-reports/pca-report-Fulani_and_Reference_HD_DB/umap-fulani-west-central.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

## Fulani + other Africa (West, Central, East, North)

pca_with_pop %>% filter(region=="West Africa" | region == "Central Africa" | region == "North Africa" | region=="East Africa") -> pca_with_pop_subset

umap.config = umap.defaults

umap.config$n_neighbors=10
umap.config$min_dist=0.7

pca.umap = umap(pca_with_pop_subset[3:12],config=umap.config)

tibble(sample_id=pca_with_pop_subset$sample_id, 
       population_label=pca_with_pop_subset$population_label, 
       longitude=pca_with_pop_subset$longitude,
       latitude=pca_with_pop_subset$latitude,
       region=pca_with_pop_subset$region,
       country=pca_with_pop_subset$country,
       lifestyle=pca_with_pop_subset$lifestyle3,
       x=pca.umap$layout[,1], 
       y=pca.umap$layout[,2]) -> pca.umap.tibble

pca.umap.tibble %>%
  mutate(fulani=case_when(grepl("Fula", population_label) ~ "Fulani",
                          grepl("Halpularen", population_label, ignore.case = TRUE) ~"Fulani",
                          TRUE ~ region)) -> pca.umap.tibble

pca.umap.tibble$fulani <- factor(pca.umap.tibble$fulani, 
                                 levels=c("Fulani","West Africa","Central Africa","East Africa","North Africa"))

p <- ggplot(pca.umap.tibble, aes(x=x, y=y, color=fulani, shape=lifestyle)) +
  geom_point(alpha=0.8,size=1) +
  scale_colour_manual(values = c("#E31A1C","#33A02C","#B2DF8A","#FF7F00","blue"),name="Legend") +
  scale_shape_manual(values = c("triangle","circle filled","circle"),name="Legend") +
  theme_bw() +
  theme(axis.title = element_text(size = 12,face="bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size=8),
        legend.title=element_text(size=8,face="bold"),
        legend.text=element_text(size=8))

jpeg("src/rmd-reports/pca-report-Fulani_and_Reference_HD_DB/umap-fulani-africa.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()


## K-means

# totss <- c()
# 
# for(i in c(1:20)){
#   km <- kmeans(pca.umap.tibble[3:4], centers = i, nstart = 25)
#   totss <- append(totss, km$tot.withinss)
# }
# 
# plot(totss)
# 
# km.pca <- kmeans(pca.umap.tibble[3:4], centers = 7, nstart = 25)
# 
# pca.umap.tibble %>% mutate(km_cluster=km.pca$cluster) -> pca.umap.tibble

