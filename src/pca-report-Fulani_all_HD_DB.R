########################################################
# Functions for PCA report (Fulani_all_HD_DB.Rmd)
# Author: Vaclav Janousek
# Date: 23/03/2022
########################################################

library(tidyverse)
library(umap)
library(cluster)
library(ggrepel)
library(ggbeeswarm)

setwd("~/h3a-fulani/")

data="data/pca-results/Fulani_all_HD_DB-164132-230322/Fulani_all_HD_DB.pca.evec"

## Load Data
## =========

pc <- as.character(paste0("PC",c(1:10)))

read_table(data, 
           col_names = FALSE, skip=1) %>%
  select(c(1:11)) %>%
  rename_at(vars(as.character(paste0("X",c(1:11)))),
            ~c("id",pc)) %>% 
  separate(id, into=c("population_label","sample_id"), sep=":") -> pca

read_tsv("data/samples/comparative_data_all_populations.tsv") -> populations

pca %>% 
  group_by(population_label) %>%
  summarize(n=length(sample_id),
            PC1=mean(PC1),
            PC2=mean(PC2),
            PC3=mean(PC3),
            PC4=mean(PC4),) %>%
  inner_join(populations %>% select(population_label, longitude, latitude), 
             by=c("population_label" = "population_label")) -> pca_by_pop

pca %>% 
  inner_join(populations %>% select(population_label, longitude, latitude), 
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

jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/pca-eigen-value-Fulani_all_HD_DB.jpg",
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

pca_with_pop$population_label <- reorder(pca_with_pop$population_label, pca_with_pop$longitude)

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC2, color=population_label, shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = terrain.colors(17),name="Population") +
  scale_shape_manual(values = rep(shape_names,2)[c(1:17)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC2")

jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/pca-pc1-pc2-Fulani_all_HD_DB.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC1, y=PC3, color=population_label, shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = terrain.colors(17),name="Population") +
  scale_shape_manual(values = rep(shape_names,2)[c(1:17)],name="Population") +
  theme_bw() +
  ggtitle("PC1 x PC3")

jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/pca-pc1-pc3-Fulani_all_HD_DB.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()

p <- ggplot(pca_with_pop, aes(x=PC2, y=PC3, color=population_label, shape=population_label)) +
  geom_point() +
  scale_colour_manual(values = terrain.colors(17),name="Population") +
  scale_shape_manual(values = rep(shape_names,2)[c(1:17)],name="Population") +
  theme_bw() +
  ggtitle("PC2 x PC3")

jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/pca-pc2-pc3-Fulani_all_HD_DB.jpg",
     units="in",width = 8, height = 5, res = 300, quality=100)
p
dev.off()


## UMAP analysis
## =============

umap.config = umap.defaults

umap.config$n_neighbors=10
umap.config$min_dist=0.1

pca.umap = umap(pca_with_pop[3:12],config=umap.config)


tibble(sample_id=pca_with_pop$sample_id, 
       population_label=pca_with_pop$population_label, 
       longitude=pca_with_pop$longitude,
       latitude=pca_with_pop$latitude,
       x=pca.umap$layout[,1], 
       y=pca.umap$layout[,2]) -> pca.umap.tibble

#pca.umap.tibble %>%
#  mutate(fulani=case_when(grepl("Fula", population_label) ~ "fulani",
#                          grepl("Halpularen", population_label, ignore.case = TRUE) ~"fulani",
#                          TRUE ~ "non-fulani")) -> pca.umap.tibble

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

pca.umap.tibble$population_label <- reorder(pca.umap.tibble$population_label, pca.umap.tibble$longitude)

p <- ggplot(pca.umap.tibble, aes(x=x, y=y, color=population_label, shape=population_label)) +
  geom_point(alpha=0.8,size=3) +
  scale_colour_manual(values = terrain.colors(17),name="Population") +
  scale_shape_manual(values = rep(shape_names,2)[c(1:17)],name="Population") +
  theme_bw() +
  theme(axis.title = element_text(size = 12,face="bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size=8),
        legend.title=element_text(size=8,face="bold"),
        legend.text=element_text(size=8))


jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/umap-Fulani_all_HD_DB.jpg",
     units="in",width = 8, height = 6, res = 300, quality=100)
p
dev.off()

## Summary statistics of PCs
## =========================

pointsize=0.5

p1 = ggplot(pca_with_pop, aes(x=population_label, y=PC1, color=population_label)) +
  geom_beeswarm(size=pointsize) +
  scale_color_manual(values = terrain.colors(17),name="Population") +
  xlab("") +
  theme_bw() +
  theme(axis.title = element_text(size = 10,face="bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust=1, hjust=1,size=6),
        legend.position="none")

p2 = ggplot(pca_with_pop, aes(x=population_label, y=PC2, color=population_label)) +
  geom_beeswarm(size=pointsize) +
  scale_color_manual(values = terrain.colors(17),name="Population") +
  xlab("") +
  theme_bw() +
  theme(axis.title = element_text(size = 10,face="bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust=1, hjust=1,size=6),
        legend.position="none")

p3 = ggplot(pca_with_pop, aes(x=population_label, y=PC3, color=population_label)) +
  geom_beeswarm(size=pointsize) +
  scale_color_manual(values = terrain.colors(17),name="Population") +
  xlab("") +
  theme_bw() +
  theme(axis.title = element_text(size = 10,face="bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust=1, hjust=1,size=6),
        legend.position="none")

p4 = ggplot(pca_with_pop, aes(x=population_label, y=PC4, color=population_label)) +
  geom_beeswarm(size=pointsize) +
  scale_color_manual(values = terrain.colors(17),name="Population") +
  xlab("") +
  theme_bw() +
  theme(axis.title = element_text(size = 10,face="bold"),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust=1, hjust=1,size=6),
        legend.position="none")

jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/pca-boxplot-Fulani_all_HD_DB.jpg",
     units="in",width = 8, height = 6, res = 300, quality=100)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
dev.off()



## Correlation PCs with geography
## ==============================

##

## Correlation with longitude
## --------------------------

pointsize=0.6
linesize=0.6
textsize=3

# PC1
crt = cor.test(pca_with_pop$PC1, 
               pca_with_pop$longitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=max(pca_with_pop$longitude), ymin=min(pca_with_pop$PC2))

p1lon = ggplot(pca_with_pop, aes(x=longitude,y=PC1)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=linesize) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=1, vjust=0, fontface='bold', size=textsize) +
  theme_bw()
  

# PC2
crt = cor.test(pca_with_pop$PC2, 
               pca_with_pop$longitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=max(pca_with_pop$longitude), ymin=max(pca_with_pop$PC2))

p2lon = ggplot(pca_with_pop, aes(x=longitude,y=PC2)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=linesize) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=1, vjust=1, fontface='bold', size=textsize) +
  theme_bw()

# PC3
crt = cor.test(pca_with_pop$PC3, 
               pca_with_pop$longitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=max(pca_with_pop$longitude), ymin=min(pca_with_pop$PC3))

p3lon = ggplot(pca_with_pop, aes(x=longitude,y=PC3)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=linesize) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=1, vjust=0, fontface='bold', size=textsize) +
  theme_bw()

# PC4
crt = cor.test(pca_with_pop$PC4, 
               pca_with_pop$longitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=max(pca_with_pop$longitude), ymin=min(pca_with_pop$PC4))

p4lon = ggplot(pca_with_pop, aes(x=longitude,y=PC4)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=1) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=1, vjust=0, fontface='bold', size=textsize) +
  theme_bw()


## Correlation with latitude
## -------------------------

# PC1
crt = cor.test(pca_with_pop$PC1, 
               pca_with_pop$latitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=min(pca_with_pop$latitude), ymin=min(pca_with_pop$PC2))

p1lat = ggplot(pca_with_pop, aes(x=latitude,y=PC1)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=linesize) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=0, vjust=0, fontface='bold', size=textsize) +
  theme_bw()

# PC2
crt = cor.test(pca_with_pop$PC2, 
               pca_with_pop$latitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=min(pca_with_pop$latitude), ymin=max(pca_with_pop$PC2))

p2lat = ggplot(pca_with_pop, aes(x=latitude,y=PC2)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=linesize) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=0, vjust=1, fontface='bold', size=textsize) +
  theme_bw()

# PC3
crt = cor.test(pca_with_pop$PC3, 
               pca_with_pop$latitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=min(pca_with_pop$latitude), ymin=min(pca_with_pop$PC3))

p3lat = ggplot(pca_with_pop, aes(x=latitude,y=PC3)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=linesize) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=0, vjust=0, fontface='bold', size=textsize) +
  theme_bw()

# PC4
crt = cor.test(pca_with_pop$PC4, 
               pca_with_pop$latitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=min(pca_with_pop$latitude), ymin=min(pca_with_pop$PC4))

p4lat = ggplot(pca_with_pop, aes(x=latitude,y=PC4)) +
  geom_point(color="dark blue",size=pointsize) +
  geom_smooth(method="lm",se = FALSE, color="red",size=linesize) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=0, vjust=0, fontface='bold', size=textsize) +
  theme_bw()


jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/pca-geom-cor-Fulani_all_HD_DB.jpg",
     units="in",width = 8, height = 10, res = 300, quality=100)
grid.arrange(p1lon, p1lat, p2lon, p2lat, p3lon, p3lat, p4lon, p4lat, ncol = 2, nrow = 4)
dev.off()

## Separately PC2 without outliers
## ===============================

pca_with_pop %>% filter(PC2 > 0.1) -> pca_with_pop_pc2filtered

crt = cor.test(pca_with_pop_pc2filtered$PC2, 
               pca_with_pop_pc2filtered$longitude, method="pearson")

cortest = data.frame(r=crt$estimate, pval=crt$p.value, xmax=max(pca_with_pop_pc2filtered$longitude), ymin=min(pca_with_pop_pc2filtered$PC2))

p2 = ggplot(pca_with_pop_pc2filtered, aes(x=longitude,y=PC2)) +
  geom_point(color="dark blue",size=0.6) +
  geom_smooth(method="lm",se = FALSE, color="red",size=0.6) +
  geom_text(data=cortest, mapping=aes(x=xmax, y=ymin, 
                                      label=paste("r=",round(r,3),"\n","p-value=",round(pval,3),sep = "")),
            nudge_x = 0, hjust=1, vjust=0, fontface='bold', size=2) +
  theme_bw() +
  theme(axis.title = element_text(size = 8,face="bold"),
        axis.text.y = element_text(size = 6),
        axis.text.x = element_text(size=6),
        legend.position="none")



jpeg("src/rmd-reports/pca-report-Fulani_all_HD_DB_files/pca-geom-cor-p2-Fulani_all_HD_DB.jpg",
     units="in",width = 3, height = 2.5, res = 300, quality=100)
p2
dev.off()

