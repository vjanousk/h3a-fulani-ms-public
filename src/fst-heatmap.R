library(tidyverse)
library(gplots)

setwd("~/DocumentsResearch/projects/fulani-2021/h3a-fulani/")

read_tsv("data/pca-results/Fulani_and_Reference_HD_DB_fst.tsv",
         col_names = FALSE) %>%
  rename_at(vars(as.character(paste0("X",c(1:5)))),
            ~c("population1","population2","fst","std","z-score"))-> fst

View(fst)

read_tsv("data/pca-results/Fulani_and_Reference_HD_DB-populations.tsv",
         col_names=FALSE) %>%
  rename_at(vars(as.character(paste0("X",c(1:2)))),
            ~c("population","n")) -> populations

View(populations)

bind_rows(fst %>%
            select(population1),
          fst %>%
            select(population2) %>%
            rename(population1 = population2)) %>%
  unique() %>%
  mutate(population2 = population1) %>%
  add_column(fst = NA) %>%
  add_column(std = NA) %>%
  add_column(`z-score` = NA) -> diagonal

fst %>% bind_rows(diagonal) %>% 
  inner_join(populations, by=c("population1" = "population")) %>%
  inner_join(populations, by=c("population2" = "population")) %>% 
  filter(n.x >= 15) %>% 
  filter(n.y >= 15) -> fst_full

fst_full %>% 
  select(population1, population2, fst) %>% 
  spread(population2,fst) -> fst_wide

# Create Fst symmetrical matrix with NAs on diagonal
fst_mat = as.matrix(fst_wide[-1])
rownames(fst_mat) = fst_wide[1]$population1

fst_mat[is.na(fst_mat)] <- 0
diag(fst_mat) <- NA

fst_mat_full = t(fst_mat) + fst_mat

dim(fst_mat_full)

View(fst_mat_full)

fula = ifelse(grepl("Fula",colnames(fst_mat_full)),
              ifelse(grepl("BurkinaFaso",colnames(fst_mat_full)),"#009900",
                     ifelse(grepl("Cameroon",colnames(fst_mat_full)),"#ff0000",
                            ifelse(grepl("Chad",colnames(fst_mat_full)),"#990000",
                                   ifelse(grepl("Gambia",colnames(fst_mat_full)),"#00ff33",
                                          ifelse(grepl("Guinea",colnames(fst_mat_full)),"#009900",
                                                 ifelse(grepl("Mali",colnames(fst_mat_full)),"#99cc00",
                                                        ifelse(grepl("Mauritania",colnames(fst_mat_full)),"#00cc66",
                                                               ifelse(grepl("Niger",colnames(fst_mat_full)),"#996600","#666600")))))))),
              ifelse(grepl("Halpularen",colnames(fst_mat_full)),"#666600","dark grey"))


pdf(file = "results/fst-heatmap-full-reference.pdf",width = 10,height = 10)

# Note: Different clustering algorithms produce slightly different clustering, 
# e.g. ward.D2 puts couple of populations on distant branch.
# ward.D produce ~ the best results?

heatmap.2(fst_mat_full,
          dendrogram = "both",
          hclustfun=function(x) hclust(x, method = "ward.D"),
          col=hcl.colors(n=50,palette="viridis",rev=TRUE),
          trace="none",
          tracecol = "red",
          cexCol=0.7,
          cexRow=0.7,
          margins=c(10,10),
          ColSideColors=fula,
          RowSideColors=fula)

dev.off()

fula = ifelse(grepl("Fula",colnames(fst_mat_full)),
              ifelse(grepl("BurkinaFaso",colnames(fst_mat_full)),"#009900",
                     ifelse(grepl("Cameroon",colnames(fst_mat_full)),"#ff0000",
                            ifelse(grepl("Chad",colnames(fst_mat_full)),"#990000",
                                   ifelse(grepl("Gambia",colnames(fst_mat_full)),"#00ff33",
                                          ifelse(grepl("Guinea",colnames(fst_mat_full)),"#009900",
                                                 ifelse(grepl("Mali",colnames(fst_mat_full)),"#99cc00",
                                                        ifelse(grepl("Mauritania",colnames(fst_mat_full)),"#00cc66",
                                                               ifelse(grepl("Niger",colnames(fst_mat_full)),"#996600","#666600")))))))),
              ifelse(grepl("Halpularen",colnames(fst_mat_full)),"#666600","black"))

library(ape)

tree = nj(fst_mat_full)

pdf(file = "results/fst-nj-full-reference.pdf",width = 10,height = 10)

plot(tree,type= "phylogram",cex=0.5,no.margin = TRUE,label.offset=0.001,tip.color=fula)

dev.off()

#####

library(tidyverse)
library(pheatmap)

setwd("~/DocumentsResearch/projects/fulani-2021/h3a-fulani/")

read_tsv("data/pca-results/Fulani_and_Reference_HD_DB_fst.tsv",
         col_names = FALSE) %>%
  rename_at(vars(as.character(paste0("X",c(1:5)))),
            ~c("pop_fulani","population","fst","std","z-score")) %>%
  filter(str_detect(pop_fulani, "Fula") | str_detect(pop_fulani,"Halpularen")) %>% 
  filter(!str_detect(population,"Fula") & !str_detect(population,"Halpularen")) -> fst1

read_tsv("data/pca-results/Fulani_and_Reference_HD_DB_fst.tsv",
         col_names = FALSE) %>%
  rename_at(vars(as.character(paste0("X",c(1:5)))),
            ~c("population","pop_fulani","fst","std","z-score")) %>%
  filter(str_detect(pop_fulani,"Fula") | str_detect(pop_fulani,"Halpularen")) %>% 
  filter(!str_detect(population,"Fula") & !str_detect(population,"Halpularen")) %>%
  select(pop_fulani, population, fst, std, `z-score`) -> fst2

bind_rows(fst1,fst2) -> fst

read_delim("docs/populations/fulani-reference-populations-by-pca-2.tsv",
           col_names = FALSE, delim = "\t") %>%
  rename_at(vars(as.character(paste0("X",c(1:3)))),
            ~c("population","geography","language")) -> populations

populations$group = populations$language

fst %>% 
  inner_join(populations, by=c("population" = "population")) -> fst_pops

fst_pops %>%  pivot_wider(id_cols=population, names_from="pop_fulani", values_from="fst") -> fst_pivot

as.matrix(fst_pivot %>% select(c(2:length(names(fst_pivot))))) -> fst_mtrx

rownames(fst_mtrx) <- as.list(fst_pivot[1])$population


fst_pops %>% select(population, language) %>% 
  unique() %>% 
  column_to_rownames("population") -> annotation


pheatmap(fst_mtrx,scale="none",clustering_method="ward.D2",annotation_row=annotation,
         fontsize=6,
         angle_col=45,
         filename="results/fst-heatmap-full-reference.pdf",
         width=6,height=7)



