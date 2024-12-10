library(tidyverse)
library(ecodist)

# Prepare Fst for mantell test 

setwd("~/h3a-fulani/")

read_tsv("data/pca-results/Fulani_and_Reference_HD_DB_fst.tsv",
         col_names = FALSE) %>%
  rename_at(vars(as.character(paste0("X",c(1:5)))),
            ~c("population1","population2","fst","std","z-score")) %>% 
  filter(!str_detect(population1,"Europe") & !str_detect(population2,"Europe")) -> fst

fst %>% 
  select(population1, population2, fst) %>%
  spread(population2,fst) -> fst_pivot


as.matrix(fst_pivot %>% select(c(2:length(names(fst_pivot))))) -> fst_mtrx

rownames(fst_mtrx) <- as.list(fst_pivot[1])$population1

write.table(t(fst_mtrx),file="data/mantel/fst-matrix.txt",sep = "\t")

##

column_names = unlist(t(as.list(read.table("data/mantel/genetic-distance.txt",sep="\t",row.names=1)[1,])))

dist_genet = read.table("data/mantel/genetic-distance.txt",sep="\t",row.names=1,skip=1)
colnames(dist_genet) <- column_names

dist_genet <- as.dist(dist_genet)

dist_geog = read.table("data/mantel/genographic-distance.txt",sep="\t",row.names=1,skip=1)
colnames(dist_geog) <- column_names

dist_geog <- as.dist(dist_geog)

"
Mantel test:
- mantel separately only Fulani
- then mantel for all other African populations (sedentary + pastoralists)
- all others but not including pastoralists (only sedentary)
"

# Mantel test
mantel(dist_geog ~ dist_genet)

# Multiple regression on distance matrices
MRM(dist_geog ~ dist_genet, nperm=10000,method="linear",mrank=TRUE)

