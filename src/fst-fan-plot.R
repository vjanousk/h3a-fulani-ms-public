library(tidyverse)

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

fulani_pops = unique(fst_pops$pop_fulani)

pdf(file="results/fan-like-plots-fulani-language.pdf", width=10,height=8)

for(p in fulani_pops){

  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(as.factor(fst_pops$group)), ncol(fst_pops)) )
  colnames(to_add) <- colnames(fst_pops)
  to_add$group <- rep(levels(as.factor(fst_pops$group)), each=empty_bar)
  
  fst_pops %>% 
    filter(str_detect(pop_fulani, p)) %>%
    rbind(to_add) %>%
    arrange(group) -> fst_pops_subset
  
  fan_plot(fst_pops_subset, title=p)
  
}

dev.off()



fan_plot <- function(fst_pops_subset,title=NA){
  
  fst_pops_subset$id <- seq(1, nrow(fst_pops_subset))
  
  label_data <- fst_pops_subset
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  fst_threshold = 0.03 # median(label_data$fst,na.rm=TRUE)
  
  label_data$hjust <- ifelse( angle < -90, 
                              ifelse( fst_threshold > label_data$fst, 1, 0), 
                              ifelse( fst_threshold > label_data$fst, 0, 1))
  label_data$y <- ifelse( angle < -90, 
                          ifelse( label_data$hjust==1, label_data$fst+label_data$fst/10, label_data$fst-label_data$fst/10), 
                          ifelse( label_data$hjust==1, label_data$fst-label_data$fst/10, label_data$fst+label_data$fst/10))
  
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  p <- ggplot(fst_pops_subset,aes(x=as.factor(id), y=fst, fill=group)) +
    geom_bar(stat="identity",alpha=0.7) + 
    geom_hline(yintercept = seq(0, 0.06, by = 0.01), colour = "grey90", size = 0.25) +
    theme_bw() +
    theme(
      legend.position = "right",
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      panel.border=element_rect(color="white"),
      plot.background = element_rect(fill = "white",color="white")
    ) +
    coord_polar(start=0,clip="on") +
    geom_text(data=label_data,aes(x=id, y=y, label=population,hjust=hjust), 
              color="black", 
              fontface="bold",
              alpha=0.6, 
              size=2.5, 
              angle=label_data$angle, 
              inherit.aes = FALSE ) +
    ggtitle(title) +
    scale_y_continuous(breaks=seq(0, 0.06, by = 0.01))
  
  p_build = ggplot_build(p)
  
  p_build[["layout"]][["panel_params"]][[1]][["r.range"]][1] <- -0.01
  p_build[["layout"]][["panel_params"]][[1]][["r.range"]][2] <- 0.05
  p2 <- ggplot_gtable(p_build)
  
  plot(p2)
  
}


