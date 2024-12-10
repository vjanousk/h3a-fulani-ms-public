library(tidyverse)

setwd("~/DocumentsResearch/projects/fulani-2021/h3a-fulani/")

library(ggplot2)
library(forcats)
library(ggthemes)
library(patchwork)

read_csv("data/admixture/Fulani_and_ref_admix_plot/Fulani_and_Reference_HD_DB_3k.csv",
         col_names = TRUE) -> k3

k3plot <-
  ggplot(k3, aes(factor(sampleID), prob, fill = factor(PopGroup))) +
  geom_col(color=NA,size=0,width = 1.2) +
  facet_grid(~fct_inorder(Pop), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=3", y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.05, "lines"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0, 1, 0, 1)
  ) +
  scale_fill_gdocs(guide = "none")
#k3plot

read_csv("data/admixture/Fulani_and_ref_admix_plot/Fulani_and_Reference_HD_DB_5k.csv",
         col_names = TRUE) -> k5
k5plot <-
  ggplot(k5, aes(factor(sampleID), prob, fill = factor(PopGroup))) +
  geom_col(color=NA,size=0,width = 1.2) +
  facet_grid(~fct_inorder(Pop), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=5", y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.05, "lines"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0, 1, 0, 1)
  ) +
  scale_fill_gdocs(guide = "none")
#k5plot

read_csv("data/admixture/Fulani_and_ref_admix_plot/Fulani_and_Reference_HD_DB_7k.csv",
         col_names = TRUE) -> k7
k7plot <-
  ggplot(k7, aes(factor(sampleID), prob, fill = factor(PopGroup))) +
  geom_col(color=NA,size=0,width = 1.2) +
  facet_grid(~fct_inorder(Pop), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=7", y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.05, "lines"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0, 1, 0, 1)
  ) +
  scale_fill_gdocs(guide = "none")
#k7plot

read_csv("data/admixture/Fulani_and_ref_admix_plot/Fulani_and_Reference_HD_DB_9k.csv",
         col_names = TRUE) -> k9
k9plot <-
  ggplot(k9, aes(factor(sampleID), prob, fill = factor(PopGroup))) +
  geom_col(color=NA,size=0,width = 1.2) +
  facet_grid(~fct_inorder(Pop), switch = "x", scales = "free", space = "free") +
  theme_minimal() + labs(x = "", title = "K=9", y = "") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.05, "lines"),
    axis.text.x = element_blank(),
    panel.grid = element_blank(),
    strip.text.x = element_blank(),
    plot.margin = margin(0, 1, 0, 1)
  ) +
  scale_fill_gdocs(guide = "none")
#k9plot

popnames <-
  ggplot(k7, aes(factor(sampleID), 0)) +
  facet_grid(~fct_inorder(Pop), switch = "x", scales = "free", space = "free") +
  theme_minimal() + 
  xlab("Populations") +
  ylab("") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(expand = expand_scale(add = 1)) +
  theme(
    panel.spacing.x = unit(0.05, "lines"),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    strip.text.x = element_text(angle = 90,hjust=1,size=8),
    plot.margin = margin(0, 1, 2, 1)
  ) +
  scale_fill_gdocs(guide = "none")
#popnames


#k3plot + k5plot + k7plot + popnames + plot_layout(ncol = 1)


layout <- c(
  area(t = 1, l = 1, b = 2, r = 5),
  area(t = 3, l = 1, b = 4, r = 5),
  area(t = 5, l = 1, b = 6, r = 5),
  area(t = 7, l = 1, b = 8, r = 5),
  area(t = 7, l = 1, b = 8, r = 5)
)

pdf(file = "results/Fulani_and_Reference_HD_DB-admixture.pdf",width = 12,height = 8)
k3plot + k5plot + k7plot + k9plot + popnames + plot_layout(design = layout)
dev.off()

jpeg(file = "results/Fulani_and_Reference_HD_DB-admixture.jpg",width = 2400,height = 1600)
k3plot + k5plot + k7plot + k9plot + popnames + plot_layout(design = layout)
dev.off()

