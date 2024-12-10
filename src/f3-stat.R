library(tidyverse)
library(ggmap)
library(ggbeeswarm)
library(wesanderson)

setwd("~/h3a-fulani/")

read_tsv("data/h3a-fulani-analysis/f3-results/Fulani_and_Reference_HD_DB-bPR3inyJV7TfM/f3-statistics-processed.tsv",
         col_names = TRUE) -> f3

# Distribution of F3 vs. Z
ggplot(f3, aes(x=f_3,y=Z,color=factor(ifelse(Z < -3,1,0)))) + 
  geom_point(alpha=0.4,size=0.5) +
  scale_color_manual(values=c("dark blue","dark red")) +
  theme_bw() +
  xlab("F3") +
  ylab("Z") +
  theme(legend.position = "none")

ggplot(f3, aes(x=f_3)) + 
  geom_histogram(bins=1000,color="dark blue") +
  xlab("F3") +
  theme_bw()

ggplot(f3, aes(x=Z)) + 
  geom_histogram(bins=1000,color="dark blue") +
  xlab("Z") +
  theme_bw()


read_delim("data/h3a-fulani-source-data/samples/comparative_data_all_populations.tsv",
           col_names = TRUE) -> populations

f3 %>%
  dplyr::inner_join(populations %>% select(population_label, country, lifestyle3, latitude, longitude),
                    by=join_by(Target == population_label)) -> f3

f3$Target <- reorder(f3$Target, f3$longitude)

c_pal <- wes_palette("Zissou1", 18, type = "continuous")
shapes <- rep(c(1:18)[-11],2)[c(1:18)]

ggplot(f3, aes(x=f_3,y=Z,color=Target,shape=Target)) + 
  geom_point(alpha=0.8,size=1) +
  scale_colour_manual(values = c_pal,
                      name="Fulani group") +
  scale_shape_manual(values = shapes,name="Fulani group") +
  theme_bw() +
  xlab("F3") +
  ylab("Z")

ggplot(f3, aes(x=f_3,y=Z,color=factor(ifelse(Z < -3,1,0)))) + 
  geom_point(alpha=0.4,size=0.5) +
  scale_color_manual(values=c("dark blue","red")) +
  theme_bw() +
  xlab("F3") +
  ylab("Z") +
  facet_wrap(~Target) +
  theme(legend.position = "none")


# Add region and country to source populations
f3 %>%
  dplyr::inner_join((populations %>% 
                       select(population_label, region, country) %>% 
                       rename(region_s1=region) %>% 
                       rename(country_s1=country)),
                    by=join_by(Source_1==population_label)) %>%
  dplyr::inner_join((populations %>% 
                       select(population_label, region, country) %>% 
                       rename(region_s2=region) %>% 
                       rename(country_s2=country)),
                    by=join_by(Source_2==population_label)) -> f3

f3 %>%
  dplyr::mutate(region_s1 = ifelse(region_s1 == "Central Europe", "North Europe", region_s1),
                region_s2 = ifelse(region_s2 == "Central Europe", "North Europe", region_s2)) -> f3

f3 %>%
  dplyr::mutate(region_x = ifelse(region_s1 > region_s2, 
                                  paste0(region_s2," x ",region_s1),
                                  paste0(region_s1," x ",region_s2))) -> f3

f3 %>% mutate(Z_mod = ifelse(Z <= -3, Z, NA),
              f_3_mod = ifelse(Z <= -3, f_3, NA)) -> f3

signif_regions <- unique((f3 %>% filter(Z < -3))$region_x)

f3 %>%
  filter(region_x %in% signif_regions) %>%
  select(f_3_mod, Target, longitude, region_x) %>%
  distinct() %>%
  ggplot(aes(x=f_3_mod,y=reorder(Target, desc(longitude)),color=region_x)) + 
  geom_beeswarm(size=0.8, method="compactswarm", cex=0.4) +
  scale_color_manual(values=c("darkorange3","darkorange","darkgoldenrod4","darkkhaki",
                              "brown","darkolivegreen","magenta4","maroon2","blue4","dodgerblue2"),
                     name="Source regions") +
  theme_bw() +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  xlab("F3") +
  ylab("") +
  ggtitle("F3: Fulani; Source 1, Source 2") +
  theme(axis.text = element_text(size=10,face = "bold"),
        title = element_text(size=12, face="bold"))




View(f3)


f3 %>%
  filter(str_detect("Senegal_Halpularen",Target)) %>%
  ggplot(aes(x=Source_1,y=Source_2)) +
   geom_tile(aes(fill=Z_mod)) + 
  scale_fill_gradientn(colours=rainbow(5,end=0.7),na.value = "grey70") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90,hjust=1,size=8)
  )

f3 %>%
  ggplot(aes(x=Source_1,y=Source_2)) +
  geom_tile(aes(fill=Z_mod)) + 
  scale_fill_gradientn(colours=rainbow(5,end=0.7),na.value = "grey70") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  facet_wrap(~Target) -> p

pdf(file="results/f3-all-fulani-pop.pdf",width=12,height=10)

plot(p)

dev.off()

################################
## Print exchanges on the map ##
################################



f3 %>%
  select(Source_1, Target, Z_mod) %>%
  rename_at(vars(c("Source_1","Target","Z_mod")),
            ~c("source","target","z_mod")) %>%
  mutate(source_type = 1) -> f3_1

f3 %>%
  select(Source_2, Target, Z_mod) %>%
  rename_at(vars(c("Source_2","Target","Z_mod")),
            ~c("source","target","z_mod")) %>%
  mutate(source_type = 2) -> f3_2

bind_rows(f3_1,f3_2) -> f3_long

populations %>%
  select(Population_label, Latitude, Longitude) %>%
  inner_join(f3_long,by=c("Population_label" = "source")) %>%
  select(Population_label, target,z_mod,source_type, Latitude, Longitude) %>%
  rename_at(vars(c("Population_label","Latitude","Longitude")),
            ~c("source","latitude_source","longitude_source")) %>%
  inner_join(populations %>% select("Population_label","Latitude","Longitude"),by=c("target"="Population_label")) %>%
  rename_at(vars(c("Latitude","Longitude")),
            ~c("latitude_target","longitude_target")) %>%
  group_by(source, target, latitude_source,longitude_source,latitude_target,longitude_target) %>%
  summarise(z_mod = mean(z_mod,na.rm = TRUE) ) %>%
  group_by(target) %>%
  mutate(n_connections = sum(ifelse(!is.na(z_mod),1,0)), avg_z = mean(z_mod,na.rm=TRUE)) -> f3_coords



#
mn = min(f3_coords$longitude_target)
mx = max(f3_coords$longitude_target)
ln = length(unique(f3_coords$target))
st = ( mx - mn ) / ln

target_positions = tibble(data.frame(target = unique((f3_coords %>% arrange(longitude_target))$target), 
                                     longitude = unique((f3_coords %>% arrange(longitude_target))$longitude_target), 
                                     latitude = unique((f3_coords %>% arrange(longitude_target))$latitude_target), 
                                     x = seq(mn, mx-st, st), y = rep(3.0,ln)))



p <- ggplot(f3_coords, aes(x=longitude_source,y=latitude_source)) +
  geom_segment(data=f3_coords %>% filter(!is.na(z_mod)) %>% arrange(desc(z_mod)), 
             aes(x = longitude_source, y=latitude_source, xend=longitude_target,yend=latitude_target,colour=z_mod)) +
  geom_point() +
  geom_point(data=f3_coords, aes(x=longitude_target,y=latitude_target,size=n_connections, fill=avg_z), shape=21, color="red") +
  geom_text(data=target_positions, 
            aes(x=x, y=y, label=target), hjust = 0, vjust=0.5, color="red", angle=-45) +
  geom_segment(data=target_positions, aes(x=x, y=y, xend=longitude, yend=latitude), color="red") +
  scale_color_gradientn(colours=rainbow(5,end=0.7,alpha=0.7)) +
  scale_fill_gradientn(colours=rainbow(5,end=0.7,alpha=0.7)) +
  ylim(-10,55) +
  theme_bw()


pdf(file="results/f3-all-fulani-pop-on-map.pdf",width=14,height=10)

plot(p)

dev.off()


read_delim("docs/populations/fulani-reference-populations-by-pca-2.tsv",
           col_names = FALSE, delim = "\t") %>%
  rename_at(vars(as.character(paste0("X",c(1:3)))),
            ~c("population","geography","language")) -> populations

f3_coords %>% 
  inner_join(populations, by=c("source" = "population")) %>%
  group_by(language) %>%
  mutate(longitude_source_avg = mean(longitude_source,na.rm=TRUE),
         latitude_source_avg = mean(latitude_source,na.rm=TRUE),)-> f3_coords_pops


fulani_pops = unique(f3_coords_pops$target)

pdf(file="results/f3-graph-by-pop.pdf", width=8,height=6)

for(pop in fulani_pops){

  p = ggplot(f3_coords_pops %>% filter(str_detect(pop,target)), aes(x=longitude_source,
                             y=latitude_source,fill=factor(language))) +
    geom_segment(data  = (f3_coords_pops %>% filter(str_detect(pop,target) & !is.na(z_mod))), 
                 aes(x=longitude_source,xend = longitude_target, y=latitude_source, 
                     yend=latitude_target,color=z_mod),alpha=0.6,size=1) +
    geom_point(shape=21,color="black",size=3) +
    geom_point(data = (f3_coords_pops %>%  
                         select(target, longitude_target, latitude_target, n_connections) %>% 
                         unique()), 
               mapping = aes(x=longitude_target,y=latitude_target),color="dark grey",size=3) +
    theme_bw() +
    scale_color_gradientn(colours=rainbow(5,end=0.7,alpha=0.7))
  
  plot(p)

}

dev.off()


  
### New

eastern = c("Mali_Fulani_InnerDelta",
            "Mali_Fulani_Diafarabe",
            "BurkinaFaso_Fulani_Tindangou",
            "BurkinaFaso_Fulani_Banfora",
            "Cameroon_Fulani_Tcheboua",
            "BurkinaFaso_Fulani_Ziniare")

western = c("Senegal_Halpularen",
            "Guinea_Fulani",
            "Gambia_Fula",
            "Senegal_Fulani_Linguere",
            "Mauritania_Fulani_Assaba")

wodaabe = c("Niger_Fulani_Abalak",
            "Niger_Fulani_Ader",
            "Niger_Fulani_Balatungur",
            "Niger_Fulani_Diffa",
            "Niger_Fulani_Zinder",
            "Chad_Fulani_Bongor",
            "Chad_Fulani_Linia")

f3 %>%
  mutate(fulani_group=case_when(grepl(paste(wodaabe,collapse="|"), Target) ~ "Fulani Wodaabe",
                                grepl(paste(eastern,collapse="|"), Target, ) ~"Fulani Eastern", 
                                grepl(paste(western,collapse="|"), Target, ) ~"Fulani Western",
                                TRUE ~ "Non-Fulani")) -> f3


read_tsv("data/samples/comparative_data_all_populations.tsv") -> populations

f3 %>% inner_join(populations %>% select(population_label,region),
                  by=c("Source_1" = "population_label")) %>%
  rename(region_source_1=region) %>%
  inner_join(populations %>% select(population_label,region),
             by=c("Source_2" = "population_label")) %>%
  rename(region_source_2=region) %>%
  inner_join(populations %>% select(population_label,longitude, latitude),
             by=c("Target" = "population_label")) -> f3

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


f3 %>%
  filter((region_source_1=="West Africa" & region_source_2=="East Africa") |
  (region_source_1=="West Africa" & region_source_2=="Central Africa") |
  (region_source_1=="West Africa" & region_source_2=="North Africa") |
  (region_source_1=="East Africa" & region_source_2=="North Africa") |
  (region_source_1=="East Africa" & region_source_2=="Central Africa") |
  (region_source_1=="North Africa" & region_source_2=="Central Africa")) %>%
  ggplot(aes(x=reorder(factor(Target),longitude), y=Z_mod, color=paste(region_source_1,region_source_2)))  +
  geom_beeswarm(size=1) +
  xlab("Fulani populations") +
  ylab("z-score") +
  scale_colour_manual(values = gg_color_hue(5),name="Admixture") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=8),
        axis.title = element_text(size = 12,face="bold"),
        axis.text.y = element_text(size = 8),
        legend.title=element_text(size=8,face="bold"),
        legend.text=element_text(size=8)) -> p
  
jpeg("src/rmd-reports/f3/f3-stat-summary-2.jpg",
     units="in",width = 8, height = 6, res = 300, quality=100)
p
dev.off()
  
