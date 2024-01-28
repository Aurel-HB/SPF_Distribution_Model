#data_model <- readRDS(paste(local_path,"code/data_model/sard_tot.rds", sep = ""))
data_model$season <- as.factor(data_model$season)
data_model$season <- factor(data_model$season, levels = c("spring","summer","fall"))


#vision of all the year####
for (saison in c("spring", "fall")){
  
  plot.binom <- ggplot(study_domain_sf) + geom_sf() +
    geom_point(data = data_model %>% filter(indice == 0) %>%
                 filter(season == saison),
               aes(x = Xgd, y = Ygd, color = indice), size = 0.3) +
    geom_point(data = data_model %>% filter(indice > 0) %>%
                 filter(season == saison),
               aes(x = Xgd, y = Ygd, color = indice), size = 0.5) +
    labs(x = "", y = "", fill = "Occurence", 
         title = "Carte de présence de la sardine dans le Golfe de Gascogne") +
    facet_wrap( ~ year, ncol = 5, nrow = 3) +
    scale_color_viridis_c()+
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    theme_bw()
  ggsave(plot = plot.binom, paste(path.output,"input_map/","binom_data",saison,".png", sep = ""), width = 16, height = 9 )
  
  
  plot.logn <- ggplot(study_domain_sf) + geom_sf() +
    geom_point(data = data_model %>% filter(!is.na(biomass)) %>% 
                 filter(season == saison), 
               aes(x = Xgd, y = Ygd, color = biomass), size = 1) +
    labs(x = "", y = "", fill = "Biomasse en kg", 
         title = "Carte de densité de la sardine dans le Golfe de Gascogne") +
    facet_wrap( ~ year, ncol = 5, nrow = 3) +
    scale_color_viridis_c(trans = "log10")+
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    theme_bw()
  ggsave(plot = plot.logn, paste(path.output,"input_map/","logn_data",saison,".png", sep = ""), width = 16, height = 9 )
  
  
  plot.ppp <- ggplot(study_domain_sf) + geom_sf() +
    geom_point(data = data_model %>% filter(is.na(indice)) %>%
                 filter(season == saison),
               aes(x = Xgd, y = Ygd, color = presence), size = 1) +
    labs(x = "", y = "", fill = "Occurence", 
         title ="Carte de présence de la sardine dans le Golfe de Gascogne par la Pêche") +
    facet_wrap( ~ year, ncol = 5, nrow = 3) +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    scale_color_viridis_c()+
  ggsave(plot = plot.ppp, paste(path.output,"input_map/","ppp_data",saison,".png", sep = ""), width = 16, height = 9 )
}

plot.ppp <- ggplot(study_domain_sf) + geom_sf() +
  geom_point(data = data_model %>% filter(is.na(indice)) %>%
               filter(season == "summer"),
             aes(x = Xgd, y = Ygd, color = presence), size = 1) +
  labs(x = "", y = "", fill = "Occurence", 
       title ="Carte de présence de la sardine dans le Golfe de Gascogne par la Pêche") +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  scale_color_viridis_c()+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme_bw()
ggsave(plot = plot.ppp, paste(path.output,"input_map/","ppp_data","summer",".png", sep = ""), width = 16, height = 9 )


#vision of one specific year ####
plot.binom.year <- ggplot(study_domain_sf) + geom_sf() +
  geom_point(data = data_model %>% filter(!is.na(indice)) %>%
               filter(year == 2017),
             aes(x = Xgd, y = Ygd, color = indice), size = 1) +
  labs(x = "", y = "", fill = "Presence/absence", 
       title = "") +
  facet_wrap( ~ season, ncol = 5, nrow = 3) +
  scale_color_viridis_c()+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme_bw()
ggsave(plot = plot.binom.year, paste(path.output,"input_map/","binom_data_",2017,".png", sep = ""), width = 16, height = 9 )




plot.logn.year <- ggplot(study_domain_sf) + geom_sf() +
  geom_point(data = data_model %>% filter(!is.na(biomass)) %>%
               filter(year == 2017),
             aes(x = Xgd, y = Ygd, color = biomass), size = 2) +
  labs(x = "", y = "", fill = "Biomass", 
       title = "") +
  facet_wrap( ~ season, ncol = 5, nrow = 3) +
  scale_color_viridis_c(trans = "log10")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme_bw()
ggsave(plot = plot.logn.year, paste(path.output,"input_map/","logn_data_",2017,".png", sep = ""), width = 16, height = 9 )




plot.ppp.year <- ggplot(study_domain_sf) + geom_sf() +
  geom_point(data = data_model %>% filter(!is.na(presence)) %>%
               filter(year == 2017),
             aes(x = Xgd, y = Ygd, color = presence), size = 1) +
  labs(x = "", y = "", fill = "Presence only", 
       title = "") +
  facet_wrap( ~ season, ncol = 5, nrow = 3) +
  scale_color_viridis_c()+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme_bw()
ggsave(plot = plot.ppp.year, paste(path.output,"input_map/","ppp_data_",2017,".png", sep = ""), width = 16, height = 9 )

# plot of the bathy and distance from the coast ####


plot.bathy <- ggplot(study_domain_sf) + geom_sf() +
  geom_sf(data = grille_pred %>%
            mutate(bathy = ifelse(bathy > 250, 250, bathy)),
             aes(fill = bathy), color = NA) +
  labs(x = "", y = "", fill = "Bathymetry", 
       title = "") +
  scale_fill_gradient(low = "steelblue1", high = "midnightblue")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme_bw()
ggsave(plot = plot.bathy, paste(path.output,"input_map/","bathy_data.png", sep = ""), width = 9, height = 9 )

plot.distance <- ggplot(study_domain_sf) + geom_sf() +
  geom_sf(data = grille_pred ,
          aes(fill = dist2coast_km), color = NA) +
  labs(x = "", y = "", fill = "Distance coast", 
       title = "") +
  scale_fill_gradient(low = "steelblue1", high = "midnightblue")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme_bw()
ggsave(plot = plot.bathy, paste(path.output,"input_map/","dist_coast_data.png", sep = ""), width = 9, height = 9 )

# create recap table of data use ####

total <- c(
  dim(data_model %>% filter(survey == "PELGAS"))[1],
  dim(data_model %>% filter(survey == "JUVENA"))[1],
  dim(data_model %>% filter(survey == "PECHE"))[1]
)

annual_mean_spring <- c(
  dim(data_model %>% filter(survey == "PELGAS") %>% filter(season == "spring"))[1] / 14,
  dim(data_model %>% filter(survey == "JUVENA") %>% filter(season == "spring"))[1] / 14,
  dim(data_model %>% filter(survey == "PECHE") %>% filter(season == "spring"))[1] / 14
)

annual_mean_summer <- c(
  dim(data_model %>% filter(survey == "PELGAS") %>% filter(season == "summer"))[1] / 14,
  dim(data_model %>% filter(survey == "JUVENA") %>% filter(season == "summer"))[1] / 14,
  dim(data_model %>% filter(survey == "PECHE") %>% filter(season == "summer"))[1] / 14
)

annual_mean_fall <- c(
  dim(data_model %>% filter(survey == "PELGAS") %>% filter(season == "fall"))[1] / 14,
  dim(data_model %>% filter(survey == "JUVENA") %>% filter(season == "fall"))[1] / 14,
  dim(data_model %>% filter(survey == "PECHE") %>% filter(season == "fall"))[1] / 14
)

# figure anchovy and sardine ####

sard_tot <- readRDS(paste(local_path,"code/data_model/sard_tot.rds", sep = ""))
anch_tot <- readRDS(paste(local_path,"code/data_model/anch_tot.rds", sep = ""))
SA_tot <- rbind(sard_tot, anch_tot)
SA_tot$season <- as.factor(SA_tot$season)
SA_tot$season <- factor(SA_tot$season, levels = c("spring","summer","fall"))

plot.SA <- ggplot(study_domain_sf) + geom_sf() +
  geom_point(data = SA_tot %>% filter(survey != "PECHE") %>% filter(indice == 0),
             aes(x = Xgd, y = Ygd, color = indice), size = 0.3) +
  geom_point(data = SA_tot %>% filter(survey != "PECHE") %>%  filter(indice > 0),
             aes(x = Xgd, y = Ygd, color = indice), size = 0.5) +
  labs(x = "", y = "", fill = "Occurence", 
       title = "") +
  facet_wrap( sp ~ survey, ncol = 2, nrow = 2) +
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme_bw()


