data_model_sard <- readRDS(paste(local_path,"code/data_model/sard_tot.rds", sep = ""))

data_model_anch <- readRDS(paste(local_path,"code/data_model/anch_tot.rds", sep = ""))

data_model_sard$season <- factor(data_model_sard$season, levels = c("spring","summer","fall"))
data_model_anch$season <- factor(data_model_anch$season, levels = c("spring","summer","fall"))

data_model_sard$survey <- factor(data_model_sard$survey, levels = c("PELGAS","JUVENA","PECHE"))
data_model_anch$survey <- factor(data_model_anch$survey, levels = c("PELGAS","JUVENA","PECHE"))


ggplot(study_domain_sf) + geom_sf() +
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  geom_point(data = data_model_sard %>% filter(survey %in% c("PELGAS","JUVENA")),
            aes(x = Xgd, y = Ygd, color = indice), size = 1) +
  geom_point(data = data_model_anch %>% filter(survey %in% c("PELGAS","JUVENA")),
             aes(x = Xgd, y = Ygd, color = indice), size = 1) +
  labs(x = "", y = "", 
       title = "Carte de présence de la sardine et de l'anchois dans le Golfe de Gascogne par les campagnes sur la période 2009-2022"
       ) +
  theme(aspect.ratio = 1,
        #legend.title = element_blank(),
        title = element_text(color = "black",face = "bold"),
        plot.title = element_text( size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 8,hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"),
        panel.background = element_rect(fill = "gray90"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))+
  coord_sf(xlim = c(-7,0), ylim = c(43,48.1), expand = FALSE)+
  #scale_color_gradient(low = "cadetblue3", high = "royalblue")+
  facet_wrap(sp ~ survey, ncol = 2, nrow = 2) +
  annotation_scale(location = "bl", line_width = .5) +
  annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm")) + 
  theme() 



ggplot(study_domain_sf) + geom_sf() +
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  geom_point(data = data_model %>% filter(is.na(indice)),
             aes(x = Xgd, y = Ygd, color = presence), size = 1) +
  labs(x = "", y = "", fill = "Occurence", 
       title = "Carte de présence de l'anchois dans le Golfe de Gascogne par la Pêche") +
  theme(aspect.ratio = 1,
        legend.title = element_blank(),
        title = element_text(color = "black",face = "bold"),
        plot.title = element_text( size = 12, hjust = 0.5),
        plot.subtitle = element_text(size = 8,hjust = 0.5),
        panel.border = element_blank(),
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"),
        panel.background = element_rect(fill = "gray90"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "white"))+
  coord_sf(xlim = c(-7,0), ylim = c(43,48.1), expand = FALSE)+
  #scale_color_gradient(low = "royalblue", high = "blue")+
  facet_wrap(type ~ season, ncol = 3, nrow = 2) +
  annotation_scale(location = "bl", line_width = .5) +
  annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm")) + 
  theme() 
