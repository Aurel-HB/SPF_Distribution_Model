ggplot(study_domain_sf)+
  geom_sf(color = "royalblue", fill = NA)+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
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
  scale_color_gradient(low = "white", high = "red")+
  xlab("")+ylab("")+
  labs(title = "Golfe de Gascogne et Zone d'étude du projet")+
  annotation_scale(location = "bl", line_width = .5) +
  annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm")) + 
  theme() 

