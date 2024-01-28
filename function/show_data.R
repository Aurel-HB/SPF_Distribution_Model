show_data <- function(data){
  world <- ne_download(scale = 10, type = 'countries', returnclass = "sf")
  #windows()
  ggplot()+
    geom_point(data=data[data$indice==0,],aes(LONG,LAT),size = 0.5,shape = 1,colour = "black")+
    geom_point(data = data[data$indice > 0,], aes(x = LONG, y = LAT), size = 1.5, colour = "palegreen3") +
    geom_sf(data=world %>% filter(SOVEREIGNT=="France"))+
    theme(aspect.ratio = 1,
          legend.title = element_blank(),
          title = element_text(color = "darkgrey",face = "bold"),
          plot.title = element_text( size = 12, hjust = 0.5),
          plot.subtitle = element_text(size = 8,hjust = 0.5),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"))+
    coord_sf(xlim = c(-7,0), ylim = c(43,48.1), expand = FALSE)+
    scale_color_gradient(low = "white", high = "red")+
    xlab("")+ylab("")+
    labs(title = "Presence/absence (vert/noir) de l'espèce observée")+
    annotation_scale(location = "bl", line_width = .5) +
    annotation_north_arrow(location = "tr", height = unit(0.7, "cm"), width = unit(0.7, "cm")) + 
    theme_bw() 
}

#version plot
#windows()
#plot(data$LONG,data$LAT,cex=data$indice/100000,xlim = c(-7,0), ylim = c(43.1,48))
#coast()
