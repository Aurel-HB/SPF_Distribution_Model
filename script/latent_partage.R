
######## 3 season ##################

# Prepare the prediction for Gaussian field
grille_pred <- sf::st_read(paste(local_path,"code/environment/20230724_envdata_bob.gpkg", sep = ""), layer = "2021")

grille_pred <- grille_pred %>% st_transform(., 4326) %>% 
  filter(season == "summer")# the season doesn't matter because we don't have cov that change between season

ggplot(grille_pred)+geom_sf()

coord_pred <- grille_pred %>% st_centroid(.) %>% st_coordinates(.)

A_pred <- inla.spde.make.A(mesh = mesh_GdG, 
                           # must be a matrix
                           loc = coord_pred %>%
                             as.matrix(), 
                           group = 3,
)

common_latent.spring <- c()
common_latent.summer <- c()
common_latent.fall <- c()
associate_year <- c()

for (annee in as.numeric(periode)){
  # one year and 3 season
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  
  nodes <- result$summary.random$xc$mean
  
  common_latent <- grille_pred %>%
    mutate(spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[1:894], ncol = 1)),
           summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894 + 1:894], ncol = 1)),
           fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894*2 + 1:894], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  common_latent.spring <- c(common_latent.spring, common_latent$spring)
  common_latent.summer <- c(common_latent.summer, common_latent$summer)
  common_latent.fall <- c(common_latent.fall, common_latent$fall)
  
  #plot.spring <- common_latent %>%
  #  ggplot() +
  #  geom_sf(aes(fill = spring), color = NA) +
  #  scale_fill_viridis_c() +
  #  labs(x = "", y = "", fill = "", title = paste("spring_",as.character(annee)))+
  #  theme_bw()
  #ggsave(plot = plot.spring, paste(path.output,"field/shared/","3_spring.shared_cov_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  #plot.summer <- common_latent %>%
  #  ggplot() +
  #  geom_sf(aes(fill = summer), color = NA) +
  #  scale_fill_viridis_c() +
  #  labs(x = "", y = "", fill = "", title = paste("summer_",as.character(annee)))+
  #  theme_bw()
  #ggsave(plot = plot.summer, paste(path.output,"field/shared/","3_summer.shared_cov_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
#  plot.fall <- common_latent %>%
#    ggplot() +
#    geom_sf(aes(fill = fall), color = NA) +
#    scale_fill_viridis_c() +
#    labs(x = "", y = "", fill = "", title = paste("fall_",as.character(annee)))+
#    theme_bw()
  #ggsave(plot = plot.fall, paste(path.output,"field/shared/","3_fall.shared_cov_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  shared <-  c(common_latent.spring[1:12380 + 12380*(annee - 2009)],
               common_latent.summer[1:12380 + 12380*(annee - 2009)],
               common_latent.fall[1:12380 + 12380*(annee - 2009)])
  shared <- data.frame(shared, rep(common_latent$geom,3))
  shared <- data.frame(shared, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(shared) <- c("latent", "geom", "season")
  shared$season <- factor(shared$season, levels = c("spring","summer","fall"))
  
  shared <- shared %>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  plot.shared <- shared %>% 
    ggplot() + 
    geom_sf(aes(fill = latent), color = NA) +
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    facet_wrap( ~ season, ncol = 3, nrow = 1) +
    theme_bw()
  ggsave(plot = plot.shared, paste(path.output,"field/shared/","3_shared_dist_",annee,".png", sep = ""), width = 16, height = 9)
}

spring.shared <- data.frame(associate_year, common_latent.spring, rep(common_latent$geom,14), "spring")
names(spring.shared) <- c("year", "latent.field", "geometry", "season")
spring.shared <- spring.shared %>% 
     st_as_sf() %>% 
     st_set_crs(., 4326) %>%
  st_cast()

plot.spring <- spring.shared %>% 
  ggplot() + 
  geom_sf(aes(fill = latent.field), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  theme_bw()
ggsave2(plot = plot.spring, paste(path.output,"field/shared/","3_spring.shared_dist.png", sep = ""), width = 16, height = 9)


summer.shared <- data.frame(associate_year, common_latent.summer, rep(common_latent$geom,14), "summer")
names(summer.shared) <- c("year", "latent.field", "geometry", "season")
summer.shared <- summer.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326) %>%
  st_cast()

plot.summer <- summer.shared %>% 
  ggplot() + 
  geom_sf(aes(fill = latent.field), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  theme_bw()
ggsave2(plot = plot.summer, paste(path.output,"field/shared/","3_summer.shared_dist.png", sep = ""), width = 16, height = 9)


fall.shared <- data.frame(associate_year, common_latent.fall, rep(common_latent$geom,14), "fall")
names(fall.shared) <- c("year", "latent.field", "geometry", "season")
fall.shared <- fall.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326) %>%
  st_cast()

plot.fall <- fall.shared %>% 
  ggplot() + 
  geom_sf(aes(fill = latent.field), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  theme_bw()
ggsave2(plot = plot.fall, paste(path.output,"field/shared/","3_fall.shared_dist.png", sep = ""), width = 16, height = 9)

 
shared <- rbind(spring.shared, summer.shared, fall.shared)
shared$season <- factor(shared$season, levels = c("spring","summer","fall"))

plot.shared <- shared %>% 
  filter(year %in% c(2011, 2013, 2015, 2017, 2022)) %>%
  ggplot() + 
  geom_sf(aes(fill = latent.field), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ year, ncol = 5, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()
ggsave(plot = plot.shared, paste(path.output,"field/shared/","3_shared_dist.png", sep = ""), width = 12, height = 9)












common_binom.spring <- c()
common_binom.summer <- c()
common_binom.fall <- c()
common_logn.spring <- c()
common_logn.summer <- c()
common_logn.fall <- c()
common_ppp.spring <- c()
common_ppp.summer <- c()
common_ppp.fall <- c()

associate_year <- c()


for (annee in as.numeric(periode)){
  
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  #binom
  nodes <- result$summary.random$x$mean
  
  
  common_latent <- grille_pred %>%
    mutate(spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[1:894], ncol = 1)),
           summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894 + 1:894], ncol = 1)),
           fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894*2 + 1:894], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  common_binom.spring <- c(common_binom.spring, common_latent$spring)
  common_binom.summer <- c(common_binom.summer, common_latent$summer)
  common_binom.fall <- c(common_binom.fall, common_latent$fall)

  #LogN
  nodes <- result$summary.random$u$mean
  
  common_latent <- grille_pred %>%
    mutate(spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[1:894], ncol = 1)),
           summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894 + 1:894], ncol = 1)),
           fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894*2 + 1:894], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  common_logn.spring <- c(common_logn.spring, common_latent$spring)
  common_logn.summer <- c(common_logn.summer, common_latent$summer)
  common_logn.fall <- c(common_logn.fall, common_latent$fall)
  
  #PPP
  nodes <- result$summary.random$p$mean
  
  common_latent <- grille_pred %>%
    mutate(spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[1:894], ncol = 1)),
           summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894 + 1:894], ncol = 1)),
           fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(nodes[894*2 + 1:894], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()

  common_ppp.spring <- c(common_ppp.spring, common_latent$spring)
  common_ppp.summer <- c(common_ppp.summer, common_latent$summer)
  common_ppp.fall <- c(common_ppp.fall, common_latent$fall)
  


  binom <-  c(common_binom.spring[1:12380 + 12380*(annee - 2009)],
              common_binom.summer[1:12380 + 12380*(annee - 2009)],
              common_binom.fall[1:12380 + 12380*(annee - 2009)])
  binom <- data.frame(binom, rep(common_latent$geom,3))
  binom <- data.frame(binom, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(binom) <- c("latent", "geom", "season")
  binom$season <- factor(binom$season, levels = c("spring","summer","fall"))
  
  binom <- binom %>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  plot.binom <- binom %>% 
    ggplot() + 
    geom_sf(aes(fill = latent), color = NA) +
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    facet_wrap( ~ season, ncol = 3, nrow = 1) +
    theme_bw()
  ggsave(plot = plot.binom, paste(path.output,"field/binom/","3_binom_dist_",annee,".png", sep = ""), width = 16, height = 9)
  
  logn <-  c(common_logn.spring[1:12380 + 12380*(annee - 2009)],
             common_logn.summer[1:12380 + 12380*(annee - 2009)],
             common_logn.fall[1:12380 + 12380*(annee - 2009)])
  logn <- data.frame(logn, rep(common_latent$geom,3))
  logn <- data.frame(logn, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(logn) <- c("latent", "geom", "season")
  logn$season <- factor(logn$season, levels = c("spring","summer","fall"))
  
  logn <- logn %>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  plot.logn <- logn %>% 
    ggplot() + 
    geom_sf(aes(fill = latent), color = NA) +
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    facet_wrap( ~ season, ncol = 3, nrow = 1) +
    theme_bw()
  ggsave(plot = plot.logn, paste(path.output,"field/logn/","3_logn_dist_",annee,".png", sep = ""), width = 16, height = 9)
  
  ppp <-  c(common_ppp.spring[1:12380 + 12380*(annee - 2009)],
            common_ppp.summer[1:12380 + 12380*(annee - 2009)],
            common_ppp.fall[1:12380 + 12380*(annee - 2009)])
  ppp <- data.frame(ppp, rep(common_latent$geom,3))
  ppp <- data.frame(ppp, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(ppp) <- c("latent", "geom", "season")
  ppp$season <- factor(ppp$season, levels = c("spring","summer","fall"))
  
  ppp <- ppp %>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  plot.ppp <- ppp %>% 
    ggplot() + 
    geom_sf(aes(fill = latent), color = NA) +
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    facet_wrap( ~ season, ncol = 3, nrow = 1) +
    theme_bw()
  ggsave(plot = plot.ppp, paste(path.output,"field/ppp/","3_ppp_dist_",annee,".png", sep = ""), width = 16, height = 9)
}


# one season and 14 years ####
result <- result_spring_cov_894

nodes <- result$summary.random$xc$mean


common_latent <- grille_pred %>%
  mutate(xc_2009 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[1:894], ncol = 1)),
         xc_2010 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[1*894 + 1:894], ncol = 1)),
         xc_2011 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[2*894 + 1:894], ncol = 1)),
         xc_2012 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[3*894 + 1:894], ncol = 1)),
         xc_2013 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[4*894 + 1:894], ncol = 1)),
         xc_2014 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[5*894 + 1:894], ncol = 1)),
         xc_2015 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[6*894 + 1:894], ncol = 1)),
         xc_2016 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[7*894 + 1:894], ncol = 1)),
         xc_2017 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[8*894 + 1:894], ncol = 1)),
         xc_2018 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[9*894 + 1:894], ncol = 1)),
         xc_2019 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[10*894 + 1:894], ncol = 1)),
         xc_2020 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[11*894 + 1:894], ncol = 1)),
         xc_2021 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[12*894 + 1:894], ncol = 1)),
         xc_2022 = (as.matrix(A_pred)[, 13*894 + 1:894] %*% matrix(nodes[13*894 + 1:894], ncol = 1))
  ) %>%
  st_as_sf() %>%
  st_cast()


plot.2009 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2009), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2010 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2010), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2011 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2011), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2012 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2012), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2013 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2013), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2014 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2014), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2015 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2015), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2016 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2016), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2017 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2017), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2018 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2018), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2019 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2019), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2020 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2020), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2021 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2021), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()

plot.2022 <- common_latent %>%
  ggplot() +
  geom_sf(aes(fill = xc_2022), color = NA) +
  scale_fill_viridis_c() +
  theme_bw()


plot_grid(plot.2009, plot.2010, plot.2011, plot.2012, plot.2013, plot.2014, plot.2015, 
          plot.2016, plot.2017, plot.2018, plot.2019, plot.2020, plot.2021,plot.2022,
          nrow = 3 , ncol = 5)


