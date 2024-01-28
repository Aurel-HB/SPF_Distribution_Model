################ 2 season ##################

# Prepare the prediction for Gaussian field
grille_pred <- sf::st_read(paste(local_path,"code/environment/20230724_envdata_bob.gpkg", sep = ""), layer = "2021")

grille_pred <- grille_pred %>% st_transform(., 4326) %>% 
  filter(season == "spring") # the season doesn't matter because we don't have cov that change between season

ggplot(grille_pred)+geom_sf()

coord_pred <- grille_pred %>% st_centroid(.) %>% st_coordinates(.)

A_pred <- inla.spde.make.A(mesh = mesh_GdG, 
                           # must be a matrix
                           loc = coord_pred %>%
                             as.matrix(), 
                           group = 2,
)

common_latent.spring <- c()
common_latent.fall <- c()
associate_year <- c()

for (annee in as.numeric(periode)){
  # one year and 2 season
  result <- readRDS(paste(path.output,"/result/result_saison_dist_",annee,"_894",".rds", sep = ""))
  
  nodes <- result$summary.random$xc$mean
  
  common_latent <- grille_pred %>%
    mutate(spring = (as.matrix(A_pred)[, 894 + 1:894] %*% matrix(nodes[1:894], ncol = 1)),
           fall = (as.matrix(A_pred)[, 894 + 1:894] %*% matrix(nodes[894 + 1:894], ncol = 1))
           ) %>%
    st_as_sf() %>%
    st_cast()
  
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  common_latent.spring <- c(common_latent.spring, common_latent$spring)
  common_latent.fall <- c(common_latent.fall, common_latent$fall)
  
  
  plot.spring <- common_latent %>%
    ggplot() +
    geom_sf(aes(fill = spring), color = NA) +
    scale_fill_viridis_c() +
    labs(x = "", y = "", fill = "", title = paste("spring_",as.character(annee)))+
    theme_bw()
  #ggsave(plot = plot.spring, paste(path.output,"field/shared/","spring.shared_cov_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  plot.fall <- common_latent %>%
    ggplot() +
    geom_sf(aes(fill = fall), color = NA) +
    scale_fill_viridis_c() +
    labs(x = "", y = "", fill = "", title = paste("fall_",as.character(annee)))+
    theme_bw()
  #ggsave(plot = plot.fall, paste(path.output,"field/shared/","fall.shared_cov_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  
  
}

spring.shared <- data.frame(associate_year, common_latent.spring, rep(common_latent$geom,14))
names(spring.shared) <- c("year", "spring", "geometry")
spring.shared <- spring.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326)

plot.spring <- spring.shared %>% 
  ggplot() + 
  geom_sf(aes(fill = spring), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  theme_bw()
ggsave2(plot = plot.spring, paste(path.output,"field/shared/","2_spring.shared_cov.png", sep = ""), width = 16, height = 9)


fall.shared <- data.frame(associate_year, common_latent.fall, rep(common_latent$geom,14))
names(fall.shared) <- c("year", "fall", "geometry")
fall.shared <- fall.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326)

plot.fall <- fall.shared %>% 
  ggplot() + 
  geom_sf(aes(fill = fall), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  theme_bw()
ggsave2(plot = plot.fall, paste(path.output,"field/shared/","2_fall.shared_cov.png", sep = ""), width = 16, height = 9)

### model null ###
# Prepare the prediction for Gaussian field
grille_pred <- sf::st_read("C:/Users/ahebertb/Desktop/StageAurel/20230724_envdata_bob.gpkg", layer = "2021")

grille_pred <- grille_pred %>% st_transform(., 4326) %>% 
  filter(season == "spring") # the season doesn't matter because we don't have cov that change between season

ggplot(grille_pred)+geom_sf()

coord_pred <- grille_pred %>% st_centroid(.) %>% st_coordinates(.)

A_pred <- inla.spde.make.A(mesh = mesh_GdG, 
                           # must be a matrix
                           loc = coord_pred %>%
                             as.matrix(), 
                           group = 2,
)

common_latent.spring <- c()
common_latent.fall <- c()
associate_year <- c()

for (annee in as.numeric(periode)){
  # one year and 2 season
  result <- readRDS(paste(path.output,"/result/result_saison_null_",annee,"_894",".rds", sep = ""))
  
  nodes <- result$summary.random$xc$mean
  
  common_latent <- grille_pred %>%
    mutate(spring = (as.matrix(A_pred)[, 894 + 1:894] %*% matrix(nodes[1:894], ncol = 1)),
           fall = (as.matrix(A_pred)[, 894 + 1:894] %*% matrix(nodes[894 + 1:894], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  common_latent.spring <- c(common_latent.spring, common_latent$spring)
  common_latent.fall <- c(common_latent.fall, common_latent$fall)
  
  
  plot.spring <- common_latent %>%
    ggplot() +
    geom_sf(aes(fill = spring), color = NA) +
    scale_fill_viridis_c() +
    labs(x = "", y = "", fill = "", title = paste("spring_",as.character(annee)))+
    theme_bw()
  #ggsave(plot = plot.spring, paste(path.output,"field/shared/","spring.shared_null_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  plot.fall <- common_latent %>%
    ggplot() +
    geom_sf(aes(fill = fall), color = NA) +
    scale_fill_viridis_c() +
    labs(x = "", y = "", fill = "", title = paste("fall_",as.character(annee)))+
    theme_bw()
  #ggsave(plot = plot.fall, paste(path.output,"field/shared/","fall.shared_null_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  
  
}

spring.shared <- data.frame(associate_year, common_latent.spring, rep(common_latent$geom,14))
names(spring.shared) <- c("year", "spring", "geometry")
spring.shared <- spring.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326)

plot.spring <- spring.shared %>% 
  ggplot() + 
  geom_sf(aes(fill = spring), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  theme_bw()
ggsave2(plot = plot.spring, paste(path.output,"field/shared/","2_spring.shared_null.png", sep = ""), width = 16, height = 9)


fall.shared <- data.frame(associate_year, common_latent.fall, rep(common_latent$geom,14))
names(fall.shared) <- c("year", "fall", "geometry")
fall.shared <- fall.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326)

plot.fall <- fall.shared %>% 
  ggplot() + 
  geom_sf(aes(fill = fall), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( ~ year, ncol = 5, nrow = 3) +
  theme_bw()
ggsave2(plot = plot.fall, paste(path.output,"field/shared/","2_fall.shared_null.png", sep = ""), width = 16, height = 9)


######## 3 season ##################

# Prepare the prediction for Gaussian field
grille_pred <- sf::st_read("C:/Users/ahebertb/Desktop/StageAurel/20230724_envdata_bob.gpkg", layer = "2021")

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

# mean value of the shared fields ####
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

shared_mean <- shared

# sd value of the shared fields ####
common_latent.spring <- c()
common_latent.summer <- c()
common_latent.fall <- c()
associate_year <- c()

for (annee in as.numeric(periode)){
  # one year and 3 season
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  
  nodes <- result$summary.random$xc$sd
  
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
  
}

spring.shared <- data.frame(associate_year, common_latent.spring, rep(common_latent$geom,14), "spring")
names(spring.shared) <- c("year", "latent.field", "geometry", "season")
spring.shared <- spring.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326) %>%
  st_cast()

summer.shared <- data.frame(associate_year, common_latent.summer, rep(common_latent$geom,14), "summer")
names(summer.shared) <- c("year", "latent.field", "geometry", "season")
summer.shared <- summer.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326) %>%
  st_cast()

fall.shared <- data.frame(associate_year, common_latent.fall, rep(common_latent$geom,14), "fall")
names(fall.shared) <- c("year", "latent.field", "geometry", "season")
fall.shared <- fall.shared %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326) %>%
  st_cast()


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
ggsave(plot = plot.shared, paste(path.output,"field/shared/","3_shared_dist_sd_map.png", sep = ""), width = 12, height = 9)


shared_sd <- shared

# creation of a figure with mean and sd
shared_mean <- shared_mean %>% mutate(type = "mean") %>% st_cast()
shared_sd <- shared_sd %>% mutate(type = "sd") %>% st_cast()
shared <- rbind(shared_mean, shared_sd)

plot.shared.mean <- shared_mean %>% 
  filter(year %in% c(2015, 2017, 2022)) %>%
  ggplot() + 
  geom_sf(aes(fill = latent.field), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ year, ncol = 3, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

plot.shared.sd <- shared_sd %>% 
  filter(year %in% c(2015, 2017, 2022)) %>%
  ggplot() + 
  geom_sf(aes(fill = latent.field), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ year, ncol = 3, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

plot_grid(plot.shared.mean, plot.shared.sd)
ggsave2(paste(path.output,"field/shared/","3_shared_final.png", sep = ""), width = 12, height = 9)

### creation of the summaries map over the series

sum_shared_mean <- shared_mean %>% filter(year == 2009) #we prepare a year structure
sum_shared_mean <- sum_shared_mean[3:5]

sum_shared_sd <- shared_sd %>% filter(year == 2009) #we prepare a year structure
sum_shared_sd <- sum_shared_sd[3:5]


for (annee in as.numeric(periode)){
  temp_shared <- shared_mean %>% filter(year == annee)
  sum_shared_mean <- cbind(sum_shared_mean, temp_shared$latent.field)
  
  temp_shared <- shared_sd %>% filter(year == annee)
  sum_shared_sd <- cbind(sum_shared_sd, temp_shared$latent.field) 
  
}

mean_mean <- c()
mean_sd <- c()
sd_mean <- c()
sd_sd <- c()


for (line in 1:37140){
  mean_mean <- c(mean_mean, mean(c(as.numeric(sum_shared_mean[line,3:16][1:14]))[1:14]))
  mean_sd <- c(mean_sd, sd(c(as.numeric(sum_shared_mean[line,3:16][1:14]))[1:14]))
  sd_mean <- c(sd_mean, mean(c(as.numeric(sum_shared_sd[line,3:16][1:14]))[1:14]))
  sd_sd <- c(sd_sd, sd(as.numeric(c(sum_shared_sd[line,3:16][1:14]))[1:14]))
}

value <- c(mean_mean, mean_sd, sd_mean, sd_sd)
sum_shared_tot <- shared_sd %>% filter(year %in% c(2009:2012)) 
sum_shared_tot <- sum_shared_tot[5]

sum_shared_tot <- sum_shared_tot %>% mutate(value = value,
                                            type = c(rep("SGF_mean", 37140),
                                                     rep("SGF_sd_inter_annual", 37140),
                                                     rep("average_error", 37140),
                                                     rep("error_sd", 37140)),
                                            season = rep(c(rep("spring", 12380),
                                                         rep("summer", 12380),
                                                         rep("fall", 12380)),
                                                         4)
                                            ) %>%
  st_cast()

sum_shared_tot$season <- as.factor(sum_shared_tot$season)
sum_shared_tot$season <- factor(sum_shared_tot$season, levels = c("spring","summer","fall"))

sd_shared_plot <- sum_shared_tot %>% filter(type %in% c("average_error")) %>%
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 1, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

mean_shared_plot <- sum_shared_tot %>% filter(type %in% c("SGF_mean", "SGF_sd_inter_annual")) %>%
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 2, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

plot_grid(mean_shared_plot, sd_shared_plot, align = "h", axis = "tblr",
          rel_widths = c(1.8,1))
ggsave2(paste(path.output,"field/shared/","3_shared_summary_2.0.png", sep = ""),
        width = 9, height = 9)


# anomaly maps ####

anomaly_mean_tot <- data.frame()
anomaly_sd_tot <- data.frame()
anomaly_error_tot <- data.frame()

for (col in 3:16){
  anomaly_mean <- sum_shared_mean[,col] - mean_mean
  #anomaly_sd <- sum_shared_mean[,col] - mean_sd
  #anomaly_error <- sum_shared_mean[,col] - sd_mean
  
  names(anomaly_mean) <- c("mean", "geom")
  #names(anomaly_sd) <- c("sd", "geom")
  #names(anomaly_error) <- c("error", "geom")
  #
  sum_shared_mean <- cbind(sum_shared_mean, c(anomaly_mean)[1], c(anomaly_sd)[1], c(anomaly_error)[1])
  
  anomaly_mean_tot <- rbind(anomaly_mean_tot, c(anomaly_mean)[1])
  #anomaly_sd_tot <- rbind(anomaly_sd_tot, c(anomaly_sd)[1])
  #anomaly_error_tot <- rbind(anomaly_error_tot, c(anomaly_error)[1])
}

# approach 2 ####
anomaly_tot <- shared_sd[5]

anomaly_tot <- data.frame(mean = anomaly_mean_tot,
                          season = rep(c(rep("spring", 12380),
                                         rep("summer", 12380),
                                         rep("fall", 12380)),
                                       14),
                          year = shared_mean$year,
                          geometry = shared_sd[5]
                          ) %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326)

anomaly_tot$season <- as.factor(anomaly_tot$season)
anomaly_tot$season <- factor(anomaly_tot$season, levels = c("spring","summer","fall"))

anomaly_mean_plot <- anomaly_tot %>%
  ggplot() + 
  geom_sf(aes(fill = mean), color = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  facet_wrap( season ~ year, ncol = 14, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()
ggsave(anomaly_mean_plot, paste(path.output,"field/shared/","3_shared_mean_anomaly.png", sep = ""),
       width = 16, height = 9)


anomaly_mean_plot <- anomaly_tot %>% filter(season == "spring") %>%
  ggplot() + 
  geom_sf(aes(fill = as.numeric(mean)), color = NA) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
  facet_wrap( season ~ year, ncol = 14, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

ggsave(anomaly_mean_plot, paste(path.output,"field/shared/","3_shared_mean_anomaly.png", sep = ""),
        width = 16, height = 9)

anomaly_sd_plot <- anomaly_tot %>% 
  ggplot() + 
  geom_sf(aes(fill = sd), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ year, ncol = 14, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

ggsave2(anomaly_sd_plot, paste(path.output,"field/shared/","3_shared_sd_anomaly.png", sep = ""),
        width = 16, height = 9)

anomaly_error_plot <- anomaly_tot %>% 
  ggplot() + 
  geom_sf(aes(fill = error), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ year, ncol = 14, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

ggsave2(anomaly_error_plot, paste(path.output,"field/shared/","3_shared_error_anomaly.png", sep = ""),
        width = 16, height = 9)

#####
### specific fields ####

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


