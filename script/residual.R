
##### 3 season #####
data_model_fin <- sf::st_read(paste(local_path,"code/environment/20230725_surveys_env_data_894.gpkg", sep =""))
for (annee in as.numeric(periode)){
  
  res.jo <- readRDS(paste(local_path,"code/output/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  coo <- data_model_fin %>% filter( year == annee) %>%
    st_drop_geometry() %>%
    dplyr::select(Xgd, Ygd)
  
  # the first lines have to be adapted to the type of mesh and number of season
  val_obs <- 3 * 3 * length(coo[,1])
  test <- res.jo$residuals$deviance.residuals[1:val_obs]
  loc <- rbind(coo,coo,coo,coo,coo,coo,coo,coo,coo)
  
  indic <- data.frame(c(rep(1,length(coo[,1])),rep(2,length(coo[,1])),
                        rep(3,length(coo[,1])),
                        rep(4,length(coo[,1])),rep(5,length(coo[,1])),
                        rep(6,length(coo[,1])),
                        rep(7,length(coo[,1])),rep(8,length(coo[,1])),
                        rep(9,length(coo[,1]))))
  
  test <- cbind(test,loc, rep("residual",length(test)),indic)
  
  names(test) <- c("Value","Long","Lat","class","indic")
  
  residual_spring_bin <- test %>% filter(indic == 1) %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  residual_spring_LogN <- test%>% filter(indic == 2)%>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  residual_spring_poi <- test %>% filter(indic == 3)  %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  
  residual_summer_bin <- test %>% filter(indic == 4)   %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  residual_summer_LogN <- test %>% filter(indic == 5 ) %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  residual_summer_poi <- test %>% filter(indic == 6)  %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  
  residual_fall_bin <- test %>% filter(indic == 7)   %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  residual_fall_LogN <- test %>% filter(indic == 8) %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  residual_fall_poi <- test %>% filter(indic == 9)  %>% 
    mutate(x = Long, y= Lat) %>%
    st_as_sf(., coords = c("x","y")) %>%
    st_set_crs(., 4326)
  
  #[inla.stack.index(stack = stk.all, tag = "dens")$data[1:length(coo[,1]) + 2*length(coo[,1])],] <- can replace the indic
  # ggplot(study_domain_sf) + geom_sf() +
  #   geom_point(data = residual_spring_bin %>% filter(!is.na(Value)),
  #              aes(x = Long, y = Lat, color = Value), size = 1) +
  #   labs(x = "", y = "", fill = "Occurence", 
  #        title = "Carte des résidus Binomiaux") +
  #   theme_bw()
  # 
  # ggplot(study_domain_sf) + geom_sf() +
  #   geom_point(data = residual_spring_LogN %>% filter(!is.na(Value)),
  #              aes(x = Long, y = Lat, color = Value), size = 1) +
  #   labs(x = "", y = "", fill = "Occurence", 
  #        title = "Carte des résidus LogNormal ") +
  #   theme_bw()
  # 
  # ggplot(study_domain_sf) + geom_sf() +
  #   geom_point(data = residual_spring_poi %>% filter(!is.na(Value)),
  #              aes(x = Long, y = Lat, color = Value), size = 1) +
  #   labs(x = "", y = "", fill = "Occurence", 
  #        title = "Carte des résidus du processus de Poisson") +
  #   theme_bw()
  # 
  # ggplot(study_domain_sf) + geom_sf() +
  #   geom_point(data = residual_fall_bin %>% filter(!is.na(Value)),
  #              aes(x = Long, y = Lat, color = Value), size = 1) +
  #   labs(x = "", y = "", fill = "Occurence", 
  #        title = "Carte des résidus Binomiaux") +
  #   theme_bw()
  # 
  # ggplot(study_domain_sf) + geom_sf() +
  #   geom_point(data = residual_fall_LogN %>% filter(!is.na(Value)),
  #              aes(x = Long, y = Lat, color = Value), size = 1) +
  #   labs(x = "", y = "", fill = "Occurence", 
  #        title = "Carte des résidus LogNormal ") +
  #   theme_bw()
  # 
  # ggplot(study_domain_sf) + geom_sf() +
  #   geom_point(data = residual_fall_poi %>% filter(!is.na(Value)),
  #              aes(x = Long, y = Lat, color = Value), size = 1) +
  #   labs(x = "", y = "", fill = "Occurence", 
  #        title = "Carte des résidus du processus de Poisson") +
  #   theme_bw()
  
  residual_spring_Bin <- grille_residual(study_domain_sf,residual_spring_bin)
  residual_spring_LogN <- grille_residual(study_domain_sf,residual_spring_LogN)
  residual_spring_Poi <- grille_residual(study_domain_sf,residual_spring_poi)
  
  residual_summer_Bin <- grille_residual(study_domain_sf,residual_summer_bin)
  residual_summer_LogN <- grille_residual(study_domain_sf,residual_summer_LogN)
  residual_summer_Poi <- grille_residual(study_domain_sf,residual_summer_poi)
  
  residual_fall_Bin <- grille_residual(study_domain_sf,residual_fall_bin)
  residual_fall_LogN <- grille_residual(study_domain_sf,residual_fall_LogN)
  residual_fall_Poi <- grille_residual(study_domain_sf,residual_fall_poi)
  
  #Créer un gradient de couleurs triées en fonction du vecteur z
  
  #colfunc<-colorRampPalette(c("cadetblue1","blue3"))
  #colors <- (colfunc(500))
  
  #binomial###################################################################
  #colour <- colors[rank(as.numeric(residual_Bin$Sum))]
  #
  #ggplot(residual_Bin) + geom_sf(color = NA, fill = colour)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "Carte des résidus en somme par rectangle") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  #
  #ggsave(paste(path.output,"residu/","Grille_Sum_Binom.png", sep = ""), width = 6.50, height = 8.42)
  
  #residual_spring_Bin <- residual_spring_Bin %>% filter(!is.na(Mean))
  
  #plot1 <- ggplot(residual_spring_Bin) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "Binom_spring") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  #ggsave(plot = plot1, paste(path.output,"residu/","binom_spring_",annee,".png", sep = ""), width = 6.50, height = 8.42 )
  
  #residual_fall_Bin <- residual_fall_Bin %>% filter(!is.na(Mean))
  
  #plot2 <- ggplot(residual_fall_Bin) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "Binom_fall") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  #ggsave(plot = plot2, paste(path.output,"residu/","binom_fall_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  #residual_summer_Bin <- residual_summer_Bin %>% filter(!is.na(Mean))
  
  #plot7 <- ggplot(residual_summer_Bin) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "Binom_summer") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  #LogN###################################################################
  
  #residual_spring_LogN <- residual_spring_LogN %>% filter(!is.na(Mean))
  
  #plot3 <- ggplot(residual_spring_LogN) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "LogN_spring") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  #ggsave(plot = plot3, paste(path.output,"residu/","LogN_spring_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  
  
  #residual_fall_LogN <- residual_fall_LogN %>% filter(!is.na(Mean))
  
  #plot4 <- ggplot(residual_fall_LogN) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "LogN_fall") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  #ggsave(plot = plot4, paste(path.output,"residu/","LogN_fall_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  #residual_summer_LogN <- residual_summer_LogN %>% filter(!is.na(Mean))
  
  #plot8 <- ggplot(residual_summer_LogN) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "LogN_summer") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  
  #poisson###################################################################
  
  #residual_spring_Poi <- residual_spring_Poi %>% filter(!is.na(Mean))
  
  #plot5 <- ggplot(residual_spring_Poi) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "PPP_spring") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  #ggsave(plot = plot5, paste(path.output,"residu/","poi_spring_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  
  #residual_fall_Poi <- residual_fall_Poi %>% filter(!is.na(Mean))
  
  #plot6 <- ggplot(residual_fall_Poi) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "PPP_fall") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  #ggsave(plot = plot6, paste(path.output,"residu/","poi_fall_",annee,".png", sep = ""), width = 6.50, height = 8.42)
  
  #residual_summer_Poi <- residual_summer_Poi %>% filter(!is.na(Mean))
  
  #plot9 <- ggplot(residual_summer_Poi) + geom_sf(aes(fill = Mean), color = NA)+
  #  labs(x = "", y = "", fill = "Residu", 
  #       title = "PPP_summer") +
  #  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  #  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  #  theme()
  
  # creation of a sf object with all the data to use the facet wrap function of ggplot
  residual_tot <-  rbind(residual_spring_Bin ,
                     residual_spring_LogN ,
                     residual_spring_Poi ,
                     residual_summer_Bin ,
                     residual_summer_LogN ,
                     residual_summer_Poi ,
                     residual_fall_Bin  ,
                     residual_fall_LogN ,
                     residual_fall_Poi  
                     )
  
  denomination <- c(
    rep("spring_Bin", length(residual_spring_Bin$Mean)),
    rep("spring_LogN", length(residual_spring_LogN$Mean)),
    rep("spring_Poi", length(residual_spring_Poi$Mean)),
    rep("summer_Bin", length(residual_summer_Bin$Mean)),
    rep("summer_LogN", length(residual_summer_LogN$Mean)),
    rep("summer_Poi", length(residual_summer_Poi$Mean)),
    rep("fall_Bin", length(residual_fall_Bin$Mean)),
    rep("fall_LogN", length(residual_fall_LogN$Mean)),
    rep("fall_Poi", length(residual_fall_Poi$Mean))
  )
  
  residual_tot <- residual_tot %>%
    mutate(name = denomination) %>%
    st_cast()
  
  residual_tot$name <- as.factor(residual_tot$name)
  residual_tot$name <- factor(residual_tot$name, levels = c("spring_Bin",
                                                            "spring_LogN",
                                                            "spring_Poi",
                                                            "summer_Bin",
                                                            "summer_LogN",
                                                            "summer_Poi",
                                                            "fall_Bin",
                                                            "fall_LogN",
                                                            "fall_Poi"))
  
  
  plot_tot <- ggplot(residual_tot) + geom_sf(aes(fill = Mean), color = NA)+
    labs(x = "", y = "", fill = "Residu") +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    scale_fill_viridis_c()+
    facet_wrap( ~ name, ncol = 3, nrow = 3)+
    theme()
  
  
  
  #plot_tot <- plot_grid(plot1, plot3, plot5, plot7, plot8, plot9, plot2, plot4, plot6, rows = 3, cols = 3)
  
  ggsave2(plot = plot_tot, paste(path.output,"residu/","3_residual_dist_",annee,".png", sep = ""), width = 9, height = 9 )
  
  
}

#comparison on one year####
data_model_fin <- sf::st_read("C:/Users/ahebertb/Desktop/StageAurel/20230725_surveys_env_data_894.gpkg")

annee <- 2017

res.jo.null <- readRDS(paste(local_path,"code/output/result/result_summer_null_",annee,"_894",".rds", sep = ""))
res.jo.dist <- readRDS(paste(local_path,"code/output/result/result_summer_dist_",annee,"_894",".rds", sep = ""))

## model null ####
coo <- data_model_fin %>% filter( year == annee) %>%
  st_drop_geometry() %>%
  dplyr::select(Xgd, Ygd)

# the first lines have to be adapted to the type of mesh and number of season
val_obs <- 3 * 3 * length(coo[,1])
test <- res.jo.null$residuals$deviance.residuals[1:val_obs]
loc <- rbind(coo,coo,coo,coo,coo,coo,coo,coo,coo)

indic <- data.frame(c(rep(1,length(coo[,1])),rep(2,length(coo[,1])),
                      rep(3,length(coo[,1])),
                      rep(4,length(coo[,1])),rep(5,length(coo[,1])),
                      rep(6,length(coo[,1])),
                      rep(7,length(coo[,1])),rep(8,length(coo[,1])),
                      rep(9,length(coo[,1]))))

test <- cbind(test,loc, rep("residual",length(test)),indic)

names(test) <- c("Value","Long","Lat","class","indic")

residual_spring_bin <- test %>% filter(indic == 1) %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_spring_LogN <- test%>% filter(indic == 2)%>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_spring_poi <- test %>% filter(indic == 3)  %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)

residual_summer_bin <- test %>% filter(indic == 4)   %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_summer_LogN <- test %>% filter(indic == 5 ) %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_summer_poi <- test %>% filter(indic == 6)  %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)

residual_fall_bin <- test %>% filter(indic == 7)   %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_fall_LogN <- test %>% filter(indic == 8) %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_fall_poi <- test %>% filter(indic == 9)  %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)


residual_spring_Bin <- grille_residual(study_domain_sf,residual_spring_bin)
residual_spring_LogN <- grille_residual(study_domain_sf,residual_spring_LogN)
residual_spring_Poi <- grille_residual(study_domain_sf,residual_spring_poi)

residual_summer_Bin <- grille_residual(study_domain_sf,residual_summer_bin)
residual_summer_LogN <- grille_residual(study_domain_sf,residual_summer_LogN)
residual_summer_Poi <- grille_residual(study_domain_sf,residual_summer_poi)

residual_fall_Bin <- grille_residual(study_domain_sf,residual_fall_bin)
residual_fall_LogN <- grille_residual(study_domain_sf,residual_fall_LogN)
residual_fall_Poi <- grille_residual(study_domain_sf,residual_fall_poi)

residual_tot <-  rbind(residual_spring_Bin ,
                       residual_spring_LogN ,
                       residual_spring_Poi ,
                       residual_summer_Bin ,
                       residual_summer_LogN ,
                       residual_summer_Poi ,
                       residual_fall_Bin  ,
                       residual_fall_LogN ,
                       residual_fall_Poi  
)

denomination <- c(
  rep("spring_Bin.null", length(residual_spring_Bin$Mean)),
  rep("spring_LogN.null", length(residual_spring_LogN$Mean)),
  rep("spring_Poi.null", length(residual_spring_Poi$Mean)),
  rep("summer_Bin.null", length(residual_summer_Bin$Mean)),
  rep("summer_LogN.null", length(residual_summer_LogN$Mean)),
  rep("summer_Poi.null", length(residual_summer_Poi$Mean)),
  rep("fall_Bin.null", length(residual_fall_Bin$Mean)),
  rep("fall_LogN.null", length(residual_fall_LogN$Mean)),
  rep("fall_Poi.null", length(residual_fall_Poi$Mean))
)

residual_tot <- residual_tot %>%
  mutate(name = denomination) %>%
  st_cast()

residual_tot$name <- as.factor(residual_tot$name)
residual_tot$name <- factor(residual_tot$name, levels = c("spring_Bin.null",
                                                          "spring_LogN.null",
                                                          "spring_Poi.null",
                                                          "summer_Bin.null",
                                                          "summer_LogN.null",
                                                          "summer_Poi.null",
                                                          "fall_Bin.null",
                                                          "fall_LogN.null",
                                                          "fall_Poi.null"))
residual_tot_null <- residual_tot

## model dist ####
coo <- data_model_fin %>% filter( year == annee) %>%
  st_drop_geometry() %>%
  dplyr::select(Xgd, Ygd)

# the first lines have to be adapted to the type of mesh and number of season
val_obs <- 3 * 3 * length(coo[,1])
test <- res.jo.dist$residuals$deviance.residuals[1:val_obs]
loc <- rbind(coo,coo,coo,coo,coo,coo,coo,coo,coo)

indic <- data.frame(c(rep(1,length(coo[,1])),rep(2,length(coo[,1])),
                      rep(3,length(coo[,1])),
                      rep(4,length(coo[,1])),rep(5,length(coo[,1])),
                      rep(6,length(coo[,1])),
                      rep(7,length(coo[,1])),rep(8,length(coo[,1])),
                      rep(9,length(coo[,1]))))

test <- cbind(test,loc, rep("residual",length(test)),indic)

names(test) <- c("Value","Long","Lat","class","indic")

residual_spring_bin <- test %>% filter(indic == 1) %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_spring_LogN <- test%>% filter(indic == 2)%>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_spring_poi <- test %>% filter(indic == 3)  %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)

residual_summer_bin <- test %>% filter(indic == 4)   %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_summer_LogN <- test %>% filter(indic == 5 ) %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_summer_poi <- test %>% filter(indic == 6)  %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)

residual_fall_bin <- test %>% filter(indic == 7)   %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_fall_LogN <- test %>% filter(indic == 8) %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)
residual_fall_poi <- test %>% filter(indic == 9)  %>% 
  mutate(x = Long, y= Lat) %>%
  st_as_sf(., coords = c("x","y")) %>%
  st_set_crs(., 4326)


residual_spring_Bin <- grille_residual(study_domain_sf,residual_spring_bin)
residual_spring_LogN <- grille_residual(study_domain_sf,residual_spring_LogN)
residual_spring_Poi <- grille_residual(study_domain_sf,residual_spring_poi)

residual_summer_Bin <- grille_residual(study_domain_sf,residual_summer_bin)
residual_summer_LogN <- grille_residual(study_domain_sf,residual_summer_LogN)
residual_summer_Poi <- grille_residual(study_domain_sf,residual_summer_poi)

residual_fall_Bin <- grille_residual(study_domain_sf,residual_fall_bin)
residual_fall_LogN <- grille_residual(study_domain_sf,residual_fall_LogN)
residual_fall_Poi <- grille_residual(study_domain_sf,residual_fall_poi)

residual_tot <-  rbind(residual_spring_Bin ,
                       residual_spring_LogN ,
                       residual_spring_Poi ,
                       residual_summer_Bin ,
                       residual_summer_LogN ,
                       residual_summer_Poi ,
                       residual_fall_Bin  ,
                       residual_fall_LogN ,
                       residual_fall_Poi  
)

denomination <- c(
  rep("spring_Bin.dist", length(residual_spring_Bin$Mean)),
  rep("spring_LogN.dist", length(residual_spring_LogN$Mean)),
  rep("spring_Poi.dist", length(residual_spring_Poi$Mean)),
  rep("summer_Bin.dist", length(residual_summer_Bin$Mean)),
  rep("summer_LogN.dist", length(residual_summer_LogN$Mean)),
  rep("summer_Poi.dist", length(residual_summer_Poi$Mean)),
  rep("fall_Bin.dist", length(residual_fall_Bin$Mean)),
  rep("fall_LogN.dist", length(residual_fall_LogN$Mean)),
  rep("fall_Poi.dist", length(residual_fall_Poi$Mean))
)

residual_tot <- residual_tot %>%
  mutate(name = denomination) %>%
  st_cast()

residual_tot$name <- as.factor(residual_tot$name)
residual_tot$name <- factor(residual_tot$name, levels = c("spring_Bin.dist",
                                                          "spring_LogN.dist",
                                                          "spring_Poi.dist",
                                                          "summer_Bin.dist",
                                                          "summer_LogN.dist",
                                                          "summer_Poi.dist",
                                                          "fall_Bin.dist",
                                                          "fall_LogN.dist",
                                                          "fall_Poi.dist"))
residual_tot_dist <- residual_tot

#####
residual_tot <- rbind(residual_tot_null, residual_tot_dist)

plot_tot<- ggplot(residual_tot) + 
  geom_sf(aes(fill = Mean), color = NA)+
  labs(x = "", y = "", fill = "Residu") +
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  scale_fill_viridis_c()+
  facet_wrap( ~ name, ncol = 9, nrow = 2)+
  theme()
ggsave2(plot = plot_tot, paste(path.output,"residu/","residual_comparison_",annee,".png", sep = ""), width = 16, height = 9 )


residual_tot$name <- factor(residual_tot$name, levels = c("spring_Bin.null",
                                                          "spring_LogN.null",
                                                          "spring_Poi.null",
                                                          "spring_Bin.dist",
                                                          "spring_LogN.dist",
                                                          "spring_Poi.dist",
                                                          "summer_Bin.null",
                                                          "summer_LogN.null",
                                                          "summer_Poi.null",
                                                          "summer_Bin.dist",
                                                          "summer_LogN.dist",
                                                          "summer_Poi.dist",
                                                          "fall_Bin.null",
                                                          "fall_LogN.null",
                                                          "fall_Poi.null",
                                                          "fall_Bin.dist",
                                                          "fall_LogN.dist",
                                                          "fall_Poi.dist"))
plot_tot<- ggplot(residual_tot) + 
  geom_sf(aes(fill = Mean), color = NA)+
  labs(x = "", y = "", fill = "Residu") +
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  scale_fill_viridis_c()+
  facet_wrap( ~ name, ncol = 6, nrow = 3)+
  theme()
