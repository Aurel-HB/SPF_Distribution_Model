# because of an issue of prediction by the model we recreate the prediction

######## 3 season ##################

# Prepare the prediction for Gaussian field
grille_pred <- sf::st_read(paste(local_path,"code/environment/20230724_envdata_bob.gpkg", sep = ""), layer = "2021")

grille_pred <- grille_pred %>% st_transform(., 4326) %>% 
  filter(season == "summer")# the season doesn't matter because we don't have cov that change between season

coord_pred <- grille_pred %>% st_centroid(.) %>% st_coordinates(.)

A_pred <- inla.spde.make.A(mesh = mesh_GdG, 
                           # must be a matrix
                           loc = coord_pred %>%
                             as.matrix(), 
                           group = 3,
)

### presence pred #####
pred_binom.spring <- c()
pred_binom.summer <- c()
pred_binom.fall <- c()
associate_year <- c()
binom_tot <- data.frame()

for (annee in as.numeric(periode)){
  # one year and 3 season
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  
  w.binom <- result$summary.random$x$mean
  e.binom <- result$summary.random$xc$mean
  v.binom.spring <- summary.res$fixed[1,1]
  v.binom.summer <- summary.res$fixed[4,1]
  v.binom.fall <- summary.res$fixed[7,1]
  c.binom.spring <- summary.res$fixed[10,1]
  c.binom.summer <- summary.res$fixed[13,1]
  c.binom.fall <- summary.res$fixed[16,1]
  
  pred_binom.spring <- c(pred_binom.spring,
                         w.binom[1:894] + e.binom[1:894] + v.binom.spring + c.binom.spring)
  pred_binom.summer <- c(pred_binom.summer,
                         w.binom[894 + 1:894] + e.binom[894 + 1:894] + v.binom.summer + c.binom.summer)
  pred_binom.fall <- c(pred_binom.fall,
                       w.binom[2*894 + 1:894] + e.binom[2*894 + 1:894] + v.binom.fall + c.binom.fall)
  
  
  
  inv.logit <- function(x){
    x<- exp(x)/(1+exp(x))
    return(x)
  }
  
  
  binom_pred <- grille_pred %>%
    mutate(pred.spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(inv.logit(pred_binom.spring[1:894 + 894*(annee - 2009)]), ncol = 1)),
           pred.summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(inv.logit(pred_binom.summer[1:894 + 894*(annee - 2009)]), ncol = 1)),
           pred.fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(inv.logit(pred_binom.fall[1:894 + 894*(annee - 2009)]), ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  binom <-  c(binom_pred$pred.spring,
              binom_pred$pred.summer,
              binom_pred$pred.fall)
  binom <- data.frame(binom, rep(binom_pred$geom,3))
  binom <- data.frame(binom, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(binom) <- c("pred", "geom", "season")
  binom$season <- factor(binom$season, levels = c("spring","summer","fall"))
  
  binom <- binom %>% 
    mutate(year = annee) %>%
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  binom_tot <- rbind(binom_tot, binom)
  
  plot.binom <- binom %>% 
    ggplot() + 
    geom_sf(aes(fill = pred), color = NA) +
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    labs(x = "", y = "", fill = "", 
         title = "") +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    facet_wrap( ~ season, ncol = 3, nrow = 1) +
    theme_bw()
  ggsave(plot = plot.binom, paste(path.output,"pred/","3_binom_pred_dist_",annee,".png", sep = ""), width = 16, height = 9)
  
}

# recuperation of the estimation error
pred_binom.spring <- c()
pred_binom.summer <- c()
pred_binom.fall <- c()
associate_year <- c()
binom_tot_error <- data.frame()

for (annee in as.numeric(periode)){
  # one year and 3 season
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  
  w.binom <- result$summary.random$x$sd
  e.binom <- result$summary.random$xc$sd
  v.binom.spring <- summary.res$fixed[1,1]
  v.binom.summer <- summary.res$fixed[4,1]
  v.binom.fall <- summary.res$fixed[7,1]
  c.binom.spring <- summary.res$fixed[10,1]
  c.binom.summer <- summary.res$fixed[13,1]
  c.binom.fall <- summary.res$fixed[16,1]
  
  pred_binom.spring <- c(pred_binom.spring,
                         w.binom[1:894] + e.binom[1:894] + v.binom.spring + c.binom.spring)
  pred_binom.summer <- c(pred_binom.summer,
                         w.binom[894 + 1:894] + e.binom[894 + 1:894] + v.binom.summer + c.binom.summer)
  pred_binom.fall <- c(pred_binom.fall,
                       w.binom[2*894 + 1:894] + e.binom[2*894 + 1:894] + v.binom.fall + c.binom.fall)
  
  
  
  inv.logit <- function(x){
    x<- exp(x)/(1+exp(x))
    return(x)
  }
  
  
  binom_pred <- grille_pred %>%
    mutate(pred.spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(inv.logit(pred_binom.spring[1:894 + 894*(annee - 2009)]), ncol = 1)),
           pred.summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(inv.logit(pred_binom.summer[1:894 + 894*(annee - 2009)]), ncol = 1)),
           pred.fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(inv.logit(pred_binom.fall[1:894 + 894*(annee - 2009)]), ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  binom <-  c(binom_pred$pred.spring,
              binom_pred$pred.summer,
              binom_pred$pred.fall)
  binom <- data.frame(binom, rep(binom_pred$geom,3))
  binom <- data.frame(binom, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(binom) <- c("pred", "geom", "season")
  binom$season <- factor(binom$season, levels = c("spring","summer","fall"))
  
  binom <- binom %>% 
    mutate(year = annee) %>%
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  binom_tot_error <- rbind(binom_tot_error, binom)
}


### creation of the summaries map over the series

sum_binom_tot <- binom_tot %>% filter(year == 2009) #we prepare a year structure
sum_binom_tot <- sum_binom_tot[2]

sum_binom_error <- binom_tot_error %>% filter(year == 2009) #we prepare a year structure
sum_binom_error <- sum_binom_error[2]


for (annee in as.numeric(periode)){
  temp_binom <- binom_tot %>% filter(year == annee)
  sum_binom_tot <- cbind(sum_binom_tot, temp_binom$pred)
  
  temp_binom <- binom_tot_error %>% filter(year == annee)
  sum_binom_error <- cbind(sum_binom_error, temp_binom$pred) 
}
binom_mean <- c()
binom_sd <- c()
binom_error <- c()

for (line in 1:37140){
  binom_mean <- c(binom_mean, mean(c(as.numeric(sum_binom_tot[line,2:15][1:14]))[1:14]))
  binom_sd <- c(binom_sd, sd(c(as.numeric(sum_binom_tot[line,2:15][1:14]))[1:14]))
  binom_error <- c(binom_error, mean(c(as.numeric(sum_binom_error[line,2:15][1:14]))[1:14]))
}

value <- c(binom_mean, binom_sd, binom_error)
sum_binom_tot <- binom_tot %>% filter(year %in% c(2009:2011)) 
sum_binom_tot <- sum_binom_tot[4]

sum_binom_tot <- sum_binom_tot %>% mutate(value = value,
                                            type = c(rep("avg_pred_pres", 37140),
                                                     rep("pres_sd_inter_annual", 37140),
                                                     rep("average_error", 37140)),
                                            season = rep(c(rep("spring", 12380),
                                                           rep("summer", 12380),
                                                           rep("fall", 12380)),
                                                         3)
) %>%
  st_cast()

sum_binom_tot$season <- as.factor(sum_binom_tot$season)
sum_binom_tot$season <- factor(sum_binom_tot$season, levels = c("spring","summer","fall"))

sum_binom_tot$type <- as.factor(sum_binom_tot$type)
sum_binom_tot$type <- factor(sum_binom_tot$type, levels = c("avg_pred_pres","pres_sd_inter_annual","average_error"))

binom_tot_plot <- sum_binom_tot %>% 
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 3, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

ggsave(binom_tot_plot, paste(path.output,"pred/","pred_binom.png", sep = ""),
        width = 9, height = 9)

binom_tot_plot.1 <- sum_binom_tot %>% filter(type %in% c("avg_pred_pres", "pres_sd_inter_annual")) %>%
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 2, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

binom_tot_plot.2 <- sum_binom_tot %>% filter(type == "average_error") %>% 
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 1, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

plot_grid(binom_tot_plot.1, binom_tot_plot.2, align = "h", axis = "tblr",
          rel_widths = c(1.8,1))
ggsave2(paste(path.output,"pred/","pres_pred.png", sep = ""),
        width = 9, height = 9)



#####
### intensity pred #####
pred_logn.spring <- c()
pred_logn.summer <- c()
pred_logn.fall <- c()
associate_year <- c()
logn_tot <- data.frame()

for (annee in as.numeric(periode)){
  # one year and 3 season
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  
  w.logn <- result$summary.random$u$mean
  e.logn <- result$summary.random$xc$mean
  v.logn.spring <- summary.res$fixed[2,1]
  v.logn.summer <- summary.res$fixed[5,1]
  v.logn.fall <- summary.res$fixed[8,1]
  c.logn.spring <- summary.res$fixed[11,1]
  c.logn.summer <- summary.res$fixed[14,1]
  c.logn.fall <- summary.res$fixed[17,1]
  
  pred_logn.spring <- c(pred_logn.spring,
                         w.logn[1:894] + e.logn[1:894] + v.logn.spring + c.logn.spring)
  pred_logn.summer <- c(pred_logn.summer,
                         w.logn[894 + 1:894] + e.logn[894 + 1:894] + v.logn.summer + c.logn.summer)
  pred_logn.fall <- c(pred_logn.fall,
                       w.logn[2*894 + 1:894] + e.logn[2*894 + 1:894] + v.logn.fall + c.logn.fall)
  
  
  #with trans in exp()
  #logn_pred <- grille_pred %>%
  #  mutate(pred.spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(exp(pred_logn.spring[1:894 + 894*(annee - 2009)]), ncol = 1)),
  #         pred.summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(exp(pred_logn.summer[1:894 + 894*(annee - 2009)]), ncol = 1)),
  #         pred.fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(exp(pred_logn.fall[1:894 + 894*(annee - 2009)]), ncol = 1))
  #  ) %>%
  #  st_as_sf() %>%
  #  st_cast()
  logn_pred <- grille_pred %>%
    mutate(pred.spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_logn.spring[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_logn.summer[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_logn.fall[1:894 + 894*(annee - 2009)], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  
  logn <-  c(logn_pred$pred.spring,
              logn_pred$pred.summer,
              logn_pred$pred.fall)
  logn <- data.frame(logn, rep(logn_pred$geom,3))
  logn <- data.frame(logn, c(rep("spring", 12380), rep("summer", 12380), rep("fall", 12380)))
  names(logn) <- c("pred", "geom", "season")
  logn$season <- factor(logn$season, levels = c("spring","summer","fall"))
  
  logn <- logn %>% 
    mutate(year = annee) %>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  logn_tot <- rbind(logn_tot, logn)
  
  plot.logn <- logn %>% 
    ggplot() + 
    geom_sf(aes(fill = pred), color = NA) +
    scale_fill_viridis_c() + #trans = "log10"
    scale_color_viridis_c() +
    labs(x = "", y = "", fill = "", 
         title = "") +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    facet_wrap( ~ season, ncol = 3, nrow = 1) +
    theme_bw()
  ggsave(plot = plot.logn, paste(path.output,"pred/","3_logn_pred_dist_",annee,".png", sep = ""), width = 16, height = 9)
  
}

# recuperation of the estimation error
pred_logn.spring <- c()
pred_logn.summer <- c()
pred_logn.fall <- c()
associate_year <- c()
logn_tot_error <- data.frame()

for (annee in as.numeric(periode)){
  # one year and 3 season
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  
  w.logn <- result$summary.random$u$sd
  e.logn <- result$summary.random$xc$sd
  v.logn.spring <- summary.res$fixed[1,1]
  v.logn.summer <- summary.res$fixed[4,1]
  v.logn.fall <- summary.res$fixed[7,1]
  c.logn.spring <- summary.res$fixed[10,1]
  c.logn.summer <- summary.res$fixed[13,1]
  c.logn.fall <- summary.res$fixed[16,1]
  
  pred_logn.spring <- c(pred_logn.spring,
                         w.logn[1:894] + e.logn[1:894] + v.logn.spring + c.logn.spring)
  pred_logn.summer <- c(pred_logn.summer,
                         w.logn[894 + 1:894] + e.logn[894 + 1:894] + v.logn.summer + c.logn.summer)
  pred_logn.fall <- c(pred_logn.fall,
                       w.logn[2*894 + 1:894] + e.logn[2*894 + 1:894] + v.logn.fall + c.logn.fall)
  
  
  logn_pred <- grille_pred %>%
    mutate(pred.spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_logn.spring[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_logn.summer[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_logn.fall[1:894 + 894*(annee - 2009)], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  logn <-  c(logn_pred$pred.spring,
              logn_pred$pred.summer,
              logn_pred$pred.fall)
  logn <- data.frame(logn, rep(logn_pred$geom,3))
  logn <- data.frame(logn, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(logn) <- c("pred", "geom", "season")
  logn$season <- factor(logn$season, levels = c("spring","summer","fall"))
  
  logn <- logn %>% 
    mutate(year = annee) %>%
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  logn_tot_error <- rbind(logn_tot_error, logn)
}


### creation of the summaries map over the series

sum_logn_tot <- logn_tot %>% filter(year == 2009) #we prepare a year structure
sum_logn_tot <- sum_logn_tot[2]

sum_logn_error <- logn_tot_error %>% filter(year == 2009) #we prepare a year structure
sum_logn_error <- sum_logn_error[2]


for (annee in as.numeric(periode)){
  temp_logn <- logn_tot %>% filter(year == annee)
  sum_logn_tot <- cbind(sum_logn_tot, temp_logn$pred)
  
  temp_logn <- logn_tot_error %>% filter(year == annee)
  sum_logn_error <- cbind(sum_logn_error, temp_logn$pred) 
}
logn_mean <- c()
logn_sd <- c()
logn_error <- c()

for (line in 1:37140){
  logn_mean <- c(logn_mean, mean(c(as.numeric(sum_logn_tot[line,2:15][1:14]))[1:14]))
  logn_sd <- c(logn_sd, sd(c(as.numeric(sum_logn_tot[line,2:15][1:14]))[1:14]))
  logn_error <- c(logn_error, mean(c(as.numeric(sum_logn_error[line,2:15][1:14]))[1:14]))
}

value <- c(logn_mean, logn_sd, logn_error)
sum_logn_tot <- logn_tot %>% filter(year %in% c(2009:2011)) 
sum_logn_tot <- sum_logn_tot[4]

sum_logn_tot <- sum_logn_tot %>% mutate(value = value,
                                          type = c(rep("avg_pred_int", 37140),
                                                   rep("int_sd_inter_annual", 37140),
                                                   rep("average_error", 37140)),
                                          season = rep(c(rep("spring", 12380),
                                                         rep("summer", 12380),
                                                         rep("fall", 12380)),
                                                       3)
) %>%
  st_cast()

sum_logn_tot$season <- as.factor(sum_logn_tot$season)
sum_logn_tot$season <- factor(sum_logn_tot$season, levels = c("spring","summer","fall"))

sum_logn_tot$type <- as.factor(sum_logn_tot$type)
sum_logn_tot$type <- factor(sum_logn_tot$type, levels = c("avg_pred_int","int_sd_inter_annual","average_error"))

logn_tot_plot <- sum_logn_tot %>% 
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 3, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

ggsave(logn_tot_plot, paste(path.output,"pred/","pred_logn.png", sep = ""),
       width = 9, height = 9)

logn_tot_plot.1 <- sum_logn_tot %>% filter(type %in% c("avg_pred_int", "int_sd_inter_annual")) %>%
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 2, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

logn_tot_plot.2 <- sum_logn_tot %>% filter(type == "average_error") %>% 
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 1, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

plot_grid(logn_tot_plot.1, logn_tot_plot.2, align = "h", axis = "tblr",
          rel_widths = c(1.8,1))
ggsave2(paste(path.output,"pred/","int_pred.png", sep = ""),
        width = 9, height = 9)

### peche pred #####
pred_ppp.spring <- c()
pred_ppp.summer <- c()
pred_ppp.fall <- c()
associate_year <- c()
ppp_tot <- data.frame()

for (annee in as.numeric(periode)){
  # one year and 3 season
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  
  w.ppp <- result$summary.random$p$mean
  e.ppp <- result$summary.random$xc$mean
  v.ppp.spring <- summary.res$fixed[3,1]
  v.ppp.summer <- summary.res$fixed[6,1]
  v.ppp.fall <- summary.res$fixed[9,1]
  c.ppp.spring <- summary.res$fixed[12,1]
  c.ppp.summer <- summary.res$fixed[15,1]
  c.ppp.fall <- summary.res$fixed[18,1]
  
  pred_ppp.spring <- c(pred_ppp.spring,
                         w.ppp[1:894] + e.ppp[1:894] + v.ppp.spring + c.ppp.spring)
  pred_ppp.summer <- c(pred_ppp.summer,
                         w.ppp[894 + 1:894] + e.ppp[894 + 1:894] + v.ppp.summer + c.ppp.summer)
  pred_ppp.fall <- c(pred_ppp.fall,
                       w.ppp[2*894 + 1:894] + e.ppp[2*894 + 1:894] + v.ppp.fall + c.ppp.fall)

  
  
  ppp_pred <- grille_pred %>%
    mutate(pred.spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_ppp.spring[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_ppp.summer[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_ppp.fall[1:894 + 894*(annee - 2009)], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  ppp <-  c(ppp_pred$pred.spring,
              ppp_pred$pred.summer,
              ppp_pred$pred.fall)
  ppp <- data.frame(ppp, rep(ppp_pred$geom,3))
  ppp <- data.frame(ppp, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(ppp) <- c("pred", "geom", "season")
  ppp$season <- factor(ppp$season, levels = c("spring","summer","fall"))
  
  ppp <- ppp %>% 
    mutate(year = annee) %>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  ppp_tot <- rbind(ppp_tot, ppp)
  
  plot.ppp <- ppp %>% 
    ggplot() + 
    geom_sf(aes(fill = pred), color = NA) +
    labs(x = "", y = "", fill = "", 
         title = "") +
    scale_fill_viridis_c() +
    scale_color_viridis_c() +
    geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
    coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
    facet_wrap( ~ season, ncol = 3, nrow = 1) +
    theme_bw()
  ggsave(plot = plot.ppp, paste(path.output,"pred/","3_ppp_pred_dist_",annee,".png", sep = ""), width = 16, height = 9)
  
}

# recuperation of the estimation error
pred_ppp.spring <- c()
pred_ppp.summer <- c()
pred_ppp.fall <- c()
associate_year <- c()
ppp_tot_error <- data.frame()

for (annee in as.numeric(periode)){
  # one year and 3 season
  summary.res <- readRDS(paste(path.output,"summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  result <- readRDS(paste(path.output,"/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  associate_year <- c(associate_year, rep(annee, A_pred@Dim[[1]]))
  
  w.ppp <- result$summary.random$p$sd
  e.ppp <- result$summary.random$xc$sd
  v.ppp.spring <- summary.res$fixed[1,1]
  v.ppp.summer <- summary.res$fixed[4,1]
  v.ppp.fall <- summary.res$fixed[7,1]
  c.ppp.spring <- summary.res$fixed[10,1]
  c.ppp.summer <- summary.res$fixed[13,1]
  c.ppp.fall <- summary.res$fixed[16,1]
  
  pred_ppp.spring <- c(pred_ppp.spring,
                        w.ppp[1:894] + e.ppp[1:894] + v.ppp.spring + c.ppp.spring)
  pred_ppp.summer <- c(pred_ppp.summer,
                        w.ppp[894 + 1:894] + e.ppp[894 + 1:894] + v.ppp.summer + c.ppp.summer)
  pred_ppp.fall <- c(pred_ppp.fall,
                      w.ppp[2*894 + 1:894] + e.ppp[2*894 + 1:894] + v.ppp.fall + c.ppp.fall)
  
  
  ppp_pred <- grille_pred %>%
    mutate(pred.spring = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_ppp.spring[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.summer = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_ppp.summer[1:894 + 894*(annee - 2009)], ncol = 1)),
           pred.fall = (as.matrix(A_pred)[, 2*894 + 1:894] %*% matrix(pred_ppp.fall[1:894 + 894*(annee - 2009)], ncol = 1))
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  ppp <-  c(ppp_pred$pred.spring,
             ppp_pred$pred.summer,
             ppp_pred$pred.fall)
  ppp <- data.frame(ppp, rep(ppp_pred$geom,3))
  ppp <- data.frame(ppp, c(rep("spring",12380), rep("summer", 12380), rep("fall", 12380)))
  names(ppp) <- c("pred", "geom", "season")
  ppp$season <- factor(ppp$season, levels = c("spring","summer","fall"))
  
  ppp <- ppp %>% 
    mutate(year = annee) %>%
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  ppp_tot_error <- rbind(ppp_tot_error, ppp)
}


### creation of the summaries map over the series

sum_ppp_tot <- ppp_tot %>% filter(year == 2009) #we prepare a year structure
sum_ppp_tot <- sum_ppp_tot[2]

sum_ppp_error <- ppp_tot_error %>% filter(year == 2009) #we prepare a year structure
sum_ppp_error <- sum_ppp_error[2]


for (annee in as.numeric(periode)){
  temp_ppp <- ppp_tot %>% filter(year == annee)
  sum_ppp_tot <- cbind(sum_ppp_tot, temp_ppp$pred)
  
  temp_ppp <- ppp_tot_error %>% filter(year == annee)
  sum_ppp_error <- cbind(sum_ppp_error, temp_ppp$pred) 
}
ppp_mean <- c()
ppp_sd <- c()
ppp_error <- c()

for (line in 1:37140){
  ppp_mean <- c(ppp_mean, mean(c(as.numeric(sum_ppp_tot[line,2:15][1:14]))[1:14]))
  ppp_sd <- c(ppp_sd, sd(c(as.numeric(sum_ppp_tot[line,2:15][1:14]))[1:14]))
  ppp_error <- c(ppp_error, mean(c(as.numeric(sum_ppp_error[line,2:15][1:14]))[1:14]))
}

value <- c(ppp_mean, ppp_sd, ppp_error)
sum_ppp_tot <- ppp_tot %>% filter(year %in% c(2009:2011)) 
sum_ppp_tot <- sum_ppp_tot[4]

sum_ppp_tot <- sum_ppp_tot %>% mutate(value = value,
                                        type = c(rep("avg_pred_fish_int", 37140),
                                                 rep("fish_sd_inter_annual", 37140),
                                                 rep("average_error", 37140)),
                                        season = rep(c(rep("spring", 12380),
                                                       rep("summer", 12380),
                                                       rep("fall", 12380)),
                                                     3)
) %>%
  st_cast()

sum_ppp_tot$season <- as.factor(sum_ppp_tot$season)
sum_ppp_tot$season <- factor(sum_ppp_tot$season, levels = c("spring","summer","fall"))

sum_ppp_tot$type <- as.factor(sum_ppp_tot$type)
sum_ppp_tot$type <- factor(sum_ppp_tot$type, levels = c("avg_pred_fish_int","fish_sd_inter_annual","average_error"))

ppp_tot_plot <- sum_ppp_tot %>% 
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 3, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

ggsave(ppp_tot_plot, paste(path.output,"pred/","pred_ppp.png", sep = ""),
       width = 9, height = 9)

ppp_tot_plot.1 <- sum_ppp_tot %>% filter(type %in% c("avg_pred_fish_int", "fish_sd_inter_annual")) %>%
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 2, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

ppp_tot_plot.2 <- sum_ppp_tot %>% filter(type == "average_error") %>% 
  ggplot() + 
  geom_sf(aes(fill = value), color = NA) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  facet_wrap( season ~ type, ncol = 1, nrow = 3) +
  labs(x = "", y = "", fill = "")+
  geom_sf(data=world %>% filter(SOVEREIGNT %in% c("France","Spain")), fill = "tan")+
  coord_sf(xlim = c(-6,-1), ylim = c(43.5,48.1), expand = FALSE)+
  theme()

plot_grid(ppp_tot_plot.1, ppp_tot_plot.2, align = "h", axis = "tblr",
          rel_widths = c(1.8,1))
ggsave2(paste(path.output,"pred/","fish_pred.png", sep = ""),
        width = 9, height = 9)

##### 
#figure tot comparaison pred and shared field

plot_grid(mean_shared_plot, binom_tot_plot,ppp_tot_plot,ppp_tot_plot, nrow = 1)
ggsave2(paste(path.output,"pred/","3_pred_summary.png", sep = ""), width = 21, height = 12)
