for (annee in as.numeric(periode)){
  ###### model dist ####
  data_model_pred <- sf::st_read("C:/Users/ahebertb/Desktop/StageAurel/20230725_surveys_env_data_894.gpkg")
  
  data_model_pred <- data_model_pred %>% 
    filter(year == annee) %>%
    filter(season != "summer")
  
  spde <- inla.spde2.pcmatern(
    mesh = mesh_GdG, 
    alpha = 1.5,
    prior.range = c(0.6, 0.1), # P(range < 0.6) = 0.1 (0.6 = 1.8/3)
    prior.sigma = c(log(5) / 3, 0.01) # P(sigma > log(5) / 3) = 0.01
  ) 
  
  timesn <- max(data_model_pred$year) - min(data_model_pred$year) + 1 #attention max(timesn) doit être = max(group)
  #sn <- length(levels(data_model_fin$season)) #number of season
  sn <- 2
  
  field.z.idx <- inla.spde.make.index(name = 'x', 
                                      n.spde = spde$n.spde, n.group = sn) 
  
  field.zc.idx <- inla.spde.make.index(name = 'xc', 
                                       n.spde = spde$n.spde, n.group = timesn*sn) 
  
  field.y.idx <- inla.spde.make.index(name = 'u', 
                                      n.spde = spde$n.spde, n.group = sn)
  
  field.p.idx <- inla.spde.make.index(name = 'p', 
                                      n.spde = spde$n.spde, n.group = sn)
  
  
  #we create group for season
  data_model_pred <- data_model_pred %>% mutate(indic_season = ifelse(data_model_pred$season == "spring", 1, 2))  
  
  data_model_pred <- data_model_pred %>% mutate(group = indic_season) %>% #we have only one year
    st_cast()
  
  
  #data_model_pred <- data_model_pred %>% mutate(group = ifelse(indic_season == 1,
  #                                            2*(year - min(year)) + 1 ,
  #                                            2*(year - min(year)) + 2 )) %>%
  #  st_cast()
  # we create an indicator where odd number are spring and even are fall
  
  data_model.spring <- data_model_pred %>% mutate(
    indice = ifelse(indic_season == 2 , NA, indice),
    biomass = ifelse(indic_season == 2 , NA, biomass),
    presence = ifelse(indic_season == 2 , NA, presence)
  )
  
  data_model.fall <- data_model_pred %>% mutate(
    indice = ifelse(indic_season == 1, NA, indice),
    biomass = ifelse(indic_season == 1, NA, biomass),
    presence = ifelse(indic_season == 1, NA, presence)
  )
  
  coo.spring <- data_model.spring %>%
    st_drop_geometry() %>%
    dplyr::select(Xgd, Ygd)
  
  A.spring <- inla.spde.make.A(mesh = mesh_GdG, 
                               # must be a matrix
                               loc = coo.spring %>%
                                 as.matrix(), 
                               group = data_model_pred$group,
  )
  
  coo.fall <- data_model.fall %>%
    st_drop_geometry() %>%
    dplyr::select(Xgd, Ygd)
  
  A.fall <- inla.spde.make.A(mesh = mesh_GdG, 
                             # must be a matrix
                             loc = coo.fall %>%
                               as.matrix(), 
                             group = data_model_pred$group,
  )
  
  # we create two matrix of the same length but each one have info just about one season
  
  
  
  # Prepare the prediction for Gaussian field
  grille_pred <- sf::st_read("C:/Users/ahebertb/Desktop/StageAurel/20230724_envdata_bob.gpkg", layer = "2021")
  
  grille_pred <- grille_pred %>% st_transform(., 4326) %>% 
    filter(season == "summer")
  
  ggplot(grille_pred)+geom_sf()
  
  coord_pred <- grille_pred %>% st_centroid(.) %>% st_coordinates(.)
  
  A_pred <- inla.spde.make.A(mesh = mesh_GdG, 
                             # must be a matrix
                             loc = coord_pred %>%
                               as.matrix(), 
                             group = 2,
  )
  
  # standardize covariates
  std <- function(x, xref = NULL) {
    if(is.null(xref)) {
      out <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    } else {
      out <- (x - mean(xref, na.rm = TRUE)) / sd(xref, na.rm = TRUE)
    }
    return(out)
  }
  
  #stack
  stk.z.spring <- inla.stack(
    data = list(Y = cbind(as.vector(data_model.spring$indice), NA, NA), e = data_model.spring$weight , link = 1), 
    A = list(A.spring, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.spring = rep(1, nrow(data_model.spring))
    ),
    tag = 'occ'
  ) 
  
  stk.y.spring <- inla.stack(
    data = list(Y = cbind(NA, as.vector(data_model.spring$biomass),NA), e = data_model.spring$weight, link = 2),
    A = list(A.spring, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.spring = rep(1, nrow(data_model.spring))
    ), 
    tag = 'dens'
  ) 
  
  stk.p.spring <- inla.stack(
    data = list(Y = cbind(NA, NA, as.vector(data_model.spring$presence)), e = data_model.spring$weight, link = 3),
    A = list(A.spring, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.spring = rep(1, nrow(data_model.spring))
    ), 
    tag = 'pres'
  )
  
  
  stk.z.fall <- inla.stack(
    data = list(Y = cbind(as.vector(data_model.fall$indice), NA, NA), e = data_model.fall$weight, link = 1), 
    A = list(A.fall, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.fall = rep(1, nrow(data_model.fall))
    ), 
    tag = 'occ'
  ) 
  
  stk.y.fall <- inla.stack(
    data = list(Y = cbind(NA, as.vector(data_model.fall$biomass),NA), e = data_model.fall$weight,  link = 2),
    A = list(A.fall, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.fall = rep(1, nrow(data_model.fall))
    ), 
    tag = 'dens'
  ) 
  
  stk.p.fall <- inla.stack(
    data = list(Y = cbind(NA, NA, as.vector(data_model.fall$presence)), e = data_model.fall$weight, link = 3),
    A = list(A.fall, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.fall = rep(1, nrow(data_model.fall))
    ), 
    tag = 'pres'
  )
  
  
  #stack prediction
  empty.pred <- rep(NA, length(coord_pred)/2)
  
  stk.z.spring.pred <- inla.stack(
    data = list(Y = cbind(empty.pred, NA, NA),
                e = rep(1, length(coord_pred)/2), link = 1), 
    A = list(A_pred, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.spring = rep(1, nrow(A_pred))
    ),
    tag = 'occ.pred'
  ) 
  
  stk.y.spring.pred <- inla.stack(
    data = list(Y = cbind(NA, empty.pred, NA),
                e = rep(1, length(coord_pred)/2), link = 2),
    A = list(A_pred, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.spring = rep(1, nrow(A_pred))
    ), 
    tag = 'dens.pred'
  ) 
  
  stk.p.spring.pred <- inla.stack(
    data = list(Y = cbind(NA, NA, empty.pred), e = rep(0, length(coord_pred)/2), link = 3),
    A = list(A_pred, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.spring = rep(1, nrow(A_pred))
    ), 
    tag = 'pres.pred'
  )
  
  
  stk.z.fall.pred <- inla.stack(
    data = list(Y = cbind(empty.pred, NA, NA), e = rep(1, length(coord_pred)/2), link = 1), 
    A = list(A_pred, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.fall = rep(1, nrow(A_pred))
    ), 
    tag = 'occ.pred'
  ) 
  
  stk.y.fall.pred <- inla.stack(
    data = list(Y = cbind(NA, empty.pred,NA), e = rep(1, length(coord_pred)/2),  link = 2),
    A = list(A_pred, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.fall = rep(1, nrow(A_pred))
    ), 
    tag = 'dens.pred'
  ) 
  
  stk.p.fall.pred <- inla.stack(
    data = list(Y = cbind(NA, NA, empty.pred), e = rep(0,
                                                       length(coord_pred)/2), link = 3),
    A = list(A_pred, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.fall = rep(1, nrow(A_pred))
    ), 
    tag = 'pres.pred'
  )
  
  
  
  
  
  
  
  stk.all <- inla.stack(stk.z.spring, stk.y.spring, stk.p.spring,
                        stk.z.fall, stk.y.fall, stk.p.fall,
                        stk.z.spring.pred, stk.y.spring.pred, stk.p.spring.pred,
                        stk.z.fall.pred, stk.y.fall.pred, stk.p.fall.pred)
  
  
  
  h.spec <- list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))
  
  #intercept prior
  prior.fixed <- list(mean.intercept = 0.0, prec.intercept = 1 / (log(10) / 2)^2, mean = 0.0, prec = 1 / (log(5) / 2)^2) 
  
  cinla <- list(strategy = 'simplified.laplace', int.strategy = 'eb',  control.vb=list(emergency= 30))
  
  bprior <- list(prior = 'gaussian', param = c(0, 1))
  
  formula.joint <- Y ~ -1 + 
    z.intercept.spring + y.intercept.spring + p.intercept.spring +
    z.intercept.fall + y.intercept.fall + p.intercept.fall +
    f(x, model = spde, group = x.group) + #binom field
    f(xc, model = spde, fixed = FALSE, #hyper = list(beta = bprior), 
      group = xc.group,
      control.group = list(model = 'ar1', hyper = h.spec)) + #shared field
    f(u, model = spde, group = u.group) + #gamma field
    f(p, model = spde, group = p.group) #PPP field
  
  res.jo <- inla(formula.joint, family = c("binomial", "lognormal", "poisson"), 
                 data = inla.stack.data(stk.all), 
                 control.predictor = list(A = inla.stack.A(stk.all), link = link, compute = TRUE),
                 control.compute = list(dic = FALSE, waic = TRUE, cpo = TRUE,
                                        config = TRUE, residuals = TRUE), control.inla = cinla, control.fixed = prior.fixed, 
                 verbose = TRUE, E = inla.stack.data(stk.all)$e)
  
  saveRDS(res.jo, file = paste(local_path,"/code/output/result/result_saison_",annee,"_894",".rds", sep = ""))
  saveRDS(summary(res.jo), file = paste(local_path,"/code/output/summary/summary_",annee,"_894",".rds", sep = ""))
  
  
  gc()
  
  index <- inla.stack.index(stack = stk.all, tag = "occ.pred")$data
  pred <- grille_pred %>%
    mutate(spring = res.jo$summary.fitted.values[index[1:12380], "mean"],
           fall = res.jo$summary.fitted.values[index[1:12380 + 12380], "mean"]
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  p1 <- pred %>%
    ggplot() +
    geom_sf(aes(fill = spring), color = NA) +
    scale_fill_viridis_c(name = "Presence - Spring") +
    theme_bw()
  
  p2 <- pred %>%
    ggplot() +
    geom_sf(aes(fill = fall), color = NA) +
    scale_fill_viridis_c(name = "Presence - Fall") +
    theme_bw()
  
  plot_tot <- plot_grid(p1, p2)
  ggsave2(plot = plot_tot, paste(path.output,"pred/","binom_null_",annee,".png", sep = ""), width = 8.50, height = 6.50 )
  
  
  
  
  index <- inla.stack.index(stack = stk.all, tag = "dens.pred")$data
  #logNormal need to add the sigma
  pred <- grille_pred %>%
    mutate(spring = exp(res.jo$summary.fitted.values[index[1:12380], "mean"] + 
                          0.5/res.jo$summary.hyperpar$mean[1]),
           fall = exp(res.jo$summary.fitted.values[index[1:12380 + 12380], "mean"]+ 
                        0.5/res.jo$summary.hyperpar$mean[1])
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  p1 <- pred %>%
    ggplot() +
    geom_sf(aes(fill = spring), color = NA) +
    scale_fill_viridis_c(name = "Biomass - Spring", trans = "log10") +
    theme_bw()
  
  p2 <- pred %>%
    ggplot() +
    geom_sf(aes(fill = fall), color = NA) +
    scale_fill_viridis_c(name = "Biomass - Fall", trans = "log10") +
    theme_bw()
  
  plot_tot <- plot_grid(p1, p2)
  ggsave2(plot = plot_tot, paste(path.output,"pred/","lognorm_null_",annee,".png", sep = ""), width = 8.50, height = 6.50 )
  
  
  
  index <- inla.stack.index(stack = stk.all, tag = "pres.pred")$data
  pred <- grille_pred %>%
    mutate(spring = res.jo$summary.fitted.values[index[1:12380], "mean"],
           fall = res.jo$summary.fitted.values[index[1:12380 + 12380], "mean"]
    ) %>%
    st_as_sf() %>%
    st_cast()
  
  # saturation
  seuil1 <- sort(pred$spring)[0.98*length(pred$spring)]
  seuil2 <- sort(pred$fall)[0.98*length(pred$fall)]
  
  pred <- pred %>% mutate (spring = ifelse(spring > seuil1, seuil1, spring),
                           fall = ifelse(fall > seuil2, seuil2, fall))
  
  p1 <- pred %>%
    ggplot() +
    geom_sf(aes(fill = spring), color = NA) +
    scale_fill_viridis_c(name = "Peche - Spring") +
    theme_bw()
  
  p2 <- pred %>%
    ggplot() +
    geom_sf(aes(fill = fall), color = NA) +
    scale_fill_viridis_c(name = "Peche - Fall") +
    theme_bw()
  
  plot_tot <- plot_grid(p1, p2)
  ggsave2(plot = plot_tot, paste(path.output,"pred/","ppp_null_",annee,".png", sep = ""), width = 8.50, height = 6.50 )
  
  
}
