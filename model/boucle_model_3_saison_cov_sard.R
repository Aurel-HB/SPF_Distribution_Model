for (annee in as.numeric(periode)){
  data_model_fin <- sf::st_read(paste(local_path,"code/environment/20230725_surveys_env_data_894.gpkg", sep =""))
  
  data_model_fin <- data_model_fin %>% 
    filter(year == annee)
  
  spde <- inla.spde2.pcmatern(
    mesh = mesh_GdG, 
    alpha = 1.5,
    prior.range = c(0.6, 0.1), # P(range < 0.6) = 0.1 (0.6 = 1.8/3)
    prior.sigma = c(log(5) / 3, 0.01) # P(sigma > log(5) / 3) = 0.01
  ) 
  
  timesn <- max(data_model_fin$year) - min(data_model_fin$year) + 1 #attention max(timesn) doit être = max(group)
  sn <- 3 #number of season
  
  
  field.z.idx <- inla.spde.make.index(name = 'x', 
                                      n.spde = spde$n.spde, n.group = sn) 
  
  field.zc.idx <- inla.spde.make.index(name = 'xc', 
                                       n.spde = spde$n.spde, n.group = timesn*sn) 
  
  field.y.idx <- inla.spde.make.index(name = 'u', 
                                      n.spde = spde$n.spde, n.group = sn)
  
  field.p.idx <- inla.spde.make.index(name = 'p', 
                                      n.spde = spde$n.spde, n.group = sn)
  

  #we create group for season
  data_model_fin <- data_model_fin %>% mutate(indic_season = ifelse(data_model_fin$season == "spring", 1, 2))  
  data_model_fin <- data_model_fin %>% mutate(indic_season = ifelse(data_model_fin$season == "fall", 3, indic_season))
  
  data_model_fin <- data_model_fin %>% mutate(group = indic_season) %>% #we have only one year
    st_cast()
  
  
  #data_model_fin <- data_model_fin %>% mutate(group = ifelse(indic_season == 1,
  #                                            2*(year - min(year)) + 1 ,
  #                                            2*(year - min(year)) + 2 )) %>%
  #  st_cast()
  # we create an indicator where odd number are spring and even are fall
  
  data_model.spring <- data_model_fin %>% mutate(
    indice = ifelse(indic_season %in% c(2,3), NA, indice),
    biomass = ifelse(indic_season %in% c(2,3), NA, biomass),
    presence = ifelse(indic_season %in% c(2,3), NA, presence)
  )
  
  data_model.summer <- data_model_fin %>% mutate(
    indice = ifelse(indic_season %in% c(1,3), NA, indice),
    biomass = ifelse(indic_season %in% c(1,3), NA, biomass),
    presence = ifelse(indic_season %in% c(1,3), NA, presence)
  )
  
  data_model.fall <- data_model_fin %>% mutate(
    indice = ifelse(indic_season %in% c(1,2), NA, indice),
    biomass = ifelse(indic_season %in% c(1,2), NA, biomass),
    presence = ifelse(indic_season %in% c(1,2), NA, presence)
  )
  
  coo.spring <- data_model.spring %>%
    st_drop_geometry() %>%
    dplyr::select(Xgd, Ygd)
  
  A.spring <- inla.spde.make.A(mesh = mesh_GdG, 
                               # must be a matrix
                               loc = coo.spring %>%
                                 as.matrix(), 
                               group = data_model_fin$group,
  )
  
  coo.summer <- data_model.summer %>%
    st_drop_geometry() %>%
    dplyr::select(Xgd, Ygd)
  
  A.summer <- inla.spde.make.A(mesh = mesh_GdG, 
                               # must be a matrix
                               loc = coo.summer %>%
                                 as.matrix(), 
                               group = data_model_fin$group,
  )
  
  coo.fall <- data_model.fall %>%
    st_drop_geometry() %>%
    dplyr::select(Xgd, Ygd)
  
  A.fall <- inla.spde.make.A(mesh = mesh_GdG, 
                             # must be a matrix
                             loc = coo.fall %>%
                               as.matrix(), 
                             group = data_model_fin$group,
  )
  
  # we create two matrix of the same length but each one have info just about one season
  
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
  
  # standardize covariates
  std <- function(x, xref = NULL) {
    if(is.null(xref)) {
      out <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    } else {
      out <- (x - mean(xref, na.rm = TRUE)) / sd(xref, na.rm = TRUE)
    }
    return(out)
  }
  
  
  stk.z.spring <- inla.stack(
    data = list(Y = cbind(as.vector(data_model.spring$indice), NA, NA), e = data_model.spring$weight , link = 1), 
    A = list(A.spring, 1, 1), # add a 1 to every covariate you add
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.spring = rep(1, nrow(data_model.spring)),
                   z.spring.Dist = std(data_model.spring$dist2coast_km) # it's possible to add other covariates like for this one
    ),
    tag = 'occ'
  ) 
  
  stk.y.spring <- inla.stack(
    data = list(Y = cbind(NA, as.vector(data_model.spring$biomass),NA), e = data_model.spring$weight, link = 2),
    A = list(A.spring, 1, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.spring = rep(1, nrow(data_model.spring)), 
                   y.spring.Dist = std(data_model.spring$dist2coast_km)
    ), 
    tag = 'dens'
  ) 
  
  stk.p.spring <- inla.stack(
    data = list(Y = cbind(NA, NA, as.vector(data_model.spring$presence)), e = data_model.spring$weight, link = 3),
    A = list(A.spring, 1, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.spring = rep(1, nrow(data_model.spring)),
                   p.spring.Dist = std(data_model.spring$dist2coast_km)
    ), 
    tag = 'pres'
  )
  
  
  
  
  stk.z.summer <- inla.stack(
    data = list(Y = cbind(as.vector(data_model.summer$indice), NA, NA), e = data_model.summer$weight , link = 1), 
    A = list(A.summer, 1, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.summer = rep(1, nrow(data_model.summer)),
                   z.summer.Dist = std(data_model.summer$dist2coast_km)
    ),
    tag = 'occ'
  ) 
  
  stk.y.summer <- inla.stack(
    data = list(Y = cbind(NA, as.vector(data_model.summer$biomass),NA), e = data_model.summer$weight, link = 2),
    A = list(A.summer, 1, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.summer = rep(1, nrow(data_model.summer)), 
                   y.summer.Dist = std(data_model.summer$dist2coast_km)
    ), 
    tag = 'dens'
  ) 
  
  stk.p.summer <- inla.stack(
    data = list(Y = cbind(NA, NA, as.vector(data_model.summer$presence)), e = data_model.summer$weight, link = 3),
    A = list(A.summer, 1, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.summer = rep(1, nrow(data_model.summer)),
                   p.summer.Dist = std(data_model.summer$dist2coast_km)
    ), 
    tag = 'pres'
  )
  
  
  stk.z.fall <- inla.stack(
    data = list(Y = cbind(as.vector(data_model.fall$indice), NA, NA), e = data_model.fall$weight, link = 1), 
    A = list(A.fall, 1, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.fall = rep(1, nrow(data_model.fall)),
                   z.fall.Dist = std(data_model.fall$dist2coast_km)
    ), 
    tag = 'occ'
  ) 
  
  stk.y.fall <- inla.stack(
    data = list(Y = cbind(NA, as.vector(data_model.fall$biomass),NA), e = data_model.fall$weight,  link = 2),
    A = list(A.fall, 1, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.fall = rep(1, nrow(data_model.fall)), 
                   y.fall.Dist = std(data_model.fall$dist2coast_km)
    ), 
    tag = 'dens'
  ) 
  
  stk.p.fall <- inla.stack(
    data = list(Y = cbind(NA, NA, as.vector(data_model.fall$presence)), e = data_model.fall$weight, link = 3),
    A = list(A.fall, 1, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.fall = rep(1, nrow(data_model.fall)),
                   p.fall.Dist = std(data_model.fall$dist2coast_km)
    ), 
    tag = 'pres'
  )
  
  
  #stack prediction
  empty.pred <- rep(NA, length(coord_pred)/2)
  
  stk.z.spring.pred <- inla.stack(
    data = list(Y = cbind(empty.pred, NA, NA),
                e = rep(1, length(coord_pred)/2), link = 1), 
    A = list(A_pred, 1, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.spring = rep(1, nrow(A_pred)),
                   z.spring.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.spring$dist2coast_km)
    ),
    tag = 'occ.pred'
  ) 
  
  stk.y.spring.pred <- inla.stack(
    data = list(Y = cbind(NA, empty.pred, NA),
                e = rep(1, length(coord_pred)/2), link = 2),
    A = list(A_pred, 1, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.spring = rep(1, nrow(A_pred)), 
                   y.spring.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.spring$dist2coast_km)
    ), 
    tag = 'dens.pred'
  ) 
  
  stk.p.spring.pred <- inla.stack(
    data = list(Y = cbind(NA, NA, empty.pred), e = rep(0, length(coord_pred)/2), link = 3),
    A = list(A_pred, 1, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.spring = rep(1, nrow(A_pred)),
                   p.spring.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.spring$dist2coast_km)
    ), 
    tag = 'pres.pred'
  )
  
  
  stk.z.summer.pred <- inla.stack(
    data = list(Y = cbind(empty.pred, NA, NA),
                e = rep(1, length(coord_pred)/2), link = 1), 
    A = list(A_pred, 1, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.summer = rep(1, nrow(A_pred)),
                   z.summer.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.summer$dist2coast_km)
    ),
    tag = 'occ.pred'
  ) 
  
  stk.y.summer.pred <- inla.stack(
    data = list(Y = cbind(NA, empty.pred, NA),
                e = rep(1, length(coord_pred)/2), link = 2),
    A = list(A_pred, 1, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.summer = rep(1, nrow(A_pred)), 
                   y.summer.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.summer$dist2coast_km)
    ), 
    tag = 'dens.pred'
  ) 
  
  stk.p.summer.pred <- inla.stack(
    data = list(Y = cbind(NA, NA, empty.pred), e = rep(0, length(coord_pred)/2), link = 3),
    A = list(A_pred, 1, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.summer = rep(1, nrow(A_pred)),
                   p.summer.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.summer$dist2coast_km)
    ), 
    tag = 'pres.pred'
  )
  
  
  
  stk.z.fall.pred <- inla.stack(
    data = list(Y = cbind(empty.pred, NA, NA), e = rep(1, length(coord_pred)/2), link = 1), 
    A = list(A_pred, 1, 1),
    effects = list(c(field.z.idx, field.zc.idx), 
                   z.intercept.fall = rep(1, nrow(A_pred)),
                   z.fall.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.fall$dist2coast_km)
    ), 
    tag = 'occ.pred'
  ) 
  
  stk.y.fall.pred <- inla.stack(
    data = list(Y = cbind(NA, empty.pred,NA), e = rep(1, length(coord_pred)/2),  link = 2),
    A = list(A_pred, 1, 1),
    effects = list(c(field.y.idx, field.zc.idx), 
                   y.intercept.fall = rep(1, nrow(A_pred)), 
                   y.fall.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.fall$dist2coast_km)
    ), 
    tag = 'dens.pred'
  ) 
  
  stk.p.fall.pred <- inla.stack(
    data = list(Y = cbind(NA, NA, empty.pred), e = rep(0,
                                                       length(coord_pred)/2), link = 3),
    A = list(A_pred, 1, 1),
    effects = list(c(field.p.idx, field.zc.idx), 
                   p.intercept.fall = rep(1, nrow(A_pred)),
                   p.fall.Dist = std(x = grille_pred$dist2coast_km, xref = data_model.fall$dist2coast_km)
    ), 
    tag = 'pres.pred'
  )
  
  
  stk.all <- inla.stack(stk.z.spring, stk.y.spring, stk.p.spring,
                        stk.z.summer, stk.y.summer, stk.p.summer,
                        stk.z.fall, stk.y.fall, stk.p.fall,
                        stk.z.spring.pred, stk.y.spring.pred, stk.p.spring.pred,
                        stk.z.summer.pred, stk.y.summer.pred, stk.p.summer.pred,
                        stk.z.fall.pred, stk.y.fall.pred, stk.p.fall.pred)

  
  
  h.spec <- list(rho = list(prior = 'pc.cor1', param = c(0, 0.9)))
  
  #intercept prior
  prior.fixed <- list(mean.intercept = 0.0, prec.intercept = 1 / (log(10) / 2)^2, mean = 0.0, prec = 1 / (log(5) / 2)^2) 
  
  cinla <- list(strategy = 'simplified.laplace', int.strategy = 'eb',  control.vb=list(emergency= 30))
  
  bprior <- list(prior = 'gaussian', param = c(0, 1))
  
  formula.joint <- Y ~ -1 + 
    z.intercept.spring + y.intercept.spring + p.intercept.spring +
    z.intercept.summer + y.intercept.summer + 
    p.intercept.summer +
    z.intercept.fall + y.intercept.fall + p.intercept.fall + 
    z.spring.Dist + y.spring.Dist + p.spring.Dist +
    z.summer.Dist + y.summer.Dist +
    p.summer.Dist +
    z.fall.Dist + y.fall.Dist + p.fall.Dist +
    f(x, model = spde, group = x.group, control.group = list(model = "iid")) + #binom field
    f(xc, model = spde, fixed = FALSE, #hyper = list(beta = bprior), 
      group = xc.group,
      control.group = list(model = 'ar1', hyper = h.spec)) + #shared field
    f(u, model = spde, group = u.group, control.group = list(model = "iid")) + #gamma field
    f(p, model = spde, group = p.group, control.group = list(model = "iid")) #PPP field
  
  res.jo <- inla(formula.joint, family = c("binomial", "lognormal", "poisson"), 
                 data = inla.stack.data(stk.all), 
                 control.predictor = list(A = inla.stack.A(stk.all), link = link, compute = TRUE),
                 control.compute = list(dic = FALSE, waic = TRUE, cpo = TRUE,
                                        config = TRUE, residuals = TRUE), control.inla = cinla, control.fixed = prior.fixed, 
                 verbose = TRUE, E = inla.stack.data(stk.all)$e)
  
  saveRDS(res.jo, file = paste(local_path,"/code/output/result/result_summer_dist_",annee,"_894",".rds", sep = ""))
  saveRDS(summary(res.jo), file = paste(local_path,"/code/output/summary/summer_summary_dist_",annee,"_894",".rds", sep = ""))
  
}







