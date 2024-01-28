

#Add a local path to the data and load the neceassary libraries 

# this command have to be use only if you don't have already source it
#source(paste(local_path,"code/script/boot.R",sep=""))



ggplot(study_domain_sf) + 
  geom_sf() + 
  theme_bw()


# Importation of the data

# this command have to be use only if you don't have already source it
#source(paste(local_path,"code/script/load_data.R",sep=""))


#We create the dataset in the good shape with Pelgas 


df.sard_PEL_tot <- sard_PEL_tot %>%
  mutate(Xgd = LONG, Ygd = LAT) %>%
  st_as_sf(., coords = c("LONG","LAT")) %>%
  st_set_crs(., 4326)

df.sard_PEL_tot <- df.sard_PEL_tot %>%
  mutate(inside = in_out_domain(., study_domain_sf)) %>%
  filter(inside == 1) %>%
  mutate(biomass = ifelse(biomass == 0, NA, biomass)) %>%
  mutate(season = "spring")


df.anch_PEL_tot <- anch_PEL_tot %>%
  mutate(Xgd = LONG, Ygd = LAT) %>%
  st_as_sf(., coords = c("LONG","LAT")) %>%
  st_set_crs(., 4326)

df.anch_PEL_tot <- df.anch_PEL_tot %>%
  mutate(inside = in_out_domain(., study_domain_sf)) %>%
  filter(inside == 1) %>%
  mutate(biomass = ifelse(biomass == 0, NA, biomass)) %>%
  mutate(season = "spring")



#We create the dataset in the good shape with Juvena 


df.sard_JUN_tot <- sard_JUN_tot %>%
  mutate(Xgd = LONG, Ygd = LAT) %>%
  st_as_sf(., coords = c("LONG","LAT")) %>%
  st_set_crs(., 4326)


df.sard_JUN_tot <- df.sard_JUN_tot %>%
  mutate(inside = in_out_domain(., study_domain_sf)) %>%
  filter(inside == 1) %>%
  mutate(biomass = ifelse(biomass == 0, NA, biomass)) %>%
  mutate(season = "fall")



df.anch_JUN_tot <- anch_JUN_tot %>%
  mutate(Xgd = LONG, Ygd = LAT) %>%
  st_as_sf(., coords = c("LONG","LAT")) %>%
  st_set_crs(., 4326)


df.anch_JUN_tot <- df.anch_JUN_tot %>%
  mutate(inside = in_out_domain(., study_domain_sf)) %>%
  filter(inside == 1) %>%
  mutate(biomass = ifelse(biomass == 0, NA, biomass)) %>%
  mutate(season = "fall")




#We create the dataset in the good shape of the presence/absence and density with survey


df_sard <- rbind(df.sard_PEL_tot,
                 df.sard_JUN_tot)

saveRDS(df_sard, file = paste(local_path,"/code/data_model/sard_survey",".rds", sep = ""))

df_anch <- rbind(df.anch_PEL_tot,
                 df.anch_JUN_tot)

saveRDS(df_anch, file = paste(local_path,"/code/data_model/anch_survey",".rds", sep = ""))



#We create the dataset in the good shape of the presence-only with VMS

path_data <- paste(local_path, "/code/data/tidy/", sep = "/")
sard_VMS_tot <- readRDS(paste(path_data,"Data_sard_VMS.RDS",sep = ""))

sard_VMS <- sard_VMS_tot[,1:9]
names(sard_VMS) <- c("LONG","LAT","year","month","indice","biomass","type", "survey", "sp")

df.sard_VMS <- sard_VMS %>%
  filter(indice > 0) %>%
  st_as_sf(., coords = c("LONG","LAT")) %>%
  st_set_crs(., 4326)


obs_sard <- df.sard_VMS %>%
  mutate(Xgd = st_coordinates(.) %>%
           as.data.frame() %>%
           pull(X),
         Ygd = st_coordinates(.)%>%
           as.data.frame() %>%
           pull(Y),
         inside = in_out_domain(.,study_domain_sf)
  )

obs_sard$month <- as.factor(obs_sard$month)

gear <- c("PS", "PTM")

obs_sard <- obs_sard %>% filter(inside == 1) %>% filter(month %in% seq(4,10,1)) %>%
  filter(type %in% gear) %>%
  mutate(season = ifelse( month %in% c(4,5), "spring", 
                          ifelse(month %in% c(6,7,8),"summer", "fall")))

obs_sard <- obs_sard %>%
  filter(indice > 0) %>%
  st_as_sf(.) %>%
  st_set_crs(., 4326)

saveRDS(obs_sard, file = paste(local_path,"/code/data_model/sard_presence_VMS",".rds", sep = ""))


ggplot(study_domain_sf) + 
  geom_sf() + 
  geom_sf(data = obs_sard, aes(fill = indice)) +
  theme_bw()




anch_VMS_tot <- readRDS(paste(path_data,"Data_anch_VMS.RDS",sep = ""))

anch_VMS <- anch_VMS_tot[,1:9]
names(anch_VMS) <- c("LONG","LAT","year","month","indice","biomass","type", "survey", "sp")

df.anch_VMS <- anch_VMS %>%
  filter(indice > 0) %>%
  st_as_sf(., coords = c("LONG","LAT")) %>%
  st_set_crs(., 4326)


obs_anch <- df.anch_VMS %>%
  mutate(Xgd = st_coordinates(.) %>%
           as.data.frame() %>%
           pull(X),
         Ygd = st_coordinates(.)%>%
           as.data.frame() %>%
           pull(Y),
         inside = in_out_domain(.,study_domain_sf)
  )

obs_anch$month <- as.factor(obs_anch$month)

gear <- c("PS", "PTM")

obs_anch <- obs_anch %>% filter(inside == 1) %>% filter(month %in% seq(4,10,1)) %>%
  filter(type %in% gear) %>%
  mutate(season = ifelse( month %in% c(4,5), "spring", 
                          ifelse(month %in% c(6,7,8),"summer", "fall")))

obs_anch <- obs_anch %>%
  filter(indice > 0) %>%
  st_as_sf(.) %>%
  st_set_crs(., 4326)

saveRDS(obs_anch, file = paste(local_path,"/code/data_model/anch_presence_VMS",".rds", sep = ""))


ggplot(study_domain_sf) + 
  geom_sf() + 
  geom_sf(data = obs_anch, aes(fill = indice)) +
  theme_bw()

###################################################
#We create the dataset in the good shape of total
###################################################


obs_sard <- obs_sard %>% mutate(
  presence = indice,
  biomass = NA,
  indice = NA
)

df.sard_PEL_tot <- df.sard_PEL_tot %>% mutate(
  presence = NA,
)

df.sard_JUN_tot <- df.sard_JUN_tot %>% mutate(
  presence = NA,
)

df_sard <- rbind(df.sard_PEL_tot,
                 df.sard_JUN_tot,
                 obs_sard)

saveRDS(df_sard, file = paste(local_path,"/code/data_model/sard_tot",".rds", sep = ""))

# Create the space weigth and add it
# It is necessary to represent the poisson point process
space_weight <- create_space_weight(mesh_GdG, study_domain_sf)

### this step is used only if you study_domain is not a single polygon
### so some cells have been cut in several parts and we sum together this part
ID <- c()
area <- c()
for (I in 1:mesh_GdG$n){
  val <- space_weight %>%
    filter(ID == I) %>%
    colSums(.)
  
  ID <- c(ID,I)
  area <- c(area, val[2])
}
space_weight <- data.frame(ID,area)

df_weight <- add_weight(mesh_GdG, space_weight, df_sard)

df_sard <- df_sard %>%
  mutate(weight = ifelse(survey == "PECHE", 0, 1)) %>%
  st_cast()

data_model_fin <- rbind(df_sard,df_weight)

saveRDS(data_model_fin, paste(local_path,"code/data_model/sard_tot.rds", sep = ""))




obs_anch <- obs_anch %>% mutate(
  presence = indice,
  biomass = NA,
  indice = NA
)

df.anch_PEL_tot <- df.anch_PEL_tot %>% mutate(
  presence = NA,
)

df.anch_JUN_tot <- df.anch_JUN_tot %>% mutate(
  presence = NA,
)

df_anch <- rbind(df.anch_PEL_tot,
                 df.anch_JUN_tot,
                 obs_anch)

saveRDS(df_anch, file = paste(local_path,"/code/data_model/anch_tot",".rds", sep = ""))

# Create the space weigth and add it
# It is necessary to represent the poisson point process
space_weight <- create_space_weight(mesh_GdG, study_domain_sf)

### this step is used only if you study_domain is not a single polygon
### so some cells have been cut in several parts and we sum together this part
ID <- c()
area <- c()
for (I in 1:mesh_GdG$n){
  val <- space_weight %>%
    filter(ID == I) %>%
    colSums(.)
  
  ID <- c(ID,I)
  area <- c(area, val[2])
}
space_weight <- data.frame(ID,area)

df_weight <- add_weight(mesh_GdG, space_weight, df_sard)

df_anch <- df_anch %>%
  mutate(weight = ifelse(survey == "PECHE", 0, 1)) %>%
  st_cast()

data_model_fin <- rbind(df_anch,df_weight)

saveRDS(data_model_fin, paste(local_path,"code/data_model/anch_tot.rds", sep = ""))

####################
# Mesh construction
####################

#We compare a random mesh contruction with the mesh_GdG of Florian's work


coo <- df %>%
  st_drop_geometry() %>%
  dplyr::select(Xgd, Ygd)

#it's a try of my own mesh to check how it work
bnd <- study_domain_sf %>%
  st_coordinates() %>%
  inla.nonconvex.hull()

mesh <- inla.mesh.2d( 
  boundary = bnd,
  cutoff=0.5,
  max.edge = c(0.6, 0.9),
  crs = grid_projection
)

plot(mesh)

save(mesh, file = paste(local_path, "code_florian/mesh_test.RData", sep = "/"))

mesh_fin <- inla.mesh.2d( loc = coo,
                          boundary = bnd,
                          cutoff=0.15,
                          max.edge = c(0.3, 0.6),
                          crs = grid_projection
)

plot(mesh_fin)


plot(mesh_GdG)
points(coo, col = "red")

meshbuilder()

#plot(mesh_GdG)
#points(cbind(sst$Xgd,sst$Ygd), col = "red")

# We want to keep the data of the domain of study



obs <- df.sard_PEL_tot %>%
  mutate(LONG = st_coordinates(.) %>%
           as.data.frame() %>%
           pull(X),
         LAT = st_coordinates(.)%>%
           as.data.frame() %>%
           pull(Y),
         ID = 1:nrow(.),
         inside = in_out_domain(., study_domain_sf)
  )

ggplot() +
  geom_sf(data = study_domain_sf)+
  geom_sf(data = obs %>%
            mutate(inside = factor(inside, levels = c(0, 1))), 
          aes(color = inside), size = 0.5
  ) +
  scale_color_viridis_d() +
  theme_bw()

sst <- df.SST_tot %>%
  mutate(Xgd = st_coordinates(.) %>%
           as.data.frame() %>%
           pull(X),
         Ygd = st_coordinates(.)%>%
           as.data.frame() %>%
           pull(Y),
         Year = year,
         ID = 1:nrow(.),
         inside = in_out_domain(., study_domain_sf)
  ) %>%
  filter(inside == 1) %>%
  #dplyr::rename(SST = '05') %>%
  filter (month == 5)%>%
  # this names are here to facilitate the code after
  dplyr::select(Xgd, Ygd, SST, Year)



ggplot() + 
  geom_sf(data = study_domain_sf,
          aes()
  ) +
  geom_sf(data = sst, 
          aes(fill = SST, color = SST), size = 2
  ) +
  scale_fill_viridis_c() +
  scale_color_viridis_c() +
  guides("none") +
  theme_bw()



# We want associate all the data of the domain of study


#We create the dataset in the good shape of the presence/absence

if(!dir.exists("./output/fusion")) {
  dir.create("./output/fusion")
}
if(!dir.exists("./output/fusion/sard")) {
  dir.create("./output/fusion/sard")
}

path_map <- "./output/fusion/sard/"
path_function <- paste(local_path, "code/function", sep = "/")

df <- data.frame()

for (an in periode){
  Grid2 <- obs %>%
    st_drop_geometry() %>%
    dplyr::select(LONG, LAT, indice, year) %>%
    dplyr::filter(year == an) %>%
    pelaMSFD::blocking(obs_xyz = .,
                       saturation = pelaMSFD::saturation_specification(saturate = FALSE,
                                                                       quantile = 0.99,
                                                                       above = TRUE
                       ),
                       grid_param = pelaMSFD::grid_specification(x_left = -7.0 - 0.25 / 2,
                                                                 x_right = 0 + 0.25 / 2,
                                                                 y_bottom = 43.0 - 0.25 / 2,
                                                                 y_top = 48.0 + 0.25 / 2,
                                                                 x_width = 0.25,
                                                                 y_width = 0.25,
                                                                 n_iter = 5E2
                       ),
                       summing = FALSE
    )
  
  
  Grid2 <- Grid2 %>% 
    dplyr::rename(Xgd = x_center,
                  Ygd = y_center,
                  Zvalue = z_mean_value
    ) %>%
    mutate(Year = as.integer(an),
           presence = ifelse(Zvalue > 0, 1, 0)
    ) %>%
    dplyr::select(Xgd, Ygd, Year, presence) #%>%
  # mutate(presence = Zvalue)
  #the grid is used to concatenate the value of Pelgas 
  df <- rbind(df, 
              as.data.frame(sst %>%
                              filter(Year == an) %>%
                              left_join(.,
                                        Grid2,
                                        by = c("Xgd", "Ygd", "Year")
                              )))  
  
  
  
}

df <- df %>% st_as_sf() %>%
  st_set_crs(., 4326)

df %>%
  ggplot() +
  geom_sf(aes(fill = presence)) +
  theme_bw()

saveRDS(df, file = paste(local_path,"/code/data_model/sard_presence",".rds", sep = ""))



#We create the dataset in the good shape of the biomasse density

if(!dir.exists("./output/fusion")) {
  dir.create("./output/fusion")
}
if(!dir.exists("./output/fusion/sard")) {
  dir.create("./output/fusion/sard")
}

path_map <- "./output/fusion/sard/"
path_function <- paste(local_path, "code/function", sep = "/")

obs <- obs %>% dplyr:: filter(biomass > 0)

df <- data.frame()

for (an in periode){
  Grid2 <- obs %>%
    st_drop_geometry() %>%
    dplyr::select(LONG, LAT, biomass,year) %>%
    dplyr::filter(year == an) %>%
    pelaMSFD::blocking(obs_xyz = .,
                       saturation = pelaMSFD::saturation_specification(saturate = FALSE,
                                                                       quantile = 0.99,
                                                                       above = TRUE
                       ),
                       grid_param = pelaMSFD::grid_specification(x_left = -7.0 - 0.25 / 2,
                                                                 x_right = 0 + 0.25 / 2,
                                                                 y_bottom = 43.0 - 0.25 / 2,
                                                                 y_top = 48.0 + 0.25 / 2,
                                                                 x_width = 0.25,
                                                                 y_width = 0.25,
                                                                 n_iter = 5E2
                       ),
                       summing = TRUE
    )
  
  Grid2 <- Grid2 %>% 
    dplyr::rename(Xgd = x_center,
                  Ygd = y_center,
                  Zvalue = z_mean_value
    ) %>%
    mutate(Year = as.integer(an),
           biomass = Zvalue
    ) %>%
    dplyr::select(Xgd, Ygd, Year, biomass) #%>%
  # mutate(presence = Zvalue)
  #the grid is used to concatenate the value of Pelgas 
  df <- rbind(df, 
              as.data.frame(sst %>%
                              filter(Year == an) %>%
                              left_join(.,
                                        Grid2,
                                        by = c("Xgd", "Ygd", "Year")
                              )))  
}

df <- df %>% st_as_sf() %>%
  st_set_crs(., 4326)

df %>%
  ggplot() +
  geom_sf(aes(fill = biomass)) +
  theme_bw()

saveRDS(df, file = paste(local_path,"/code/data_model/sard_density",".rds", sep = ""))


#We create the dataset in the good shape of the presence-only with  VMS

path_data <- paste(local_path, "/code/data/tidy/", sep = "/")
sard_VMS_tot <- readRDS(paste(path_data,"Data_sard_VMS.RDS",sep = ""))

df.sard_tot <- sard_VMS_tot %>%
  filter(indice > 0) %>%
  st_as_sf(., coords = c("LONG","LAT")) %>%
  st_set_crs(., 4326)


obs <- df.sard_tot %>%
  mutate(LONG = st_coordinates(.) %>%
           as.data.frame() %>%
           pull(X),
         LAT = st_coordinates(.)%>%
           as.data.frame() %>%
           pull(Y),
         ID = 1:nrow(.),
         inside = in_out_domain(.,study_domain_sf)
  )

obs$month <- as.integer(obs$month)

gear <- c("PS", "PTM")

obs <- obs %>% filter(inside == 1) %>% filter(month %in% c(5,9)) %>%
  filter(type %in% gear)

saveRDS(obs, file = paste(local_path,"/code/data_model/sard_presence_VMS",".rds", sep = ""))


ggplot(study_domain_sf) + 
  geom_sf() + 
  geom_sf(data = obs, aes(fill = indice)) +
  theme_bw()




# Mesh construction

#We compare a random mesh contruction with the mesh_GdG of Florian's work


coo <- df %>%
  st_drop_geometry() %>%
  dplyr::select(Xgd, Ygd)

#it's a try of my own mesh to check how it work
bnd <- study_domain_sf %>%
  st_coordinates() %>%
  inla.nonconvex.hull()

mesh <- inla.mesh.2d(loc = coo, 
                     boundary = bnd,
                     cutoff=0.11,
                     max.edge = c(0.1, 0.45),
                     min.angle = c(21, 21),
                     crs = grid_projection
)

plot(mesh)

plot(mesh_GdG)
points(coo, col = "red")


