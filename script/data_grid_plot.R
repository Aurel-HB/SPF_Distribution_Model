#path.grids <- "C:/Users/ahebertb/Desktop/StageAurel/code/output/Grid/"
#path_function <- "C:/Users/ahebertb/Desktop/StageAurel/code/function/"

#this code create a ggplot and gridmap of the data

#for mosaic grid map ----
##### presence/absence
type_carte <- "Presence"
type_donnee <- "indice"
#Sardine----
path_map <- paste(path.output,"Grid/sard/presence/",sep = "")
#pelgas data mapgrid
source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))
#run parameters to have a good grid plot
Grid1 <- mosaic_gridDataframe(sard_PEL_tot,"year",
                              paste(path_map,"PELGAS",sep = ""),
                              type_carte,type_donnee)
#create a frame with the information about the grid
mat <- mosaic_matrice(Grid1)
#prepare the data of Nsample, stdev and value to plot the grid on map
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"PELGAS",sep = ""))
#save the mosaic of map in the folder indicated

#evhoe data mapgrid
source(paste(path_function,"prepare_grid_evhoe.R",sep = ""))#run parameters to have a good grid plot
Grid1 <- mosaic_gridDataframe(sard_EVH_tot,"year", 
                              paste(path_map,"EVHOE",sep = ""),
                              type_carte,type_donnee)

Grid1 <- Grid1 %>% filter(Ygd < 48)
Grid1 <- Grid1 %>% filter(Ysample < 48)
# we want only the data in the Bay of Biscay

mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"EVHOE",sep = ""))

#juvena data mapgrid
source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))#run parameters to have a good grid plot
Grid1 <- mosaic_gridDataframe(sard_JUN_tot,"year", 
                              paste(path_map,"JUVENA",sep = ""),
                              type_carte,type_donnee)
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"JUVENA",sep = ""))

#VMS data mapgrid
for (an in periode){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  sard_VMS <- readRDS(paste(path_data,"sard_",an,"_VMS",".rds",sep = ""))
  sard_VMS$month <- as.integer(sard_VMS$month) 
  
  #the grid plot work for integer(year)
  #so when we try to plot it work but he put the 10-11-12 month just after the 1
  sard_VMS <- sard_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
  #we put a zero before the first month so recognise the order
  
  Grid1 <- mosaic_gridDataframe(sard_VMS,"month", 
                                paste(path_map,"VMS/",an,sep = ""),type_carte,
                                type_donnee)
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,"VMS/",an,sep = ""))
}

#delete the parasit folder
for (survey in c("EVHOE","PELGAS","JUVENA")){
  unlink(paste(path_map,survey,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"NdataMaps",sep = ""), recursive = TRUE)
}

for (an in periode){
  unlink(paste(path_map,"VMS/",an,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"NdataMaps",sep = ""), recursive = TRUE)
}


#Anchovy ----

path_map <- paste(path.output,"Grid/anch/presence/",sep = "")
#pelgas data mapgrid
source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))
#run parameters to have a good grid plot
Grid1 <- mosaic_gridDataframe(anch_PEL_tot,"year",
                              paste(path_map,"PELGAS",sep = ""),type_carte,type_donnee)
#create a frame with the information about the grid
mat <- mosaic_matrice(Grid1)
#prepare the data of Nsample, stdev and value to plot the grid on map
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"PELGAS",sep = ""))
#save the mosaic of map in the folder indicated

#evhoe data mapgrid
source(paste(path_function,"prepare_grid_evhoe.R",sep = ""))#run parameters to have a good grid plot
Grid1 <- mosaic_gridDataframe(anch_EVH_tot,"year", 
                              paste(path_map,"EVHOE",sep = ""),
                              type_carte,type_donnee)

Grid1 <- Grid1 %>% filter(Ygd < 48)
Grid1 <- Grid1 %>% filter(Ysample < 48)
# we want only the data in the Bay of Biscay

mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"EVHOE",sep = ""))

#juvena data mapgrid
#source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))#run parameters to have a good grid plot
#Grid1 <- mosaic_gridDataframe(anch_JUN_tot,"year", 
#                              paste(path_map,"JUVENA",sep = ""),
#                              type_carte,type_donnee)
#mat <- mosaic_matrice(Grid1)
#plot1 <- mosaic_plot(Grid1,mat,
#                     paste(path_map,"JUVENA",sep = ""))

#VMS data mapgrid
for (an in periode){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  anch_VMS <- readRDS(paste(path_data,"anch_",an,"_VMS",".rds",sep = ""))
  anch_VMS$month <- as.integer(anch_VMS$month) 
  
  #the grid plot work for integer(year)
  #so when we try to plot it work but he put the 10-11-12 month just after the 1
  anch_VMS <- anch_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
  #we put a zero before the first month so recognise the order
  
  Grid1 <- mosaic_gridDataframe(anch_VMS,"month", 
                                paste(path_map,"VMS/",an,sep = ""),type_carte,
                                type_donnee)
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,"VMS/",an,sep = ""))
}

#delete the parasit folder
for (survey in c("EVHOE","PELGAS","JUVENA")){
  unlink(paste(path_map,survey,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"NdataMaps",sep = ""), recursive = TRUE)
}

for (an in periode){
  unlink(paste(path_map,"VMS/",an,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"NdataMaps",sep = ""), recursive = TRUE)
}

for(i in 1:length(dev.list())) {
  dev.off(i)
}


#for mosaic grid map ----
##### biomasse et cpue
type_carte <- "Biomasse"
type_donnee <- "biomass"
#Sardine----
path_map <- paste(path.output,"Grid/sard/biomasse/",sep = "")
#pelgas data mapgrid
source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))
#run parameters to have a good grid plot

sard_PEL <- sard_PEL_tot %>% mutate(biomass = ifelse(biomass>1,log(biomass),biomass))
#we use the log  to reduce the higher values 
#we need keep value inferior to 1 because we get minus values which are not show on map
Grid1 <- mosaic_gridDataframe(sard_PEL,"year",
                              paste(path_map,"PELGAS",sep = ""),
                              type_carte,type_donnee)
#create a frame with the information about the grid
mat <- mosaic_matrice(Grid1)
#prepare the data of Nsample, stdev and value to plot the grid on map
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"PELGAS",sep = ""))
#save the mosaic of map in the folder indicated

#evhoe data mapgrid
source(paste(path_function,"prepare_grid_evhoe.R",sep = ""))#run parameters to have a good grid plot

sard_EVH <- sard_EVH_tot %>% mutate(biomass = ifelse(biomass>1,log(biomass),biomass))

Grid1 <- mosaic_gridDataframe(sard_EVH,"year", 
                              paste(path_map,"EVHOE",sep = ""),
                              type_carte,type_donnee)

Grid1 <- Grid1 %>% filter(Ygd < 48)
Grid1 <- Grid1 %>% filter(Ysample < 48)
# we want only the data in the Bay of Biscay

mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"EVHOE",sep = ""))

#juvena data mapgrid
source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))#run parameters to have a good grid plot

sard_JUN <- sard_JUN_tot %>% mutate(biomass = ifelse(biomass>1,log(biomass),biomass))

Grid1 <- mosaic_gridDataframe(sard_JUN,"year", 
                              paste(path_map,"JUVENA",sep = ""),
                              type_carte,type_donnee)
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"JUVENA",sep = ""))

#VMS data mapgrid
for (an in periode){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  sard_VMS <- readRDS(paste(path_data,"sard_",an,"_VMS",".rds",sep = ""))
  sard_VMS$month <- as.integer(sard_VMS$month) 
  
  #the grid plot work for integer(year)
  #so when we try to plot it work but he put the 10-11-12 month just after the 1
  sard_VMS <- sard_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
  #we put a zero before the first month so recognise the order
  
  Grid1 <- mosaic_gridDataframe(sard_VMS,"month", 
                                paste(path_map,"VMS/",an,sep = ""),"CPUE",
                                "CPUE")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,"VMS/",an,sep = ""))
}

#VMS data mapgrid for may and september for each year
sard_VMS_mai <- sard_VMS_tot %>% filter(month == 5)
sard_VMS_sept <- sard_VMS_tot %>% filter(month == 9)

source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
Grid1 <- mosaic_gridDataframe(sard_VMS_mai,"year", 
                              paste(path_map,"VMS/mai",sep = ""),
                              "CPUE","CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"VMS/mai",sep = ""))


Grid1 <- mosaic_gridDataframe(sard_VMS_sept,"year", 
                              paste(path_map,"VMS/sept",sep = ""),
                              "CPUE","CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"VMS/sept",sep = ""))


layout <- matrix(c(1,seq(5,12,1),c(2,3,4,13,14,15)), ncol = 5, nrow = 3, byrow = TRUE)


#delete the parasit folder
for (survey in c("EVHOE","PELGAS","JUVENA")){
  unlink(paste(path_map,survey,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"NdataMaps",sep = ""), recursive = TRUE)
}

for (an in periode){
  unlink(paste(path_map,"VMS/",an,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"NdataMaps",sep = ""), recursive = TRUE)
}

unlink(paste(path_map,"VMS/mai","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/mai","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/mai","NdataMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/sept","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/sept","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/sept","NdataMaps",sep = ""), recursive = TRUE)

#Anchovy ----

path_map <- paste(path.output,"Grid/anch/biomasse/",sep = "")
#pelgas data mapgrid
source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))
#run parameters to have a good grid plot

anch_PEL <- anch_PEL_tot %>% mutate(biomass = ifelse(biomass>1,log(biomass),biomass))

Grid1 <- mosaic_gridDataframe(anch_PEL,"year",
                              paste(path_map,"PELGAS",sep = ""),
                              type_carte,type_donnee)
#create a frame with the information about the grid
mat <- mosaic_matrice(Grid1)
#prepare the data of Nsample, stdev and value to plot the grid on map
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"PELGAS",sep = ""))
#save the mosaic of map in the folder indicated

#evhoe data mapgrid
source(paste(path_function,"prepare_grid_evhoe.R",sep = ""))
#run parameters to have a good grid plot

anch_EVH <- anch_EVH_tot %>% mutate(biomass = ifelse(biomass>1,log(biomass),biomass))

Grid1 <- mosaic_gridDataframe(anch_EVH,"year", 
                              paste(path_map,"EVHOE",sep = ""),
                              type_carte,type_donnee)

Grid1 <- Grid1 %>% filter(Ygd < 48)
Grid1 <- Grid1 %>% filter(Ysample < 48)
# we want only the data in the Bay of Biscay

mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"EVHOE",sep = ""))

#juvena data mapgrid
#source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))
#run parameters to have a good grid plot

#anch_JUN <- anch_JUN_tot %>% mutate(biomass = ifelse(biomass>1,log(biomass),biomass))

#Grid1 <- mosaic_gridDataframe(anch_JUN_tot,"year", 
#                              paste(path_map,"JUVENA",sep = ""),
#                              type_carte,type_donnee)
#mat <- mosaic_matrice(Grid1)
#plot1 <- mosaic_plot(Grid1,mat,
#                     paste(path_map,"JUVENA",sep = ""))

#VMS data mapgrid
for (an in periode){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  anch_VMS <- readRDS(paste(path_data,"anch_",an,"_VMS",".rds",sep = ""))
  anch_VMS$month <- as.integer(anch_VMS$month) 
  
  #the grid plot work for integer(year)
  #so when we try to plot it work but he put the 10-11-12 month just after the 1
  anch_VMS <- anch_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
  #we put a zero before the first month so recognise the order
  
  Grid1 <- mosaic_gridDataframe(anch_VMS,"month", 
                                paste(path_map,"VMS/",an,sep = ""),"CPUE",
                                "CPUE")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,"VMS/",an,sep = ""))
}

#VMS data mapgrid for may and september for each year
anch_VMS_mai <- anch_VMS_tot %>% filter(month == 5)
anch_VMS_sept <- anch_VMS_tot %>% filter(month == 9)

source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
Grid1 <- mosaic_gridDataframe(anch_VMS_mai,"year", 
                              paste(path_map,"VMS/mai",sep = ""),
                              "CPUE","CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"VMS/mai",sep = ""))


Grid1 <- mosaic_gridDataframe(anch_VMS_sept,"year", 
                              paste(path_map,"VMS/sept",sep = ""),
                              "CPUE","CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"VMS/sept",sep = ""))

#delete the parasit folder
for (survey in c("EVHOE","PELGAS","JUVENA")){
  unlink(paste(path_map,survey,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,survey,"NdataMaps",sep = ""), recursive = TRUE)
}

for (an in periode){
  unlink(paste(path_map,"VMS/",an,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,"VMS/",an,"NdataMaps",sep = ""), recursive = TRUE)
}

unlink(paste(path_map,"VMS/mai","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/mai","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/mai","NdataMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/sept","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/sept","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"VMS/sept","NdataMaps",sep = ""), recursive = TRUE)



## mosaic grid map of covariable ####
#
# Chla ----
path_map <- paste(path.output,"Grid/envrmnt/",sep = "")
type_carte <- "Chla"
type_donnee <- "Chla"

#VMS data mapgrid for may and september for each year
Chla_mai <- Chla_tot %>% filter(month == 5)
Chla_sept <- Chla_tot %>% filter(month == 9)

source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))#run parameters to have a good grid plot

Chla_mai$month <- as.integer(Chla_mai$month) 
Chla_sept$month <- as.integer(Chla_sept$month) 
  
Grid1 <- mosaic_gridDataframe(Chla_mai,"year", 
                              paste(path_map,"Chla/mai",sep = ""),type_carte,
                              type_donnee)
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                    paste(path_map,"Chla/mai",sep = ""))

Grid1 <- mosaic_gridDataframe(Chla_sept,"year", 
                              paste(path_map,"Chla/sept",sep = ""),type_carte,
                              type_donnee)
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"Chla/sept",sep = ""))

#delete the parasit folder
unlink(paste(path_map,"Chla/mai","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"Chla/mai","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"Chla/mai","NdataMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"Chla/sept","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"Chla/sept","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"Chla/sept","NdataMaps",sep = ""), recursive = TRUE)


# SST ----
path_map <- paste(path.output,"Grid/envrmnt/",sep = "")
type_carte <- "SST"
type_donnee <- "SST"

SST_mai <- SST_tot %>% filter(month == 5)
SST_sept <- SST_tot %>% filter(month == 9)

source(paste(path_function,"prepare_grid_pel_jun.R",sep = ""))#run parameters to have a good grid plot

SST_mai$month <- as.integer(SST_mai$month) 
SST_sept$month <- as.integer(SST_sept$month) 

Grid1 <- mosaic_gridDataframe(SST_mai,"year", 
                              paste(path_map,"SST/mai",sep = ""),type_carte,
                              type_donnee)
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"SST/mai",sep = ""))

Grid1 <- mosaic_gridDataframe(SST_sept,"year", 
                              paste(path_map,"SST/sept",sep = ""),type_carte,
                              type_donnee)
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"SST/sept",sep = ""))

#delete the parasit folder
unlink(paste(path_map,"SST/mai","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"SST/mai","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"SST/mai","NdataMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"SST/sept","meanMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"SST/sept","stdevMaps",sep = ""), recursive = TRUE)
unlink(paste(path_map,"SST/sept","NdataMaps",sep = ""), recursive = TRUE)

#Bathy ----
Grid1 <- read.table(paste(path_cov,
                          "grid_Seabed_Depth_Filternone.txt",sep = ""),
                    header = TRUE, sep = ";", dec = ".", na.strings = "NA")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path_map,"Bathy/",sep = ""))





