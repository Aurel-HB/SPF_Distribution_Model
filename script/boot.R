#prepare the library and the path

#library of packages
#library(vmstools)
library(dplyr)
library(ggspatial)
library("rnaturalearth")
library("rnaturalearthdata")
library(mapdata)
library(viridis)
library(devtools)
library(rspatial)#if (!require("rspatial")) remotes::install_github('rspatial/rspatial'
library(INLA)
#library(inlabru)
library("gstat")
library(rgeos)
library(maptools)
library(sp)
library(sf)
library(gtable)
library(readr)
library(grid)
library(marmap)
library(EchoR)
library(GGally)
library(Matrix)
library(reshape2)
library(spatstat)
library(spatstat.geom)
library(cowplot)
#library(inlatools)#remotes::install_github("inbo/inlatools")
#library(INLAutils) #install.packages('INLAutils')




#path will be use during code
path_data <- "C:/Users/ahebertb/Desktop/Final_work_Aurel/code/data/tidy/" #to find the data
path.output <- "C:/Users/ahebertb/Desktop/Final_work_Aurel/code/output/"
path_function <- "C:/Users/ahebertb/Desktop/Final_work_Aurel/code/function/"
path_cov <- "C:/Users/ahebertb/Desktop/Final_work_Aurel/code/data/row/CLIMAT/"
local_path <- "C:/Users/ahebertb/Desktop/Final_work_Aurel/"

#function for extract the data in the good shape
source(paste(path_function,"extract_Pelgas_species.R",sep = ""))
source(paste(path_function,"extract_Evhoe_species.R",sep = ""))
source(paste(path_function,"extract_Juvena_species.R",sep = ""))
source(paste(path_function,"extract_VMS_species.R",sep = ""))
source(paste(path_function,"extract_cov.R",sep = ""))
source(paste(path_function,"show_presence.R",sep = ""))
source(paste(path_function,"show_quantity.R",sep = ""))

#functions use for grid and map
source(paste(path_function,"mosaic_grid.R",sep = ""))
source(paste(path_function,"mosaic_matrice.R",sep = ""))
source(paste(path_function,"mosaic_plot.R",sep = ""))
source(paste(path_function,"in_out_domain.R",sep = ""))

#functions spde 
source(paste(path_function,"spde-book-functions.R",sep = ""))

inv.logit <- function(x){
  x<- exp(x)/(1+exp(x))
  return(x)
}

periode <- as.character(seq(2009,2022,1))
world <- ne_download(scale = 10, type = 'countries', returnclass = "sf")
load(paste(local_path, "code/study_area/study_domain_sp.RData", sep = "/"))
study_domain_sf <- study_domain_sp %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326) #%>%
  #st_transform(.,2154)
load(paste(local_path, "code/study_area/mesh_GdG.Rdata", sep = ""))
load(paste(local_path, "code/study_area/mesh_test.Rdata", sep = ""))
grid_projection <- "+proj=longlat +datum=WGS84" 
#grid_projection is a valid CRS specification in the PROJ.4 format




