
#####################################
#we want to have gridmap per maingear

path_map <- paste(path.output,"CPUE_exploration/sard/gear/",sep = "")

for (gear in c("LHP","PS","PTM")){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  sard_VMS <- sard_VMS_tot %>% filter(type == gear)
  
  Grid1 <- mosaic_gridDataframe(sard_VMS,"year", 
                                paste(path_map,gear,sep = ""),"CPUE",
                                "CPUE")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,gear,sep = ""))
}

for (gear in c("LHP","PS","PTM")){
  unlink(paste(path_map,gear,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"NdataMaps",sep = ""), recursive = TRUE)
}


path_map <- paste(path.output,"CPUE_exploration/anch/gear/",sep = "")

for (gear in c("OTM","PS","PTM")){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  anch_VMS <- anch_VMS_tot %>% filter(type == gear)
  
  Grid1 <- mosaic_gridDataframe(anch_VMS,"year", 
                                paste(path_map,gear,sep = ""),"CPUE",
                                "CPUE")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,gear,sep = ""))
}

for (gear in c("OTM","PS","PTM")){
  unlink(paste(path_map,gear,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"NdataMaps",sep = ""), recursive = TRUE)
}


#we do the same work but per month
path_map <- paste(path.output,"CPUE_exploration/sard/gear/",sep = "")

for (gear in c("LHP","PS","PTM")){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  sard_VMS <- sard_VMS_tot %>% filter(type == gear)
  #the grid plot work for integer(year)
  #so when we try to plot it work but he put the 10-11-12 month just after the 1
  sard_VMS <- sard_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
  #we put a zero before the first month so recognise the order
  
  Grid1 <- mosaic_gridDataframe(sard_VMS,"month", 
                                paste(path_map,gear,sep = ""),"CPUE",
                                "CPUE")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,gear,sep = ""))
}

for (gear in c("LHP","PS","PTM")){
  unlink(paste(path_map,gear,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"NdataMaps",sep = ""), recursive = TRUE)
}


path_map <- paste(path.output,"CPUE_exploration/anch/gear/",sep = "")

for (gear in c("OTM","PS","PTM")){
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  anch_VMS <- anch_VMS_tot %>% filter(type == gear)
  #the grid plot work for integer(year)
  #so when we try to plot it work but he put the 10-11-12 month just after the 1
  anch_VMS <- anch_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
  
  Grid1 <- mosaic_gridDataframe(anch_VMS,"month", 
                                paste(path_map,gear,sep = ""),"CPUE",
                                "CPUE")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       paste(path_map,gear,sep = ""))
}

for (gear in c("OTM","PS","PTM")){
  unlink(paste(path_map,gear,"meanMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"stdevMaps",sep = ""), recursive = TRUE)
  unlink(paste(path_map,gear,"NdataMaps",sep = ""), recursive = TRUE)
}







