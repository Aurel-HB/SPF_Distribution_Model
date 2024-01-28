#exploration CPUE

#CPUE et gears####

######sardine####
moy <- c()
med <- c()#we use median because of the dispersion of the cpue values
for (gear in levels(sard_VMS_tot$type)){
  sard_VMS <- sard_VMS_tot %>% filter(type == gear)
  G <- mean(sard_VMS$CPUE)#mean
  quant <- quantile(sard_VMS$CPUE)#quantile and we keep the 50% one
  moy <- c(moy,G)
  med <- c(med,quant[3])
}

data <- data.frame(c(levels(sard_VMS_tot$type)),moy,med)
names(data) <- c("Gear","cpue_moy","cpue_med")

# add a column to store the colors
data$fill_color <- ifelse(data$cpue_med > 0.020, "skyblue2", "gray20")

data <- data %>% filter(!is.na(cpue_med))
# plot the bar chart with the fill color based on the new column
ggplot(data, aes(x=Gear, y=cpue_med, fill=fill_color)) + 
  geom_bar(stat = "identity") +  
  scale_fill_manual(values=c("gray20", "skyblue2"),
                    name = "CPUE by selectivity",
                    labels = c("Superior at 0.02","Inferior at 0.02")) +
  labs(title = "CPUE median per gear", x = "Gear", y = "CPUE")
ggsave(paste(path.output,"CPUE_exploration/","CPUE_median_sard.png", sep = ""), 
       width = 9.225, height = 6.525)



#we want to show the probabilist density of cpue per gear
for (gear in levels(sard_VMS_tot$type)[2:16]){
  sard_VMS <- sard_VMS_tot %>% filter(type == gear)
  prob.dens <- density(sard_VMS$CPUE)
  quant <- quantile(sard_VMS$CPUE)
  if (quant[4] > 1.5){
    windows()
    plot(prob.dens, xlim = c(0,quant[4]*10),main = gear)
    abline(v = 20, col = "red4")
    #dev.print(device = png, file = paste(path.output,"CPUE_exploration/",
    #gear,".png",sep = ""), width = 600)
  }
}


######anchovy####
moy <- c()
med <- c()#we use median because of the dispersion of the cpue values
for (gear in levels(anch_VMS_tot$type)){
  anch_VMS <- anch_VMS_tot %>% filter(type == gear)
  G <- mean(anch_VMS$CPUE)#mean
  quant <- quantile(anch_VMS$CPUE)#quantile and we keep the 50% one
  moy <- c(moy,G)
  med <- c(med,quant[3])
}

data <- data.frame(c(levels(anch_VMS_tot$type)),moy,med)
names(data) <- c("Gear","cpue_moy","cpue_med")

# add a column to store the colors
data$fill_color <- ifelse(data$cpue_med > 0.02, "skyblue2", "gray20")

data <- data %>% filter(!is.na(cpue_med))
# plot the bar chart with the fill color based on the new column
ggplot(data, aes(x=Gear, y=cpue_med, fill=fill_color)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("gray20", "skyblue2"), 
                    name = "CPUE by selectivity",
                    labels = c("Superior at 0.02","Inferior at 0.02")) +
  labs(title = "CPUE median per gear", x = "Gear", y = "CPUE")
ggsave(paste(path.output,"CPUE_exploration/","CPUE_median_anch.png", sep = ""), width = 9.225, height = 6.525)



#####################################
#we want to have gridmap per maingear
#####################################

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

