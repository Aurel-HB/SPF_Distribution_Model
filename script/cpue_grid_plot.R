
#We want to have a map with the mean on 1 month from the periode for VMS data
source(paste(path_function,"prepare_grid_vms.R",sep = ""))
#sardine
sard_VMS <- sard_VMS_tot 
#the grid plot work for integer(year)
#so when we try to plot it work but he put the 10-11-12 month just after the 1
sard_VMS <- sard_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
#we put a zero before the first month so recognise the order

Grid1 <- mosaic_gridDataframe(sard_VMS_tot,"month", 
                              paste(path.output,"CPUE_exploration/sard",sep = ""),"CPUE",
                              "CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path.output,"CPUE_exploration/Map",sep = ""))

unlink(paste(path.output,"CPUE_exploration/Map","meanMaps",sep = ""), 
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","stdevMaps",sep = ""),
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","NdataMaps",sep = ""),
       recursive = TRUE)


#anchovy
anch_VMS <- anch_VMS_tot
#the grid plot work for integer(year)
#so when we try to plot it work but he put the 10-11-12 month just after the 1
anch_VMS <- anch_VMS %>% mutate(month = ifelse(month < 10,month/10,month))
#we put a zero before the first month so recognise the order

Grid1 <- mosaic_gridDataframe(anch_VMS,"month", 
                              paste(path.output,"CPUE_exploration/anch",sep = ""),"CPUE",
                              "CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path.output,"CPUE_exploration/Map",sep = ""))

unlink(paste(path.output,"CPUE_exploration/Map","meanMaps",sep = ""), 
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","stdevMaps",sep = ""),
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","NdataMaps",sep = ""),
       recursive = TRUE)


#####
#we want now a mosaic with a mean per year

source(paste(path_function,"prepare_grid_vms.R",sep = ""))
#sardine
Grid1 <- mosaic_gridDataframe(sard_VMS_tot,"year", 
                              paste(path.output,"CPUE_exploration/sard",sep = ""),"CPUE",
                              "CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path.output,"CPUE_exploration/Map",sep = ""))

unlink(paste(path.output,"CPUE_exploration/Map","meanMaps",sep = ""), 
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","stdevMaps",sep = ""),
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","NdataMaps",sep = ""),
       recursive = TRUE)


#anchovy
Grid1 <- mosaic_gridDataframe(anch_VMS_tot,"year", 
                              paste(path.output,"CPUE_exploration/anch",sep = ""),"CPUE",
                              "CPUE")
mat <- mosaic_matrice(Grid1)
plot1 <- mosaic_plot(Grid1,mat,
                     paste(path.output,"CPUE_exploration/Map",sep = ""))

unlink(paste(path.output,"CPUE_exploration/Map","meanMaps",sep = ""), 
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","stdevMaps",sep = ""),
       recursive = TRUE)
unlink(paste(path.output,"CPUE_exploration/Map","NdataMaps",sep = ""),
       recursive = TRUE)








#an option is to check the correlation per gear by a filter before the grid

#sard_VMS_tot <- sard_VMS_tot %>% filter(type == "PS")







