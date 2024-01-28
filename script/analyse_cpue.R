#first we create a big data frame with all the fisheries data ----

path <- "C:/Users/ahebertb/Desktop/StageAurel/code/data/row/VMS/" #localisation of the data

#import data of the good species    
loading1 <- paste(path,'2023.03.23_DELMOGES_SACROIS_VMS_Effort-Captures_33.csv',sep='')
VMS_1 <- read.csv2(loading1)
loading2 <- paste(path,"2023.03.23_DELMOGES_Carres_33.csv",sep='')
VMS_2 <- read.csv2(loading2)

data <- merge(VMS_1,VMS_2,by = "SQUARE")
data$FISHING_TIME_HOUR <- as.numeric(data$FISHING_TIME_HOUR)
data$GEAR_LABEL <- as.factor(data$GEAR_LABEL)
data$QUANTITY_KG <- as.numeric(data$QUANTITY_KG)
data <- data %>% mutate(indice = ifelse(QUANTITY_KG>0,1,0))#vector indicator of presence/absence

#extraction of the coordinates 
 #for (square in 1:length(VMS_2$SQUARE)){VMS_2$SQUARE_WKT[square] <- readWKT(VMS_2$SQUARE_WKT[square])}
#the method upper doesn't work


data <- data %>% mutate(LONG = as.numeric(substr(data$SQUARE_WKT,start = 11, stop = 15)))
data <- data %>% mutate(LAT = substr(data$SQUARE_WKT,start = 16, stop = 21))
data <- data %>% mutate(LAT = sub(",","",LAT))
data <- data %>% mutate(LAT = as.numeric(LAT))
data <- data %>% filter(!is.na(LONG))# be careful the manip before delete some information
data <- data %>% filter(!is.na(LAT))

#filter the period
data <- data %>% filter(YEAR %in% periode)

#work on the data ----

#creation of the CPUE
data <- data %>% mutate(CPUE = QUANTITY_KG / FISHING_TIME_HOUR)

#we prepare the good format of frame for the function below
data <- data %>% mutate(sp = TAXON_GROUP_LABEL)
data <- data %>% mutate(survey = "peche")
data <- data %>% mutate(quantity = QUANTITY_KG)

#we keep only the gear of interest

gear_lab <- as.factor(c("OTM","PS","PTM"))

data <- data %>% filter(GEAR_LABEL %in% gear_lab)

#the grid plot work for integer(year)
#so when we try to plot it work but he put the 10-11-12 month just after the 1
data <- data %>% mutate(MONTH = ifelse(MONTH < 10,MONTH/10,MONTH))
#we put a zero before the first month so recognise the order


###########################################################################
#work per species ----
species <- "PIL"
spedata <- data %>% filter(sp == species)
espece <- "sardine"
carte <- c()



# occurence and quantity map per gear per year 
for (gear in gear_lab ){
  if(!dir.exists(paste(path.output,"map/sard/",gear, sep = ""))) {
    dir.create(paste(path.output,"map/sard/",gear, sep = ""))
  }
  gear_data <-  spedata %>% filter(GEAR_LABEL == gear)
  for (an in periode){
    spedata_year <- gear_data %>% filter(YEAR == as.numeric(an))
    map <- show_presence(spedata_year,world,an,espece)
    #windows()
    #show(map)
    ggsave(filename = paste("sard_",an,".png", sep = ""), plot =  map,
           path = paste(path.output,"map/sard/",gear, sep = ""),
           width = 6.29, height = 6.29)
    
    map <- show_quantity(spedata_year,world,an,espece)
    #windows()
    #show(map)
    ggsave(filename = paste("Quant_sard_",an,".png", sep = ""), plot =  map,
           path = paste(path.output,"map/sard/",gear, sep = ""),
           width = 6.29, height = 6.29)
    carte <- c(carte,map)
    
  }
}



#fishing time
for (tool in gear_lab){
  path_map <- paste(path.output,"CPUE_analysis/sard/",tool,"/",sep = "")
  
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  subdata <- spedata %>% filter(GEAR_LABEL == tool)
  
  Grid1 <- mosaic_gridDataframe(subdata,"YEAR", 
                               path_map,"FISHING_TIME_HOUR",
                                "FISHING_TIME_HOUR")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
  
  Grid1 <- mosaic_gridDataframe(subdata,"MONTH", 
                                path_map,"FISHING_TIME_HOUR",
                                "FISHING_TIME_HOUR")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
}

#quantity
for (tool in gear_lab){
  path_map <- paste(path.output,"CPUE_analysis/sard/",tool,"/",sep = "")
  
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  subdata <- spedata %>% filter(GEAR_LABEL == tool)
  
  Grid1 <- mosaic_gridDataframe(subdata,"YEAR", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
  
  Grid1 <- mosaic_gridDataframe(subdata,"MONTH", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
}



#quantity with intersection study_domain

local_path <- "C:/Users/ahebertb/Desktop/StageAurel"
load(paste(local_path, "code_florian/study_domain_sp.RData", sep = "/"))
study_domain_sf <- study_domain_sp %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326)

spedata <- spedata %>% mutate(Xgd = LONG, Ygd = LAT)%>% 
  st_as_sf(., coords = c("Xgd","Ygd")) %>% 
  st_set_crs(., 4326)

spedata <- spedata %>% mutate(inside = in_out_domain(.,study_domain_sf)) %>%
  filter(inside == 1)


for (tool in gear_lab){
  path_map <- paste(path.output,"CPUE_analysis/sard/",tool,"/",sep = "")
  
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  subdata <- spedata %>% filter(GEAR_LABEL == tool) %>%
    as.data.frame()
  
  Grid1 <- mosaic_gridDataframe(subdata,"YEAR", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
  
  Grid1 <- mosaic_gridDataframe(subdata,"MONTH", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
}



#############################################################################################
#work per species ----
species <- "ANE"
spedata <- data %>% filter(sp == species)
espece <- "anchois"
carte <- c()

# occurence and quantity map per gear per year 
for (gear in gear_lab ){
  if(!dir.exists(paste(path.output,"map/anch/",gear, sep = ""))) {
    dir.create(paste(path.output,"map/anch/",gear, sep = ""))
  }
  gear_data <-  spedata %>% filter(GEAR_LABEL == gear)
  for (an in periode){
    spedata_year <- gear_data %>% filter(YEAR == as.numeric(an))
    map <- show_presence(spedata_year,world,an,espece)
    #windows()
    #show(map)
    ggsave(filename = paste("anch_",an,".png", sep = ""), plot =  map,
           path = paste(path.output,"map/anch/",gear, sep = ""),
           width = 6.29, height = 6.29)
    
    map <- show_quantity(spedata_year,world,an,espece)
    #windows()
    #show(map)
    ggsave(filename = paste("Quant_anch_",an,".png", sep = ""), plot =  map,
           path = paste(path.output,"map/anch/",gear, sep = ""),
           width = 6.29, height = 6.29)
    carte <- c(carte,map)
    
  }
}



#fishing time
for (tool in gear_lab){
  path_map <- paste(path.output,"CPUE_analysis/anch/",tool,"/",sep = "")
  
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  subdata <- spedata %>% filter(GEAR_LABEL == tool)
  
  Grid1 <- mosaic_gridDataframe(subdata,"YEAR", 
                                path_map,"FISHING_TIME_HOUR",
                                "FISHING_TIME_HOUR")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
  
  Grid1 <- mosaic_gridDataframe(subdata,"MONTH", 
                                path_map,"FISHING_TIME_HOUR",
                                "FISHING_TIME_HOUR")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
}

#quantity
for (tool in gear_lab){
  path_map <- paste(path.output,"CPUE_analysis/anch/",tool,"/",sep = "")
  
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  subdata <- spedata %>% filter(GEAR_LABEL == tool)
  
  Grid1 <- mosaic_gridDataframe(subdata,"YEAR", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
  
  Grid1 <- mosaic_gridDataframe(subdata,"MONTH", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
}


#quantity with intersection study_domain

local_path <- "C:/Users/ahebertb/Desktop/StageAurel"
load(paste(local_path, "code_florian/study_domain_sp.RData", sep = "/"))
study_domain_sf <- study_domain_sp %>% 
  st_as_sf() %>% 
  st_set_crs(., 4326)

spedata <- spedata %>% mutate(Xgd = LONG, Ygd = LAT)%>% 
  st_as_sf(., coords = c("Xgd","Ygd")) %>% 
  st_set_crs(., 4326)

spedata <- spedata %>% mutate(inside = in_out_domain(.,study_domain_sf)) %>%
  filter(inside == 1)


for (tool in gear_lab){
  path_map <- paste(path.output,"CPUE_analysis/anch/",tool,"/",sep = "")
  
  source(paste(path_function,"prepare_grid_vms.R",sep = ""))#run parameters to have a good grid plot
  subdata <- spedata %>% filter(GEAR_LABEL == tool) %>%
    as.data.frame()
  
  Grid1 <- mosaic_gridDataframe(subdata,"YEAR", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
  
  Grid1 <- mosaic_gridDataframe(subdata,"MONTH", 
                                path_map,"quantity",
                                "quantity")
  mat <- mosaic_matrice(Grid1)
  plot1 <- mosaic_plot(Grid1,mat,
                       path_map) 
}


