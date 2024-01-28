#' Title
#'this function extract the data from EVHOE of a species in a good dataframe shape
#'the shape of database use is LONG(num),LAT(num),year(int),month(factor),indice(num),biomass(num){t},type(chr),survey(chr),sp(chr)
#' @param species is the species of interest at the format Code Medits
#'anchovy=ENGR-ENC
#'sardine=SARD-PIL
#'horse mackerel=TRAC-TRU
#'
#' @return a dataframe with the interest information
#' 
#'
#' 
extract_Evhoe_species <- function(species, local_path){
  library(dplyr)
  #create a column of zero for each station in the database of localisation info
  data1 <- read.csv2(paste(local_path,"code/data/row/EVHOE/Traits.csv"), sep = "")
  data1 <- data1 %>% mutate (indice = 0 )
  data1 <- data1 %>% mutate (biomass = 0 )
  
  #import the database with fish info 
  data2 <- read.csv2(paste(local_path,"code/data/row/EVHOE/Captures.csv"), sep = "")
  
  #filter the specie of interest
  if (species == "SARD-PIL"){
    data2 <- data2 %>% filter(Espece == "Sardina pilchardus")
  }
  if (species == "ENGR-ENC"){
    data2 <- data2 %>% filter(Espece == "Engraulis encrasicolus")
  }
  if (species == "TRAC-TRU"){
    data2 <- data2 %>% filter(Espece == "Trachurus trachurus")
  }
  
  #create a column with all the information absence/presence
  data <- data1
  data2$Trait <- as.factor(data2$Trait)
  
  for (station in data2$Trait){
    data$indice[data$Trait == station] <- data2$Nombre[data2$Trait == station]
    data$biomass[data$Trait == station] <- data2$Poids[data2$Trait == station]
  }
  
  
  #filter the good year
  data <- data %>% filter(Annee %in% periode)
  
  #create the proper table vector by vector
  LONG <- as.numeric(data$Long)#vector long
  LAT <- as.numeric(data$Lat)#vector lat
  
  year <- data$Annee#vector year
  
  sp <-  rep(species, length(data$Trait)) #vector species
  
  data <- data %>% mutate(indice = ifelse(indice>0,1,0))# we change all the number in a 1 of presence
  indice <- data$indice
  
  survey <- data$Campagne#vector ref data
  
  type <- rep("Chalut",length(data$Trait) )#vector of gear use to collect data
  
  #We assume that the Evhoe survey in mostly on the month of november
  month <- as.factor(rep("11",length(data$Trait)))
  
  biomass <- as.numeric(data$biomass)/1000
  
  data <- data.frame(LONG,LAT,year,month,indice,biomass,type,survey,sp)
  
  return(data)
}