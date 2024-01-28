#' Title
#'this function extract the data from PELGAS of a specie in a good dataframe shape
#'the shape of database use is LONG(num),LAT(num),year(int),month(factor),indice(num),biomass(num) {t},type(chr),survey(chr),sp(chr)
#' @param species is the species of interest at the format Code Medits :
#'anchovy=ENGR-ENC
#'sardine=SARD-PIL
#'horse mackerel=TRAC-TRU
#'
#' @return a dataframe with the interest information
#' 
#'
#' 
extract_Pelgas_species <- function(species, local_path){
  #this function use the package dplyr 
  path <- paste(local_path, "code/data/row/PELGAS/Biomasse/biomres.size.PELGAS.csv", sep = "")
  biomres_size_PELGAS <-  read.csv2(path)
  data <- biomres_size_PELGAS
  
  #conversion in the good format 
  data$N <- as.numeric(data$N)
  data$W <- as.numeric(data$W)
  data$LAT <- as.numeric(data$LAT)
  data$LONG <- as.numeric(data$LONG)
  
  #filter the specie and year
  data <- data %>% filter(sp == species)
  data <- data %>% filter(year %in% as.integer(periode))
  
  #create the proper table
  LONG <- data$LONG#vector long
  LAT <- data$LAT#vector lat
  
  year <- data$year#vector year
  
  sp <- data$sp #vector species
  
  data <- data %>% mutate(indice = ifelse(N>0,1,0))
  indice <- data$indice
  
  survey <- rep("PELGAS",length(data$esdu.id) )#vector ref data
  
  type <- rep("Acoustic",length(data$esdu.id) )#vector of gears use to collect data
  
  #We assume that the Pelgas survey in mostly on the month of mai
  month <- as.factor(rep("05",length(data$esdu.id)))
  
  biomass <- data$W/1000
  
  data <- data.frame(LONG,LAT,year,month,indice,biomass,type,survey,sp)
  
  return(data)
  
}
