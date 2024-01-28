#' Title
#'this function extract the data from JUVENA a specie in a good dataframe shape
#'the shape of database use is LONG(num),LAT(num),year(int),month(chr),indice(num),biomass(num){t},type(chr),survey(chr),sp(chr)
#' @param species is the species of interest at the format Code Medits :
#'anchovy=ENGR-ENC
#'sardine=SARD-PIL
#'horse mackerel=TRAC-TRU
#'
#' @return a dataframe with the interest information
#' 
#'
#' 
extract_Juvena_species <- function(species, local_path){
  library(dplyr)
  path <- paste(local_path,"code/data/row/JUVENA/biomass_PPP.csv", sep = "")
  biomass_PPP <- read_delim(path, 
                            delim = "\t", escape_double = FALSE, 
                            trim_ws = TRUE,show_col_types = FALSE)
  data <- biomass_PPP
  
  #conversion in the good format 
  data$B <- as.numeric(data$B)
  data$Lat <- as.numeric(data$Lat)
  data$Long <- as.numeric(data$Long)
  
  
  #filter the specie of interest 
  if (species == "SARD-PIL"){
    data <- data %>% filter(sp == "PIL")
  }
  if (species == "ENGR-ENC"){
    data <- data %>% filter(sp %in% c("ANE","ANE.juv","ANE.adult"))
  }
  if (species == "TRAC-TRU"){
    data <- data %>% filter(sp == "TRA")
  }
  
  #filter the period
  data <- data %>% filter(Year %in% periode)
  
  
  #create the proper table
  LONG <- data$Long#vector long
  LAT <- data$Lat#vector lat
  
  year <- data$Year#vector year
  
  sp <-  rep(species, length(data$esdu)) #vector species 
  
  data <- data %>% mutate(indice = ifelse(B>0,1,0))
  indice <- data$indice
  
  survey <- rep("JUVENA",length(data$esdu) )#vector ref data
  
  type <- rep("Acoustic",length(data$esdu) )#vector of gears use to collect data
  
  #We assume that the Pelgas survey in mostly on the month of september
  month <- as.factor(rep("09",length(data$esdu)))
  
  data <- data %>% mutate(B = as.numeric(B)) %>% 
    mutate(B = ifelse(B>0,B,0)) #because there is negative values
  biomass <- as.numeric(data$B)/1000
  
  data <- data.frame(LONG,LAT,year,month,indice,biomass,type,survey,sp) %>%
    filter(LONG < 100)
  
  return(data)
  
}
