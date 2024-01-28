#' Title : Extraction of the VMS data 
#'this function extract the data from VMS of a species in a good dataframe shape
#'the shape of database use is LONG(num),LAT(num),year(int),month(factor),indice(num),quantity(num){t},type(chr),survey(chr),sp(chr),CPUE(num){kg}
#' @param species is the species of interest at the format Code Medits (character)
#'anchovy=ENGR-ENC
#'sardine=SARD-PIL
#'horse mackerel=TRAC-TRU
#' 
#' @return a dataframe with the interest information
#'
#'
#'
extract_VMS_species <- function(species, local_path){
  library(dplyr)
  
  path <- paste(local_path, "code/data/row/VMS/", sep = "") #localisation of the data
  
  #import data of the good species    
  loading1 <- paste(path,'2023.03.23_DELMOGES_SACROIS_VMS_Effort-Captures_33.csv',sep='')
  VMS_1 <- read.csv2(loading1)
  loading2 <- paste(path,"2023.03.23_DELMOGES_Carres_33.csv",sep='')
  VMS_2 <- read.csv2(loading2)
   
  if (species == "SARD-PIL"){
    VMS_1 <- VMS_1 %>% filter(VMS_1$TAXON_GROUP_LABEL == "PIL")
  }
  if (species == "ENGR-ENC"){
    VMS_1 <- VMS_1 %>% filter(VMS_1$TAXON_GROUP_LABEL == "ANE")
  }
  
  data <- merge(VMS_1,VMS_2,by = "SQUARE")
  data$FISHING_TIME_HOUR <- as.numeric(data$FISHING_TIME_HOUR)
  data$GEAR_LABEL <- as.factor(data$GEAR_LABEL)
  data$QUANTITY_KG <- as.numeric(data$QUANTITY_KG)
  data <- data %>% mutate(indice = ifelse(QUANTITY_KG>0,1,0))#vector indicator of presence/absence
  
  #extraction of the coordinates # for (square in 1:length(VMS_2$SQUARE)){VMS_2$SQUARE_WKT[square] <- readWKT(VMS_2$SQUARE_WKT[square])}
  data <- data %>% mutate(LONG = as.numeric(substr(data$SQUARE_WKT,start = 11, stop = 15)))
  data <- data %>% mutate(LAT = substr(data$SQUARE_WKT,start = 16, stop = 21))
  data <- data %>% mutate(LAT = sub(",","",LAT))
  data <- data %>% mutate(LAT = as.numeric(LAT))
  data <- data %>% filter(!is.na(LONG))# be careful the manip before delete some information
  data <- data %>% filter(!is.na(LAT))
  
  #creation of the CPUE
  data <- data %>% mutate(CPUE = (QUANTITY_KG/1000) / FISHING_TIME_HOUR)
  
  #filter the period
  data <- data %>% filter(YEAR %in% periode)
  
  
  #create the proper table
  LONG <- data$LONG#vector long
  LAT <- data$LAT#vector lat
  
  year <- data$YEAR#vector year
  
  sp <-  rep(species, length(data$CPUE)) #vector species 
  
  indice <- data$indice
  
  survey <- rep("PECHE",length(data$CPUE) )#vector ref data
  
  type <- data$GEAR_LABEL #vector of gears use to collect data
  
  month <- formatC(data$MONTH, width = 2, format = "d", flag = "0")
  month <- as.integer(month)
  
  quantity <- data$QUANTITY_KG/1000
  
  CPUE <- data$CPUE
  
  data <- data.frame(LONG,LAT,year,month,indice,quantity,type,survey,sp,CPUE)
  
  return(data)
}
#LE_KG_PIL=sardine
#LE_KG_ANE=anchois
#LE_KG_HOM=chinchard





#old version
# Create a vector of first days of each month with a random year
#first_days <- as.Date(paste0("01-", data$MONTH, "-2023"), format = "%d-%m-%Y")
# Convert the vector of first days to POSIXct format
#posix_dates <- as.POSIXct(first_days, tz = "UTC")
#the false day and year have to be delete
#month <- as.factor(months(posix_dates,abbreviate = TRUE))# we use the POSIXct format to extract month