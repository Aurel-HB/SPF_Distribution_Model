add_weight <- function(mesh,space_weight,data_model){
  data_weight <- data.frame(mesh[["loc"]])
  data_weight$X3 <- space_weight[,2]
  
  col.null <- rep(0, mesh$n*42)
  df_weight <- data.frame(col.null,col.null,
                          rep(NA, mesh$n*42),
                          rep(NA, mesh$n*42),
                          rep("PS,PTM", mesh$n*42),
                          rep("peche", mesh$n*42),
                          rep("SPF", mesh$n*42),
                          rep(data_weight$X1, mesh$n*42),
                          rep(data_weight$X2, mesh$n*42),
                          rep(1, mesh$n*42),
                          col.null,
                          col.null,
                          rep(data_weight$X3, mesh$n*42))
  
  
  names(df_weight) <- c(names(data_model)[1:12],"weight")
  
  
  year <- c()
  season <- c()
  
  for (time in periode){
    year <- c(year, rep(as.integer(time), mesh$n*3))
    season <- c(season, 
                rep("spring",mesh$n),
                rep("summer",mesh$n),
                rep("fall",mesh$n))
  }
  
  df_weight$year <- year
  df_weight$season <- season
  
  df_weight <- df_weight %>%
    mutate(long = Xgd, lat = Ygd) %>%
    st_as_sf(.,coords = c("long","lat")) %>% 
    st_set_crs(., 4326)
  
  return(df_weight)
}