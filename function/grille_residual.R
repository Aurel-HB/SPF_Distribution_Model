#' Title
#'
#' @param study_domain_sf 
#' @param residual 
#'
#' @return (grille_sum,grille_mean)
#' @export
#'
#' @examples
grille_residual <- function(study_domain_sf, residual){
  # make a fishnet grid over the countries
  grd <- st_make_grid(study_domain_sf, n = 5)
  inside <- in_out_domain(grd,study_domain_sf)
  grd <- data.frame(grd,inside) %>% filter(inside == 1)

  
  grd_gdg <- grd[,1] %>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  
  grille_sum <- c()
  grille_mean <- c()
  for (i in 1:length(grd_gdg$x)){
    somme <- residual %>% 
      mutate(inside = in_out_domain(residual,grd_gdg[i,1])) %>%
      filter(inside == 1) %>% 
      filter(!is.na(Value)) 
    grille_sum <- c(grille_sum,sum(c(somme$Value)))
    grille_mean <- c(grille_mean,mean(c(somme$Value)))
  }
  
  grille_sum <- data.frame(grd_gdg$x,grille_sum)%>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  names(grille_sum) <- c("Sum","geometry")
  
  grille_mean <- data.frame(grd_gdg$x,grille_mean)%>% 
    st_as_sf() %>% 
    st_set_crs(., 4326)
  names(grille_mean) <- c("Mean","geometry")
  
  result <- data_frame(grille_sum$Sum,grille_mean)
  names(result) <- c("Sum","Mean","geometry")
  
  result <- result %>%
  st_as_sf(.) %>%
    st_set_crs(., 4326)
  
  return(result)
}