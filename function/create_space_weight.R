create_space_weight <- function(mesh, study_domain_sf){
  
  ##make boundary object
  bnd <- study_domain_sf %>%
    st_coordinates() %>%
    inla.nonconvex.hull()
  
  ### turn into a window object (in spatstat.geom)
  bob_owin <- study_domain_sf %>%
    st_transform(crs = 2154) %>%
    as.owin()
  
  ###mesh
  nv <- mesh$n
  dmesh <- book.mesh.dual(mesh)
  
  ### check and add unique ID
  mesh_sf <- dmesh %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    mutate(ID = 1:nv) %>%
    st_cast()
  
  mesh_sf %>%
    ggplot() +
    geom_sf() +
    theme_bw()
  
  ### compute the intersection between each polygon in the dual mesh and the study area
  ## these are the integration weights for the PPP
  w <- st_intersection(mesh_sf %>%
                         st_transform(crs = 2154),
                       study_domain_sf %>%
                         st_transform(crs = 2154)
                       ) %>%
    mutate(area = st_area(.)) %>%
    st_drop_geometry() %>%
    mutate(area = area / sum(area)) %>%
    mutate(area = units::drop_units(area))
    
  ###select(ID, area)
  w <- w[,c(1,9)]
  
  mesh_sf <- mesh_sf %>%
    left_join(., w) %>%
    mutate(w = ifelse(is.na(area), 0, area)) %>%
    st_cast()
  
  ### check
  mesh_sf %>% 
    pull(w) %>%
    sum()
  
  ### Augment the vector of ones for the observations (representing the points) 
  ## with a sequence of zeros (representing the mesh nodes)
  
  return(w)
  
}