
#' Création of a gridDataframe and gridMap in a way of doing a mosaic
#'
#' @param data a dataframe with the data already in a good format
#' @param time is a chr it can be month or year 
#' @param path is the way to the folder of the map and dataframe
#' @param type_carte is a chr that will we be use to name the map
#' @param type_donnee is a chr that represent the variable use for map
#'
#' @return a dataframe with the value of interest and its grid
#' @export
#'
#' @examples
mosaic_gridDataframe <- function(data,time,path,type_carte,type_donnee){

  Grid1=gridNplot(data,vart=type_donnee,varname=type_carte,
                 path.grids=path ,bathy.plot=TRUE,
                 spname='sp',tname= time ,xname='LONG',
                 p=0.95,zfilter="quantile",
                 yname='LAT',sname= "survey",
                 lon1=-7,lon2=0,lat1=48.1,lat2=43,
                 deep=-200,shallow=0,bstep=450,
                 bcol='grey50',drawlabels=TRUE,default.grid = FALSE)
  
  for(i in 2:13) {
    dev.off(i)
  }
  
  

  
  # Extract gridded data in gridDataframe format 
  gridDataframe=Grid1$gridDataframe
  head(gridDataframe)  
  
  
  return(gridDataframe)
  
  
  
 
}


