#' This function create maps and synthesise them on a mosaic
#'
#' @param gridDataframe is interest value and its grid from mosaic_grid function
#' @param mat a matrix template
#' @param path is the way to the folder of the map and dataframe
#'
#' @return nothing because the maps are directly save 
#' @export
#'
#' @examples
mosaic_plot <- function(gridDataframe,mat,path){
  path.grid <- path
  
  mat <- grid.plot(pat=gridDataframe,input='gridDataframe',ux11=TRUE,
                   pat2=path.grid,
                   deep=-200,shallow=0,bstep=50,bcol='grey50',drawlabels=FALSE,
                   default.grid=FALSE,bathy.plot=TRUE,lon1=-7,lon2=0,
                   lat1=48.1,lat2=43,cex.labs = 0.7,
                   plotit=list(zmean=TRUE,zstd=TRUE,nz=TRUE,plot1=FALSE,
                               mosaic=TRUE))
}
