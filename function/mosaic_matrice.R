#' Create a matrix template that will be use in the function mosaic_plot
#'
#' @param gridDataframe is interest value and its grid from mosaic_grid function
#'
#' @return a matrix
#' @export
#'
#' @examples
mosaic_matrice <- function(gridDataframe){
  
  mat <- grid.plot(pat=gridDataframe,input='gridDataframe',ux11=TRUE,pat2=NULL,
                   namz='mean_presence',nams='stdev',namn='no of samples',deep=-200,
                   shallow=0,bstep=50,bcol='grey50',drawlabels=TRUE,
                   default.grid=FALSE,bathy.plot=TRUE,lon1=-7,lon2=0,
                   lat1=48.1,lat2=43,cex.labs = 0.7,
                   plotit=list(zmean=TRUE,zstd=TRUE,nz=TRUE,plot1=FALSE,
                               mosaic=FALSE))
  return(mat)
}