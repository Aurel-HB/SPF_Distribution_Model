#' this function take a frame of one year with the info of the cov per month in col
#' the aim is to have the same format of dataset than with the variable
#'
#' @param data <- frame
#' @param year <- chr
#' @param donnee <- chr
#'
#' @return a frame with long(num), lat(num), month(factor), year(int), survey(chr)
#' @export
#'
#' @examples
extract_cov <- function(data,Year,donnee){
  cov <- data.frame()
  for (place in 3:length(names(data))){
    month <- rep(place-2,length(data[,1]))
    month <- as.factor(month)
    frame <- data.frame(data[,1:2],month,data[,place])
    names(frame) <- c("LONG","LAT","month",donnee)
    cov <- rbind(cov,frame)
  }
  Year <- as.integer(Year)
  year <- rep(Year,length(cov[,1]))
  survey <- rep("Satelite",length(cov[,1]))
  sp <- rep("PPP",length(cov[,1]))
  cov <- data.frame(cov,year,survey,sp)
  return(cov)
}