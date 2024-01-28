#' Title
#'
#' @param sfdf sf object
#' @param study_domain_sf sf object of the study area
#'
#' @return
#' @export
#'
#' @examples
in_out_domain <- function(sfdf,study_domain_sf) {
  out <- sfdf %>%
    st_intersects(study_domain_sf) %>%
    lapply(., function(x) {ifelse(length(x) == 0, 0, 1)}) %>%
    unlist()
  return(out)
}