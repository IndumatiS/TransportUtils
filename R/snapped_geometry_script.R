#' Title
#'
#' @param rowIndex
#' @param processed_roadNetwork
#' @param census_ODs
#'
#' @return
#' @export
#'
#' @examples
snapped_geometry_script<-function(rowIndex, processed_roadNetwork, census_ODs ){
  #browser()
  p1<-processed_roadNetwork %>% activate("nodes") %>% st_as_sf()
  rowIndex<-which(p1$geometry == census_ODs$geometry[rowIndex])
  return(rowIndex)
}
