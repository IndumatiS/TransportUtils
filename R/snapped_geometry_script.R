#' Snapped Geometry
#'
#' The coordinates from of the origin destination nodes are mapped to the closest node on the sf_network.
#'
#' @param rowIndex The row index of origin destination table
#' @param processed_roadNetwork The final sf_network after undergoing network processing.
#' @param census_ODs Table of origin destination data.
#'
#' @return rowIndex of the sf_network which is closest to the origin destination node.
#' @export
#'
#' @examples
snapped_geometry_script<-function(rowIndex, processed_roadNetwork, census_ODs ){
  #browser()
  p1<-processed_roadNetwork %>% activate("nodes") %>% st_as_sf()
  rowIndex<-which(p1$geometry == census_ODs$geometry[rowIndex])
  return(rowIndex)
}
