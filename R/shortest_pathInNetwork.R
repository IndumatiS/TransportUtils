#' Title
#'
#' @param processed_roadNetwork
#' @param ToRowNumber
#' @param FromRowNumber
#' @param census_ODs
#'
#' @return
#' @export
#'
#' @examples
shortest_pathInNetwork<-function(processed_roadNetwork, ToRowNumber, FromRowNumber, census_ODs){
  paths = st_network_paths(processed_roadNetwork,
                           from = snapped_geometry_script(rowIndex=ToRowNumber, processed_roadNetwork, census_ODs),
                           to = snapped_geometry_script(rowIndex=FromRowNumber, processed_roadNetwork, census_ODs),
                           weights = "time_travelled")
  shortestPath_network<- processed_roadNetwork %>% activate("edges") %>% st_as_sf() %>% slice (unlist(paths$edge_paths))

  return(shortestPath_network)
}
