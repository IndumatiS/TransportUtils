#' Shortest Path in the Network
#'
#' @param processed_roadNetwork sf_network after undergoing network processing
#' @param ToRowNumber Origin node relating to the list of origin destination input data
#' @param FromRowNumber Destination node relating to the list of origin destination input data
#' @param census_ODs
#'
#' @return vector of row indices of edges part of the shortest path network
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
