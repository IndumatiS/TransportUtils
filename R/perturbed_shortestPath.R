#' Title
#'
#' @param from Origin node relating to the list of origin destination input data
#' @param to Destination node relating to the list of origin destination input data
#' @param processed_roadNetwork
#' @param sd Standard deviation is applied to re-calculate the time_traveled so as to produce more than one shortest
#' routes between any given OD pairs.
#'
#' @return vector of row indices of edges part of the shortest path network
#' @export
#'
#' @examples
perturbed_shortestPath<-function(from, to, processed_roadNetwork,sd){
  #browser()
  roads.clean.timetravelled<-processed_roadNetwork %>% activate("edges") %>%
    mutate(perturbed_time1 = abs(time_travelled*(1+rnorm(length(time_travelled),sd=sd))) )

  paths <- as.numeric((st_network_paths(roads.clean.timetravelled, from = from, to = to, weights = "perturbed_time1"))$edge_paths[[1]])

  return(paths)
}
