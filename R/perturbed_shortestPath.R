#' Title
#'
#' @param from
#' @param to
#' @param processed_roadNetwork
#' @param sd
#'
#' @return
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
