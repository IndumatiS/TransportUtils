#' Creating directed network
#'
#' Creates directed network by reversing the start and end coordinates of two way roads
#' and adding these as extra edges on the final sf_network.
#' @param sf_network The processed sf_network after network processing step.
#'
#' @return sf_network This sf_network will have approximately twice the number of edges
#' and the same number of nodes as the original sf_network passed as an argument.
#' @export
#'
#' @examples
directed_network<-function(sf_network){
  roads.twoway<- sf_network %>%
    activate("edges") %>%
    filter(oneway==FALSE) %>%
    st_reverse() %>%
    st_as_sf()

  roads.clean.df<-sf_network %>%
    activate("edges") %>%
    st_as_sf()

  roads.clean.directed<-suppressWarnings(rbind(roads.clean.df, roads.twoway) %>%
                                           as_sfnetwork() %>%
                                           activate("edges") %>%
                                           mutate(time_travelled = weight/highway_median) %>%
                                           filter(!is.na(time_travelled)))


  return(roads.clean.directed)
}
