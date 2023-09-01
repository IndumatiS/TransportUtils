#' Title
#'
#' @param sf_network
#'
#' @return
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
