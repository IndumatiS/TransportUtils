#' View Sf network
#'
#' @param sf_network
#'
#' @return map of the sf_network
#' @export
#'
#' @examples
#' data(joined_script)
#' viewSf_network(joined_script)
viewSf_network<-function(sf_network){
  tmap_mode("view")
  map<-tm_tiles("CartoDB.Positron") +
    tm_shape(st_as_sf(sf_network, "edges")) +
    tm_lines() +
    tm_shape(st_as_sf(sf_network, "nodes")) + tm_dots()

  return(map)
}
