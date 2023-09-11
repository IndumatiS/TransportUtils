#'
#'
#' @param lat_col
#' @param long_col
#' @param crs
#' @param sf_network_directed
#' @param inputCoordinatedf
#'
#' @return
#' @export
#'
#' @examples
addODnodes<-function(lat_col, long_col, crs, sf_network_directed, inputCoordinatedf){
  data_sf <- st_as_sf(inputCoordinatedf, coords = c(lat_col, long_col), crs = crs )
  nearest_nodes_script = st_nearest_feature(data_sf, sf_network_directed%>% activate("nodes"))
  snapped_pois_script = sf_network_directed %>% activate("nodes") %>% st_as_sf() %>% select(geometry)  %>% slice(nearest_nodes_script)

  return(snapped_pois_script)
}
