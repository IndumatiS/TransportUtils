#' Simplifies sf_data
#' @description Here the sf_data undergoes some simplification process which includes arranging the rows by edge length, removing multiple edges and removing edges with loops
#' @param sf_network The sf_data extracted from OSM needs to be converted to sf_network first before being passed into this function
#'
#' @return sf_network should be of class: "sfnetwork" "tbl_graph" "igraph"
#' @export
#'
#' @examples
#' osm_data<-extract_data( c( -1.41785, -1.09785, 51.59201, 51.91201)) # for Oxford region
#' osm_roads_data<-modify_Linesdata(osm_data, road_types = c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ) ,road_levels = NULL,crs=9766)
#' osm_polygon_data<-modify_Polygondata(osm_data,road_types=c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ), road_levels = NULL,crs=9766 )
#' osm_roads_processed<-lineString_rounding(osm_roads_data,rounding=3) %>% as_sfnetwork(directed= FALSE) %>% simplify_sf_data()
#'
simplify_sf_data<-function(sf_network){

  simple = sf_network %>%
    activate("edges") %>%
    arrange(edge_length()) %>%
    filter(!edge_is_multiple()) %>%
    filter(!edge_is_loop())

  return(simple)
}
