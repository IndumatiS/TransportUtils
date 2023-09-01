#' Processing sf_network
#' @description
#' The sf_network undergoes processing. These include 1) subdivision at the junctions, 2) removing redundant nodes,
#' 3) Clustering nodes based on eps values (particularly useful to cluster points around a round about as a single node),
#'  and 4) Contracting the network.
#'
#'
#' @param sf_network should be of class: "sfnetwork" "tbl_graph" "igraph"
#' @param eps It is the proximity of point in relation to other data pointsi.e. if the distance between two points is lower or equal to ‘eps’ then they are considered neighbors.
#'
#' @return sf_network
#' @export
#'
#' @examples
#' osm_data<-extract_data( c( -1.41785, -1.09785, 51.59201, 51.91201)) # for Oxford region
#' osm_roads_data<-modify_Linesdata(osm_data, road_types = c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ) ,road_levels = NULL,crs=9766)
#' osm_polygon_data<-modify_Polygondata(osm_data,road_types=c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ), road_levels = NULL,crs=9766 )
#' osm_roads_processed<-lineString_rounding(osm_roads_data,rounding=3) %>% as_sfnetwork(directed= FALSE) %>% simplify_sf_data()
#' osm_polygons_processed<-lineString_rounding(osm_polygon_data,rounding=3) %>% as_sfnetwork(directed= FALSE) %>% simplify_sf_data()
#' joined_script <- st_network_join(osm_roads_processed, osm_polygons_processed) %>% network_preProcessing()
network_preProcessing<-function(sf_network,eps=80){
  require(dbscan)
  subdivision = suppressWarnings(convert(sf_network, to_spatial_subdivision))
  smoothed = convert(subdivision, to_spatial_smooth, summarise_attributes = "first")

  # Retrieve the coordinates of the nodes.
  node_coords = smoothed %>%
    activate("nodes") %>%
    st_coordinates()

  clusters = dbscan(node_coords, eps = eps, minPts = 1)$cluster

  # Add the cluster information to the nodes of the network.
  clustered =  smoothed %>%
    activate("nodes") %>%
    mutate(cls = clusters)

  clustered = clustered %>%
    mutate(cmp = group_components())

  clustered<-clustered %>% activate("nodes") %>% filter(cmp<2)

  contracted = suppressWarnings(convert(
    clustered,
    to_spatial_contracted,
    cls, cmp,
    simplify = TRUE
  ))



  contracted.sf <- contracted %>%
    activate("edges") %>%
    st_as_sf()

  roads.clean <- suppressWarnings(as_sfnetwork(contracted.sf, directed = FALSE, length_as_weight = TRUE) )

  return(roads.clean)
}
