#' Adds Origin destination points on the network
#'
#' @description
#' Adds orgin destination points derived from the library pct (propensity to cycle tool) on to the nearest nodes on the
#' sf_network provided.
#'
#' @param region The PCT region or local authority to download data from (e.g. oxford or Leeds). See View(pct_regions_lookup) for a full list of possible region names.
#' @param crs coordinate reference system
#' @param sf_network should be of class: "sfnetwork" "tbl_graph" "igraph"
#'
#' @return
#' @export
#'
#' @examples
#' osm_data<-extract_data( c( -1.41785, -1.09785, 51.59201, 51.91201)) # for Oxford region
#' osm_roads_data<-modify_Linesdata(osm_data, road_types = c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ) ,road_levels = NULL,crs=9766)
#' osm_polygon_data<-modify_Polygondata(osm_data,road_types=c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ), road_levels = NULL,crs=9766 )
#' osm_roads_processed<-lineString_rounding(osm_roads_data,rounding=3) %>% as_sfnetwork(directed= FALSE) %>% simplify_sf_data()
#' osm_polygons_processed<-lineString_rounding(osm_polygon_data,rounding=3) %>% as_sfnetwork(directed= FALSE) %>% simplify_sf_data()
#' joined_script <- st_network_join(osm_roads_processed, osm_polygons_processed) %>% network_preProcessing()

add_CensusOD_points<-function(region, crs, sf_network){
  oxford.desire.lines<-get_desire_lines(region)
  oxford.desire.lines<-oxford.desire.lines[oxford.desire.lines$geo_code1==oxford.desire.lines$geo_code1[1],] #test this
  oxford.desire.lines<-st_transform(oxford.desire.lines, crs=crs) #converts coordinates to the user defined crs.

  # Find indices of nearest nodes.
  nearest_nodes_script = st_nearest_feature(oxford.desire.lines, sf_network%>% activate("nodes"))
  oxford.desire.lines$nearest_node<-nearest_nodes_script

  # Snap geometries of POIs to the network.
  snapped_pois_script = sf_network %>% activate("nodes") %>% st_as_sf() %>% select(geometry)  %>% slice(nearest_nodes_script)

  temp<-as.data.frame(st_coordinates(censusOD_points[[1]]$geometry) %>%
                        .[seq(2, nrow(.), by = 2),]) %>%
    st_as_sf( coords = c("X", "Y"), crs = crs)
  snapped_pois_script$geoCode2<-oxford.desire.lines$geo_code2
  snapped_pois_script$original_coordinates<-temp$geometry
  #snapped_pois = oxford.desire.lines %>% st_set_geometry(st_geometry(sf_network)[nearest_nodes])

  list1<-list(oxford.desire.lines, snapped_pois_script)
  return(list1) #test this
}
