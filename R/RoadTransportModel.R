#'
#'
#' @description
#' Executes all the steps in creating road transport model in  a single function. This includes line/node modification,
#' roundabout simplification, creating two way networks,  smoothing pseudo nodes etc.
#'
#' @param arg1 Location coordinates
#' @param arg2 Input road types or default to NULL
#' @param arg3 Input road levels or default to NULL
#' @param arg4 Coordinate reference system
#'
#' @return
#' @export
#'
#' @examples

##############################################################################3
RoadTransportModel<-function(arg1,
                             arg2= NULL,
                             arg3= NULL,
                             arg4 ){

  #Download all the osm data related to the bounding box coordinates
  osm_data<-extract_data(arg1)


  #Extract lines data
  osm_roads_data<-modify_Linesdata(osm_data, road_types = arg2,road_levels = arg3, crs=arg4)



  #Extract polygon data
  osm_polygon_data<-modify_Polygondata(osm_data,road_types=arg2, road_levels = arg3, crs=arg4 )

  #Data processing on lines data
  osm_roads_processed<-lineString_rounding(osm_roads_data,rounding=3) %>% as_sfnetwork(directed= FALSE) %>% simplify_sf_data()

  #Data processing on polygonlines data
  osm_polygons_processed<-lineString_rounding(osm_polygon_data,rounding=3) %>% as_sfnetwork(directed= FALSE)

  #Join the lines and polygon data, and process the joined data
  joined_script <- st_network_join(osm_roads_processed, osm_polygons_processed) %>%
    network_preProcessing() %>%
    directed_network() %>%
    findEdgesBearings()

  return(joined_script)

}
