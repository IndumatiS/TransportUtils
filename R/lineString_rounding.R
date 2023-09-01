#' Rounding off the co-coordinates
#'
#' @param sfdata_dataframe
#' @param rounding to which nearest decimal
#'
#' @return sf data frame
#' @export
#'
#' @examples
#' osm_data<-extract_data( c( -1.41785, -1.09785, 51.59201, 51.91201)) # for Oxford region
#' osm_roads_data<-modify_Linesdata(osm_data, road_types = c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ) ,road_levels = NULL,crs=9766)
#' osm_roads_data<-modify_Polygondata(osm_data, road_types = c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ) ,road_levels = NULL,crs=9766)
#' osm_roads_processed<-lineString_rounding(osm_roads_data,rounding=3)
#'
lineString_rounding<-function(sfdata_dataframe, rounding=3){
  sf_data.lines <- suppressWarnings(st_cast(sfdata_dataframe, "LINESTRING"))

  st_geometry( sf_data.lines) = st_geometry( sf_data.lines) %>%
    lapply(function(x) round(x, rounding)) %>%
    st_sfc(crs = st_crs( sf_data.lines))

  return(sf_data.lines)
}
