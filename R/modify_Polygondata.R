#' Modifies the extracted polygon data from OSM
#'
#' @param sf_data OSM data extracted
#' @param road_types road types which are to be retained alternatively,
#' @param road_levels road level thresholds to be retained.
#' @param crs enter the appropriate coordinate reference system (crs) appropriate to the region of interest
#'
#' @return modified OSM polygons data containing user specified road types/levels - sf data frame
#' @export
#'
#' @examples
#' osm_data<-extract_data( c( -1.41785, -1.09785, 51.59201, 51.91201)) # for Oxford region
#' osm_roads_data<-modify_Polygondata(osm_data, road_types = c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary','secondary_link' ,'tertiary' , 'tertiary_link' ) ,road_levels = NULL,crs=9766)
#'
modify_Polygondata<-function(sf_data, road_types, road_levels, crs){

  levels<-c('motorway'=1, 'motorway_link'=1, 'trunk'=2,'trunk_link'=2, 'primary'=3, 'primary_link'=3, 'secondary'=4,
            'secondary_link'=4 ,'tertiary'=5 , 'tertiary_link'=5 ,  'residential'=6 ,'living_street'=7, 'unclassified'=8)

  sf_data$osm_polygons <- sf_data$osm_polygons %>%
    mutate(polygon_levels = levels[highway])



  roads.polygons<-tryCatch({st_transform(sf_data$osm_polygons, crs = crs)},
                           error=function(err){
                             roads.polygons=NA
                             stop("Check either sf_data type or the validity of crs")})

  #Stop if both road types and levels have values
  if(!(is.null(road_levels)) & !(is.null(road_types))){
    stop("Either road_levels or road_types should be used to filter OSM data; not both.")
  }

  #Filter OSM data if road_types argument has value other than NULL
  if(!(is.null(road_types))){
    roads.polygons<-tryCatch({roads.polygons[roads.polygons$highway %in% road_types,]},
                             error= function(err){
                               roads.polygons=NA
                               stop("The road_types list need to be a subset of c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary',
                'secondary_link' ,'tertiary' , 'tertiary_link' ,  'residential' ,'living_street', 'unclassified')")
                             })
  }

  #Filter OSM data if road_levels argument has value other than NULL
  if(!(is.null(road_levels))){
    roads.polygons<-tryCatch({roads.polygons[roads.polygons$polygon_levels <= road_levels,]},
                             error= function(err){
                               roads.polygons=NA
                               stop("The road_types list need to be a subset of c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary',
                'secondary_link' ,'tertiary' , 'tertiary_link' ,  'residential' ,'living_street', 'unclassified')")
                             })
  }



  message("Note: The road_types list need to be a subset of c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary',
                'secondary_link' ,'tertiary' , 'tertiary_link' ,  'residential' ,'living_street', 'unclassified')")

  roads.polygons<-roads.polygons %>% select(geometry, highway, oneway, polygon_levels)

  roads.polygons <- st_as_sf(roads.polygons)  %>%
    st_transform((crs=crs)) %>%
    mutate(oneway = case_when(oneway == 'yes' ~ TRUE,
                              TRUE ~ FALSE))

  return(roads.polygons)
}
