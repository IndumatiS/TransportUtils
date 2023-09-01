#' Title Extract OSM data.
#'
#' @param bounding_coordinates
#'
#' @return Data frame containing OSM data pertaining to the bouding coordinates
#' @export
#'
#' @examples
#' osm_data<-extract_data( c( -1.41785, -1.09785, 51.59201, 51.91201)) #for Oxford region
extract_data<-function(bounding_coordinates){
  #Oxford - c(-1.41785, -1.09785, 51.59201, 51.91201)
  #bounding box

  bbx<-matrix(
    data = bounding_coordinates,
    nrow = 2,
    ncol = 2,
    byrow= T,
    dimnames = list(c('x','y'), c('min','max'))
  )

  query<- tryCatch({bbx %>%
      opq() %>%
      add_osm_feature(
        key= 'highway',
        value = c('motorway', 'motorway_link', 'trunk','trunk_link', 'primary', 'primary_link', 'secondary',
                  'secondary_link' ,'tertiary' , 'tertiary_link' ,  'residential' ,'living_street', 'unclassified')
      )},
      error=function(err){
        query=NA
        stop("Bounding coordinates should be exactly as per specifications. Also check if all the required libraries are loaded.")})

  message("downloading OSM data")
  sf_data<-osmdata_sf(query)


  return(sf_data)
}

