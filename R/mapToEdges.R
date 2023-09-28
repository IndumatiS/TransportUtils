#' Map user nodes onto sf_network edges
#'
#' @param userData
#' @param joined_script
#' @param crs
#' @param lat_col
#' @param long_col
#'
#' @return
#' @export
#'
#' @examples
mapToEdges<-function(userData, sf_network, crs, lat_col, long_col){
  #Convert geometry from userData to the input crs
  transformedCRS<-as.data.frame(userData) %>%
    st_as_sf(coords = c(long_col, lat_col), crs=4326) %>%
    st_transform(crs=crs)

  nearestEdges<-st_nearest_feature(transformedCRS,joined_script%>% activate("edges"))

  df<-cbind(transformedCRS,nearestEdges)
  return(df)
}
