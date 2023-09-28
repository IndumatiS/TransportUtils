#' Calculate bearing
#'
#' @param lat1 - latitude of starting node
#' @param lon1 - longitude of starting node
#' @param lat2 - latitude of ending node
#' @param lon2 - longitude of ending node
#'
#' @return Bearing in degrees
#' @export
#'
#' @examples
calculate_initial_bearing <- function(lat1, lon1, lat2, lon2) {
  # Convert latitude and longitude from degrees to radians
  lat1 <- deg2rad(lat1)
  lon1 <- deg2rad(lon1)
  lat2 <- deg2rad(lat2)
  lon2 <- deg2rad(lon2)

  # Calculate the difference in longitudes
  dLon <- lon2 - lon1

  # Calculate the initial bearing using the Haversine formula
  y <- sin(dLon) * cos(lat2)
  x <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dLon)
  initial_bearing <- atan2(y, x)

  # Convert the initial bearing from radians to degrees
  initial_bearing <- rad2deg(initial_bearing)

  # Normalize the bearing to be in the range [0, 360]
  initial_bearing <- (initial_bearing + 360) %% 360

  return(initial_bearing)
}

