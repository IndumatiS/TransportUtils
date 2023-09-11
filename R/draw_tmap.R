#' Draw map
#'
#' @param sf_network_directed should be of class: "sfnetwork" "tbl_graph" "igraph"
#' @param shortestPath_network should be of class: "sfnetwork" "tbl_graph" "igraph"
#' @param shortestPathReverse_network should be of class: "sfnetwork" "tbl_graph" "igraph"
#' @param ToRowNumber Origin node relating to the list of origin destination input data
#' @param FromRowNumber Destination node relating to the list of origin destination input data
#' @param col1 colour for shortestPath. Default set to red.
#' @param col2 colour for reverse_shortestPath. Default set to pink.
#' @param lwd Line width. Default set to 3.
#' @param lty Line type. Default set to "dashed".
#' @param shape Line shape. Default set to 8.
#'
#' @return Tmap object
#' @export
#'
#' @examples
draw_tmap<-function(sf_network_directed,
                    shortestPath_network=NULL,
                    shortestPathReverse_network=NULL,
                    ToRowNumber=NULL,
                    FromRowNumber=NULL,
                    col1= "red",
                    col2="pink",
                    lwd=3,
                    lty="dashed",
                    shape=8){

  tmap_mode("view") # set to interactive mode
  tmmap<-tm_tiles("CartoDB.Positron") +
    tm_shape(st_as_sf(sf_network_directed, "edges")) +
    tm_lines(col= "oneway", palette = "Accent", lwd=5)+
    tm_shape(st_as_sf(sf_network_directed, "nodes")) +
    tm_dots()

  if(!(is.null(shortestPath_network)) & !(is.null(shortestPathReverse_network))){
    tmmap<- tmmap+
      tm_shape(shortestPath_network)+
      tm_lines(col=col1, lwd=lwd, lty = lty) +
      tm_shape(shortestPathReverse_network)+
      tm_lines(col=col2, lwd=lwd, lty = lty)

  }
}
