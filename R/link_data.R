#' @title Traffic counts -Oxford region
#'
#' @description A sample of traffic counts for Oxford region in relation to particular roads.
#' The traffic has been counted at a particular point and  is related to
#' one of the directions of a road.



#' @format{
#'  the data is of the class sfnetwork, tbl_graph and igraph. Upon converting the data to an sf_object the user can see the values of the columns as follows:
#'    \describe{
#'      \item{Site}{Site of traffic count data collection}
#'      \item{Name}{Street identifier}
#'      \item{Long}{Longitute}
#'      \item{Lat}{Latitude}
#'      \item{Direction}{Direction of the traffic flow}
#'      \item{Count}{Traffic count}
#'    }
#'}
"link_data"
