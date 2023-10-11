
#' Bearings of the traffic flow information
#'
#' @description Identifies direction of the traffic flow information.
#' Every edge within the network has traffic flow information and starting and ending coordinates. Using the start and end coordinates
#' this function calculates the cardinal and inter-cardinal direction.
#'
#' @param joined_script
#'
#' @return joined_script with added column on traffic flow direction
#' @export
#'
#' @examples
#' directiondf<-findEdgesBearings(joined_script)
findEdgesBearings<-function(joined_script){

 edges_df<-joined_script %>% activate("edges") %>% st_as_sf()
 edgesdf<-st_transform(edges_df, crs = "WGS84")
 reversed_edgesdf<-edgesdf %>% st_reverse()
 start_coordinates<-list()
 end_coordinates<-list()

 for(i in 1:nrow(edgesdf)){
   start_coordinates[[i]]<-st_coordinates(edgesdf$geometry[i])[1,]
   end_coordinates[[i]]<-st_coordinates(reversed_edgesdf$geometry[i])[1,]
 }

 start_df<-as.data.frame(do.call(rbind, start_coordinates))
 end_df<-as.data.frame(do.call(rbind, end_coordinates))

 vector<-calculate_initial_bearing(start_df$X,
                                   start_df$Y,
                                   end_df$X,
                                   end_df$Y)
 joined_script<- joined_script %>%
                 activate("edges") %>%
                 mutate(BearingsInDegrees = vector)


 return(joined_script)

}
