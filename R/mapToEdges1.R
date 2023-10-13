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
mapToEdges1<-function(userData, sf_network, crs, lat_col, long_col,SiteColname){
  return_list<-list()
  #Convert geometry from userData to the input crs
  transformedCRS<-as.data.frame(userData) %>%
    st_as_sf(coords = c(long_col, lat_col), crs=4326) %>%
    st_transform(crs=crs)
  uniqueSites<-unique(userData[[SiteColname]])

  #for each of the unique sites identify the nearest edge, including the
  #the edge travelling the opposite direction. Extract counts and direction information
  #related to that site and add them on as lists.

  list1<-list()
    for (i in 1:length(uniqueSites)){

      sliced_userData<- transformedCRS %>% filter(.[[SiteColname]] == uniqueSites[i])

      #Identify the edge information related to coordinates of the unique sites,
      nearestEdge_indicies<-st_nearest_feature(sliced_userData[1,],joined_script%>% activate("edges"))

      sliced_sfnetwork<-joined_script %>%
        activate("edges") %>%
        st_as_sf()%>%
        slice(nearestEdge_indicies)


      #For one way roads
      if(sliced_sfnetwork$oneway==TRUE){
        list1[[i]]<-sliced_sfnetwork   %>%
          st_as_sf() %>%
        mutate(usercounts=sliced_userData$Count[indicies[1]],
               userDirection=sliced_userData$Direction[indicies[1]])

      }
      #For two way roads
    if(sliced_sfnetwork$oneway==FALSE){
      #browser()

      sfnetworkTwo<-joined_script %>%
        activate("edges") %>%
        st_as_sf() %>%
        slice(-nearestEdge_indicies) %>%
        as_sfnetwork()

      secondnearestEdge_indicies<-st_nearest_feature(sliced_userData[1,],
                                                     sfnetworkTwo%>%
                                                      activate("edges"))
      sliced_sfnetworkTwo<-sfnetworkTwo %>%
        activate("edges") %>%
        st_as_sf() %>%
        slice( secondnearestEdge_indicies)


      indicies<-assign_direction(sliced_sfnetwork$BearingsInDegrees[1],
                                 userDataSliced=sliced_userData)

      #Add counts and direction information based on the values returned by assign_directions
      sliced_sfnetwork<-sliced_sfnetwork%>%
        mutate(usercounts=sliced_userData$Count[indicies[1]],
               userDirection=sliced_userData$Direction[indicies[1]])

      sliced_sfnetworkTwo<- sliced_sfnetworkTwo%>%
        mutate(usercounts=sliced_userData$Count[indicies[2]],
               userDirection=sliced_userData$Direction[indicies[2]])

      #rbind the two sliced sfnetwork
      list1[[i]]<-rbind(sliced_sfnetwork,sliced_sfnetworkTwo)

    }
      #print(list1[[i]])

    }

  return(list1)
  }
