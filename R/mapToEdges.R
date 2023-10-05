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
mapToEdges<-function(userData, sf_network, crs, lat_col, long_col,SiteColname){
  return_list<-list()
  #Convert geometry from userData to the input crs
  transformedCRS<-as.data.frame(userData) %>%
    st_as_sf(coords = c(long_col, lat_col), crs=4326) %>%
    st_transform(crs=crs)

  twoOrOneway<-table(userData[[SiteColname]])

  unique_twowaysites<- as.numeric(names( twoOrOneway[twoOrOneway == 2]))
  unique_onewaysites<-as.numeric(names( twoOrOneway[twoOrOneway == 1]))

  #for each of the unique sites identify the nearest edge, including the
  #the edge travelling the opposite direction. Extract counts and direction information
  #related to that site and add them on as lists.

  #First map the two way road traffic count information
  list1<-list()
  if(length(unique_twowaysites)!=0){
    for (i in 1:length(unique_twowaysites)){

      sliced_userData<- transformedCRS %>% filter(.[[SiteColname]] == unique_twowaysites[i])

      #Identify the edge information related to coordinates of the unique sites,
      #and add the respective counts and direction information.
      nearestEdge_indicies<-st_nearest_feature(sliced_userData[1,],joined_script%>% activate("edges"))
      sliced_sfnetwork<-joined_script %>%
        activate("edges") %>%
        st_as_sf() %>%
        slice(nearestEdge_indicies)

      indicies<-assign_direction(sliced_sfnetwork$BearingsInDegrees, userDataSliced=sliced_userData)

      sliced_sfnetwork<-sliced_sfnetwork%>%
                        mutate(usercounts=sliced_userData$Count[indicies[1]],
                        userDirection=sliced_userData$Direction[indicies[1]])

      #Identify the edge information related to reversed direction,
      #and add the respective counts and direction information.
      sliced_df_reversedDirection<-joined_script%>%
                                      activate("edges") %>%
                                    st_as_sf() %>%
                                    filter(from==sliced_sfnetwork$to & to==sliced_sfnetwork$from)%>%
                                    mutate(usercounts=sliced_userData$Count[indicies[2]],
                                    userDirection=sliced_userData$Direction[indicies[2]])
      #Row bind these two dataframes and store them as lists
      list1[[i]]<-rbind(sliced_sfnetwork,sliced_df_reversedDirection)

    }
  }

  else(
    list1<-NULL
  )


  #Second map the one way road traffic count information
  list2<-list()
  if(length(unique_onewaysites)!=0){
    for (i in 1:length(unique_onewaysites)){
      df<- transformedCRS %>% filter(.[[SiteColname]] == unique_onewaysites[i])

      #Identify the edge information related to coordinates of the unique sites,
      #and add the respective counts and direction information.
      nearestEdge_indicies<-st_nearest_feature(df[1,],joined_script%>% activate("edges"))
      sliced_df<-joined_script %>%
        activate("edges") %>%
        st_as_sf() %>%
        slice(nearestEdge_indicies) %>%
        mutate(usercounts=df$Count[1],
               userDirection=df$Direction[1])

      list2[[i]]<-sliced_df
    }
  }

  else(
    list2<-NULL
  )


  return_list[[1]]<-list1
  return_list[[2]]<-list2
  browser()
  #names(return_list)<-c("TwoWay_trafficCounts","OneWay_trafficCounts")
  return(return_list)
}
