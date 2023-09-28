#' Derives OD data from Census UK
#'
#' @description
#' Extracts the Census UK. The object returned is then passed onto addODnodes() function to map them on the
#' processed sf_network.
#'
#' @param region The PCT region or local authority to download data from (e.g. oxford or Leeds). See View(pct_regions_lookup) for a full list of possible region names.
#' @param crs coordinate reference system
#'
#'
#' @return returnList[[1]] - contains traffic demands between any given ODs.
#'         returnList[[2]] - contains origin and destination nodes and their coordinates. This is an
#'         input to addODnodes() function.
#' @export
#'
#' @examples
#'
extract_CensusOD_data<-function(region, crs){
  returnList<-list()
  desire.lines<-get_desire_lines(region)

  list1<-st_transform(desire.lines, crs=crs) #converts coordinates to the user defined crs.
  list1<- list1 %>% dplyr::select(geo_code1, geo_code2, car_driver, geometry)


  listGeo_code1<-list1[which(list1$geo_code1==list1$geo_code2[1]),]
  listGeo_code2<-list1[which(list1$geo_code2==list1$geo_code1[1]),]


  end_coordinates<-as.data.frame(st_coordinates(listGeo_code1$geometry) %>%
                                   .[seq(2, nrow(.), by = 2),]) %>%
    mutate(Geo_code=listGeo_code1$geo_code2)#ending coordinate


  start_coordinates<-as.data.frame(st_coordinates(listGeo_code2$geometry) %>%
                                     .[seq(1, nrow(.), by = 2),]) %>%
    mutate(Geo_code=listGeo_code2$geo_code1)#starting coordinate


  # Perform a join on everything (full outer join)
  result <- merge(end_coordinates, start_coordinates, by = "Geo_code", all = TRUE)


  # Fill NA in Value1 with corresponding values from Value2
  list2 <- result %>%
    mutate(
      X = coalesce(X.x, X.y),
      Y = coalesce(Y.x, Y.y)
    ) %>%
    as.data.frame()%>%
    dplyr::select(Geo_code,X,Y)

  returnList[[1]]<-list1
  returnList[[2]]<-list2
  names(returnList)<-c("censusTrafficDemands","UniqueODnodes")

  return(returnList) #test this
}
