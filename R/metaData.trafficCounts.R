#' Title
#'
#' @param a_matrix_list
#' @param censusOD_points
#' @param region
#'
#' @return
#' @export
#'
#' @examples
metaData_trafficCounts<-function(a_matrix_list, censusOD_points, region){
  df<-as.data.frame(cbind(a_matrix_list[[2]],a_matrix_list[[3]]))%>% distinct(V1, V2, .keep_all=TRUE)
  oxford.desire.lines<-get_desire_lines(region)
  traffic_countVector<-c()
  geo_codeOriginVector<-c()
  geo_codeDestiVector<-c()

  for(i in 1:nrow(df)){
    geo_codeOrigin<-censusOD_points[[2]]$geoCode2[unlist(df$V1[i])]
    geo_codeOriginVector<-append(geo_codeOriginVector,geo_codeOrigin)
    geo_codeDesti<-censusOD_points[[2]]$geoCode2[unlist(df$V2[i])]
    geo_codeDestiVector<-append(geo_codeDestiVector,geo_codeDesti)
    temp<-oxford.desire.lines %>% filter(geo_code1 == geo_codeOrigin & geo_code2 == geo_codeDesti)
    traffic_countVector<-append(traffic_countVector,temp$car_driver)
  }

  #test:nrow(df) and length(traffic_countVector) should be equal

  returnDf<-as.data.frame(cbind(df$V1,df$V2,geo_codeOriginVector,geo_codeDestiVector,traffic_countVector))
  colnames(returnDf)<-c("Origin_index", "Destination_index", "geo_codeOrigin", "geo_codeDesti","Traffic_Count")

  return(returnDf)
}
