#' Title
#'
#' @param processed_roadNetwork
#' @param census_ODs
#' @param sd
#'
#' @return
#' @export
#'
#' @examples
create.Amatrix<-function(processed_roadNetwork,census_ODs,sd=0.3){

  #define variables
  counter<-1
  list_names<-c()
  origin_list<-c()
  destination_list<-c()
  a_matrix<-list()
  return_list<-list()
  roads.clean.timetravelled<-processed_roadNetwork %>% activate("edges") %>%
    mutate(time_travelled = weight/highway_median)
  list_of_dfs<-list()
  matrix_column_length<-nrow(roads.clean.timetravelled %>% activate("edges") %>% st_as_sf())
  census_ODs_nrow<-nrow(census_ODs) #add this once the utils transport library is ready for distribution.

  #Looping
  for(i in 1:5){

    from=snapped_geometry_script(rowIndex=i, processed_roadNetwork, census_ODs)
    for(j in 1:5){
      to=snapped_geometry_script(rowIndex=j, processed_roadNetwork, census_ODs)
      if((i!=j) & (census_ODs$geometry[i]!=census_ODs$geometry[j] )){
        #browser()
        list_of_dfs <- lapply(1:100, function(z) perturbed_shortestPath(from,to,processed_roadNetwork=roads.clean.timetravelled,sd=sd))

        duplicated_vectors<-duplicated(list_of_dfs)

        #Derive metadata related to each of the columns
        list_of_vecNames<-rep(paste0("shortest_route_from_", i,"_",j), length(list_of_dfs[duplicated_vectors==FALSE]))
        list_names[[counter]]<-list_of_vecNames
        #browser()

        #Seperated lists for Origin and destination
        temp_list<- rep(i, length(list_of_dfs[duplicated_vectors==FALSE]))
        origin_list[[counter]]<-temp_list
        temp_list<- rep(j, length(list_of_dfs[duplicated_vectors==FALSE]))
        destination_list[[counter]]<-temp_list


        a_matrix[[counter]]<-lapply(list_of_dfs[duplicated_vectors==FALSE], function(x) convert_0_1(x,matrix_column_length))

        #browser()
        counter<-counter+1
        print(i)
      }
    }
  }
  #Return objects
  return_list[[1]]<-purrr::flatten(list_names)
  return_list[[2]]<-purrr::flatten(origin_list)
  return_list[[3]]<-purrr::flatten(destination_list)
  return_list[[4]]<-do.call(cbind,purrr::flatten(a_matrix))


  return(return_list)
}