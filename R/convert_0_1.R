#' Producing matrix columns of 1s and 0s
#'
#' This function will be called exclusively from create.Amatrix function.
#' @param x Every vector in a list
#' @param matrix_column_length The length of every vector in argument x.
#'
#' @return A vector of 0s and 1s where 1s indicate the row index that is part of the shortest path and
#' 0s indicate the route index not part of the shortest path.
#' @export
#'
#' @examples
convert_0_1<-function(x,matrix_column_length){
  #create the new list with all 0s, then add 1s to the path index, then store this in the list initialised outside   the loop, then add counter
  matrix_column<- rep(0,matrix_column_length)
  matrix_column[x]<- 1


  return(matrix_column)
}
