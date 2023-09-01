#' Title
#'
#' @param x
#' @param matrix_column_length
#'
#' @return
#' @export
#'
#' @examples
convert_0_1<-function(x,matrix_column_length){
  #create the new list with all 0s, then add 1s to the path index, then store this in the list initialised outside   the loop, then add counter
  matrix_column<- rep(0,matrix_column_length)
  matrix_column[x]<- 1


  return(matrix_column)
}
