#Assign one of the four directions based on the bearings information

assign_direction<-function(bearings, userDataSliced){
 # browser()
  userDirection_Vector<-userDataSliced$Direction
  if(all(userDataSliced$Direction %in% c("N","S"))){
    if(between(bearings, 90,269)){
      direction1<-which(userDataSliced$Direction=="S")
      direction2<-which(userDataSliced$Direction=="N")
    }

    else{
      direction1<-which(userDataSliced$Direction=="N")
      direction2<-which(userDataSliced$Direction=="S")
    }
  }

  if(all(userDataSliced$Direction %in% c("E","W"))){
    if(between(bearings, 0,179)){
      direction1<-which(userDataSliced$Direction=="E")
      direction2<-which(userDataSliced$Direction=="W")
    }

    else{
      direction1<-which(userDataSliced$Direction=="W")
      direction2<-which(userDataSliced$Direction=="E")
    }
  }

  returnVector<-c(direction1,direction2)
  return(returnVector)
}
