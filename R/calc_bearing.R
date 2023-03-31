#' Calculates bearing between points
#'
#' Calculates bearing between two points in UTM coordinates
#' @param p1 Start point. Specified as c(Easting, Northing)
#' @param p2 End Point. Specified as c(Easting, Northing)
#' @export

calc_bearing<- function(p1,p2){
  b<- p2[1] - p1[1]
  h<- p2[2] - p1[2]
  if (is.na(b) | is.na(h)) {
    bearing <- NA_real_
  } else{
    Q<- 0
    if (b > 0 & h > 0) {Q<-1} #Determine Quadrant
    if (b > 0 & h < 0) {Q<-2}
    if (b < 0 & h < 0) {Q<-3}
    if (b < 0 & h > 0) {Q<-4}

    if(b == 0 & h == 0) {bearing = NA_real_} #Same point
    if (b == 0 & h > 0) {bearing<- 0}
    if (b == 0 & h < 0) {bearing<- 180}
    if (b > 0 & h == 0) {bearing<- 90}
    if (b < 0 & h == 0) {bearing<- 270}

    if(Q==1){
      bearing<- degrees(atan(abs(b/h)))
    }
    if(Q==2){
      theta<- degrees(atan(abs(b/h)))
      bearing<- 180 - theta
    }
    if(Q==3){
      theta<- degrees(atan(abs(b/h)))
      bearing<- 180 + theta
    }
    if(Q==4){
      theta<- degrees(atan(abs(b/h)))
      bearing<- 360 - theta
    }}
    return(bearing)
}
