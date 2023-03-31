#' Interpolate Ship Position
#'
#' Interpolates NA's by filling them with the average of the previous and subsequent values.
#' @param my_data vector of data
#' @param na.rm Logical indicating whether a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @export

interp<- function(my_data, na.rm=FALSE){
  if(sum(is.na(my_data))==length(my_data)){
    return(my_data)} #If all NA's just return a vector of NA's
  new_data <- my_data
  idx_min <- min(which(!is.na(my_data))) #First non NA value (saves time b/c don't need to loop through a ton of leading NA's)
  idx <- which(is.na(my_data))
  idx <- idx[max(idx_min-1,1):length(idx)] #Start 1 before first non NA value or 1
  for (i in idx) {
    new_data[i] <- mean(c(my_data[i - 1], my_data[i + 1]),na.rm=na.rm)
  }
  return(new_data)}
