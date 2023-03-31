#' Calculates distance CBASS has traveled
#'
#' Calculates distance (in meters) CBASS has traveled over a given duration based on speed and time
#' @param speed speed of CBASS
#' @param start_time start time
#' @param end_time end time
#' @param speed_units option of "knots" (default) or "m/s"
#' @param duration instead of start/end time can also supply the duration in seconds
#' @export
calc_dist<- function(speed, start_time=NULL, end_time=NULL, speed_units="knots", duration=NULL){
  if (speed_units=="knots") {speed=speed*.514444444444445}
  if (is.null(duration)){duration=as.numeric(end_time-start_time)}
  distance=speed*duration
  return(distance)
}
