#' Calculates width of video frame
#'
#' Calculates width of video frame in meters (Grasty 2014)
#' Grasty, S. (2014). Use of a towed camera system for estimating reef fish population dynamics on the West Florida Shelf. (Master's), University of South Florida.
#' @param alt altitude
#' @param pitch pitch in degrees
#' @param cam_angle camera angle in degrees (as measured down from the horizontal)
#' @param H_AFOV_air Horizontal angular field of view in air in degrees for the camera system (depends on lens and image sensor size)
#' @export
calc_width<- function(alt, pitch, cam_angle=32.8, H_AFOV_air= 82.4){
  #Constants
  n_air<- 1.000277
  n_sea<- 4/3

  #Convert to Radians
  pitch<- radians(pitch)
  cam_angle<- radians(cam_angle)
  H_AFOV_air<- radians(H_AFOV_air)

  #Calc Width
  cam_angle_to_ground<- cam_angle-pitch #Eq 1
  alt_adj<- cos(pitch)*alt #Eq 2
  center_dist<- alt_adj/sin(cam_angle_to_ground) #Eq 3
  H_AFOV_water<- 2 * asin(sin((H_AFOV_air/2))*(n_air/n_sea)) #Eq 4 (By snell's law)
  width<- 2*center_dist*tan(H_AFOV_water/2) #Eq 5
  return(width)
}
