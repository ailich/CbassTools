#' Function for detecting string type in camera table
#'
#' Function for detecting string type in camera table
#' @param cam_filepath vector of filepaths from blackfly camera

detect_cam_pattern<- function(cam_filepath){
  patterns<- unique(str_replace_all(string = basename(cam_filepath), pattern = "\\d+", replacement = "\\\\d+"))
  patterns
  my_pattern<- patterns[patterns != "\\d+-\\d+" & (!grepl(pattern = "*\\.txt", x = patterns))]
  my_pattern
  if(length(my_pattern)>1){
    message("Error: multiple patterns available")
    stop()}
  if(length(my_pattern)<1){
    message("Error: no patterns available")
    stop()}
  return(my_pattern)}

#' Calculates frame-rate of Blackfly Camera
#'
#' Calculates frame-rate of Blackfly Camera.
#'
#' @param cam_timestamp vector of camera timestamps (must be same length as cam_filepath)
#' @param cam_filepath vector of filepaths from blackfly camera (must be same length as cam_timestamp)
#' @param u_second vector of microseconds (Alternatively microseconds can already be in the timestamp) (must be same length as cam_filepath)
#' @param check logical indicating whether or not to check if camera was reset
#' @importFrom lubridate dmicroseconds
#' @import dplyr
#' @importFrom stringr str_extract
#' @export

calc_fps<- function (cam_timestamp, u_second = rep(0, length(cam_timestamp)), cam_filepath, check = TRUE) {
  if (check) {
    if (check_if_reset(cam_filepath)) {
      message("Error: camera was reset")
      stop()
    }
  }
  cam <- bind_cols(cam_timestamp = cam_timestamp, u_second = u_second, cam_filepath = cam_filepath)
  my_pattern<-  detect_cam_pattern(cam_filepath = cam_filepath)
  start_pt <- cam[10, ]
  end_pt <- tail(cam, n = 10)[1, ] #End Point (Edges trimmed similar to #2's code. Seems to result in fps very close (4e-11) to rate use by Alex #2)
  cam <- rbind(start_pt, end_pt)
  cam <- cam %>% mutate(base_name = basename(cam_filepath))
  cam <- cam %>% filter(grepl(pattern = my_pattern, x = cam$base_name))
  if(nrow(cam)!=2){
    message("Error: One of chosen rows was not a frame")
    stop()}
  cam <- cam %>% mutate(frame_id = as.numeric(str_extract(cam$base_name, "\\d+")))
  n_frames <- (cam$frame_id[2] - cam$frame_id[1]) + 1
  cam <- cam %>% mutate(exact_time = cam_timestamp + dmicroseconds(u_second))
  n_secs <- as.numeric(difftime(cam$exact_time[2], cam$exact_time[1], units = "secs"))
  fps <- n_frames/n_secs
  return(fps)
}
