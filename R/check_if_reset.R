#' Checks if camera has been restarted during transect
#'
#' Checks if camera has been restarted during transect by seeing if frames where written to different directories. Optionally can return index where restart occurred (Gives index of first value after restart).
#' @param cam_filepath vector of filepaths from blackfly camera
#' @param display_warning Logical indicating whether a warning should be displayed if restart is detected
#' @param return_idx Logical indicating whether the index of where the restart occurred should also be returned
#' @import dplyr
#' @importFrom stringr str_remove
#' @export

check_if_reset<- function (cam_filepath, display_warning = TRUE, return_idx = FALSE) {
  cam_filepath2<- tibble(file_path = cam_filepath)
  cam_filepath2<- cam_filepath2 %>% filter(grepl(pattern = "\\.", file_path))
  cam_filepath2<- cam_filepath2 %>% mutate(base_name = basename(file_path))
  cam_filepath2<- cam_filepath2 %>% mutate(cam_dir= str_remove(pattern = base_name, string = file_path))
  cam_dirs<- unique(cam_filepath2$cam_dir)
  cam_dirs
  was_reset<- length(cam_dirs)>1
  idx<- rep(NA_integer_, length(cam_dirs)-1)
  for (i in 2:length(cam_dirs)) {
    idx[i-1]<- grep(pattern = cam_dirs[i], cam_filepath)[1]+1
  }
  if (return_idx){
    output<- data.frame(was_reset=was_reset, idx=idx)} else{
      output<- was_reset}
  if(display_warning & was_reset){warning("Warning: Camera was reset!")}
  return(output)
}
