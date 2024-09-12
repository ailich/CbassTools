#' Retrieve media information
#'
#' Uses program MediaInfo to extract details from media (e.g. video or image files).
#' @param filepath file path of media file
#' @param MediaInfo_Path Path to MediaInfo program
#' @importFrom stringr str_replace_all
#' @details
#' You can download MediaInfo from https://mediaarea.net/en/MediaInfo
#'
#' @export

MediaInfo<- function(filepath, MediaInfo_Path){
  filepath<-  str_replace_all(string = filepath, pattern = "/{2,}", replacement = "/") #Remove multiple "/" that are in a row for command line interface in system call
  media_info <- system(paste(shQuote(Media_Info_Path), shQuote(filepath)), intern = TRUE)
  return(media_info)
}

#' Parse frames per second information
#'
#' Parses frames per second of video from output of MediaInfo function
#' @param vid_info video metadata output from the MediaInfo function
#' @import dplyr
#' @importFrom stringr str_extract
#' @export

parse_video_fps<-function(vid_info){
  fps_check<- sum(grepl(pattern = "Frame rate", vid_info),na.rm=TRUE) > 0
  if(fps_check == FALSE){
    message("Error: No Frame rate Field Present in Video")
    stop()}
  vid_fps<- vid_info[grepl(pattern = "Frame rate", vid_info)]
  vid_fps
  parse_check<- sum(grepl(pattern = "FPS", vid_info),na.rm=TRUE) > 0
  parse_check
  if(parse_check==FALSE){
    message("Error: Could not parse frame rate")
    stop()}
  vid_fps<- vid_fps %>%
    str_extract("\\d+\\.\\d+") %>%
    as.numeric()
  return(vid_fps)
}

#' Parse video duration
#'
#' Parses duration of video in seconds from output of MediaInfo function
#' @param vid_info output from MediaInfo function
#' @importFrom stringr str_remove
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_extract_all
#' @import dplyr
#' @export

parse_video_time<-function(vid_info){
  dur_check<- sum(grepl(pattern = "Duration", vid_info),na.rm=TRUE) > 0
  if(dur_check == FALSE){
    message("Error: No Duration Field Present in Video")
    stop()
  }
  vid_dur<- vid_info[grepl(pattern = "Duration", vid_info)][[1]]
  units_list=c("min", "s", "ms")
  parse_check<- vid_dur %>%
    str_remove("Duration") %>%
    str_remove_all("\\d") %>%
    str_remove(":") %>%
    str_remove(" min") %>%
    str_remove(" ms") %>%
    str_remove(" s") %>%
    str_remove_all(" ") #Should result in empty string
  if(parse_check!=""){
    message("Error: Could not parse video time")
    stop()
  }
  units_idx<- c(grepl(x = vid_dur, pattern = "\\d min"), grepl(x = vid_dur, pattern = "\\d s"), grepl(x = vid_dur, pattern = "\\d ms"))
  units<- units_list[units_idx]
  vals<- as.numeric(str_extract_all(string = vid_dur, pattern = "\\d+")[[1]]) #Values in corresponding units
  vals_secs<- rep(NA_real_, length(vals)) #Values in seconds
  for (i in 1:length(units)){
    if(units[i]=="min"){vals_secs[i]<-60*vals[i]}
    if(units[i]=="s"){vals_secs[i]<-vals[i]}
    if(units[i]=="ms"){vals_secs[i]<-vals[i]/1000} #COnvert vals to seconds
  }
  vals_secs<- sum(vals_secs) #Sum vals now that all in same units
  return(vals_secs)
}
