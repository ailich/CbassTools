#' Merge C-Vision CSV's
#'
#' Merges all C-Vision CSV's into 1 CSV using a list of files or will merge all csv's in current working directory.
#' @param file_list list of cvision files to be merged
#' @param frames_per_sec frames per second (default is NA)
#' @param vid_length Length of video in minutes (default is 1 min)
#' @param filename Optional name of csv file to write result to (Default is NULL)
#' @param append If FALSE, will overwrite existing file. If TRUE, will append to existing file. In both cases, if the file does not exist a new file is created (Default is FALSE).
#' @param strict logical indicating whether the procedure should stop if a csv cannot be processed (default is FALSE which instead displays a warning).
#' @export
#' @import dplyr
#' @import stringr
#' @import readr
#' @details
#' NOTE: Use total frame for blackfly/AVT and Total Time for axis when trying to match to timestamp or position (advice is outdated but holds for some older cruises b/c of drift in blackfly imagery relative to axis/actual time)

merge_cvision_csv<- function(file_list=list.files(pattern = "\\.csv$"), frames_per_sec=NA, vid_length=1, strict=FALSE, filename=NULL, append=FALSE) {
  fish<- tibble("Trip_ID" = integer(), "Tow_Number"= integer(), "Reviewer"=character(), "Tow_Type"=character(), "Fish_Number"=integer(), "Fish_Type"=character(), "Species"=character(), "Frame"=integer(), "Time_In_Video"=numeric(), file_name=character()) #Initialize accumulator

  NULL_df<- fish
  NULL_df$video<- numeric()
  NULL_df$Total_Time_In<- numeric()
  NULL_df$Total_Frame<- numeric()
  NULL_df<- NULL_df %>% select(file_name,video,Reviewer,Fish_Number,Fish_Type,Time_In_Video, Frame, Total_Time_In, Total_Frame)

  n_files<- length(file_list) #length of file_list
  if(n_files==0){
    warning("No annotation files")
    if(!is.null(filename)){
      readr::write_csv(NULL_df, file = filename, append = append)
    }
    return(NULL_df)
  }
  for (i in 1:length(file_list)){
    col_names<- c(names(suppressWarnings(suppressMessages(read_csv(file_list[i], n_max=0)))), "Count_Label")
    col_names<- col_names[col_names!="...10"] #Some files accidentally have an extra comma to fix ragged rows leading to this
    if(!identical(col_names, c("Trip_ID", "Tow_Number", "Reviewer", "Tow_Type", "Fish_Number", "Fish_Type", "Species", "Frame", "Time_In_Video", "Count_Label"))){
      n_files<- n_files-1 #reduce length by one
      if(strict){
        stop(paste("Error: Columns of file",  file_list[i], "not as expected. Stopping", basename(file_list[i])))} else{
          warning(paste("Columns of file",  file_list[i], "not as expected. Skipping", basename(file_list[i])))
          next()
          }} #Skip file if col names not as expected
    new_fish<- read_csv(file = file_list[i], col_types = "iicciccidc", col_names = col_names, skip=1)
    new_fish<- new_fish[,1:9] #Keep subsetting for now since we don't use count label column and removing this line would lead to different results
    new_fish<- new_fish %>% mutate(file_name= file_list[i])
    fish<- bind_rows(fish,new_fish)
    rm(i,new_fish)
    }
  if(n_files==0){
    warning("No annotation files")
    if(!is.null(filename)){
      readr::write_csv(NULL_df, file = filename, append = append)
    }
    return(NULL_df)
  }
  if(n_files<0){
    message("Error: Number of annotation files is somehow negative")
    stop() #Must be error in function code if this happens
  }
  if(max(fish$Time_In_Video) > (vid_length*60)){
    message("Error: Time in Video Greater than Video Length")
    stop()}
  fish<- fish %>% select(file_name, Reviewer, Fish_Number, Fish_Type, Time_In_Video, Frame)
  fish<- fish %>% mutate (short_filename =basename(file_name))
  fish<- fish %>% mutate (video =NA_real_) #Initialize video column

  for (j in 1:nrow(fish)) {
    curr_vid_num<- fish$short_filename[j] %>%
      str_remove(pattern = "ax\\d-") %>% #Remove axis number
      str_extract_all(pattern = "\\d+", simplify = TRUE)
    if(length(curr_vid_num)==1){
      fish$video[j]<- as.numeric(curr_vid_num)} else{
        message(paste("Error: Could not extract video number from file name", fish$short_filename[j]))
        stop()}
    rm(j, curr_vid_num)
  }
  fish<- fish %>% mutate(Total_Frame= video*(frames_per_sec*(vid_length*60))+Frame)
  fish<- fish %>% mutate(Total_Time_In= (video*(vid_length*60))+Time_In_Video)
  fish<- fish %>% select(file_name,video,Reviewer,Fish_Number,Fish_Type,Time_In_Video, Frame, Total_Time_In, Total_Frame)
  dup_check<- fish %>% group_by(video, file_name) %>% count()
  if(sum(duplicated(dup_check$video))>0){
    message(paste("Error:", as.character(sum(duplicated(dup_check$video))), "video(s) have multiple associated csvs"))
    stop()}
  if(!is.null(filename)){
    readr::write_csv(fish, file = filename, append = append)
  }
  return(fish)
}

