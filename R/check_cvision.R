#' Check if any C-Vision csv's are missing
#'
#' Check if any C-Vision csv's are missing
#' @param expected vector of expected video numbers
#' @param file_list list of cvision csv files. If left blank will use all csvs in working directory
#' @export
#' @importFrom stringr str_extract
#' @import dplyr

check_cvision<- function(expected, file_list=list.files(pattern = ".csv$")){
  short_filename<- rep(NA_character_, length(file_list))
  for (i in 1:length(file_list)) {
    idx<- gregexpr("\\/", file_list[i])[[1]]
    start_pos<- idx[length(idx)]+1
    end_pos<- nchar(file_list[i])
    short_filename[i]<-substr(x = file_list[i], start = start_pos, stop=end_pos)
    }
  num_list<- as.integer(str_extract(string = short_filename, pattern = "\\d+"))
  idx<- !(expected %in% num_list)
  if(sum(idx)==0){message("All files are present")} else{
  missing_vids<- expected[idx]
  for (i in 1:length(missing_vids)) {
    message(paste("Missing video", as.character(missing_vids[i])))
  }}
  check_dups<- tibble(num_list=num_list) %>% group_by(num_list) %>% count() %>% ungroup
  idx<- check_dups$n>1
  if(sum(idx)==0){message("No multiples of same video")} else{
    dup_vids<- check_dups$num_list[idx]
    for(i in 1:length(dup_vids)) {
      message(paste("Multiple of video", as.character(dup_vids[i])))
    }}
}
