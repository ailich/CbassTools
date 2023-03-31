#' Collapses merged C-vision csv to time bins
#'
#' Collapses merged C-vision csv to time bins
#' @param merged_cvision_csv output of merge_cvision_csv (as a dataframe or tibble)
#' @param start_vid video to start output table with
#' @param end_vid video to end output table with
#' @param bin_size length of bins in seconds
#' @param vid_length length of video in minutes
#' @import dplyr
#' @export

bin_cvision<-function(merged_cvision_csv, start_vid, end_vid, bin_size=15, vid_length=1){
  vid_length<- vid_length*60 #convert to seconds
  if ((vid_length %% bin_size)!=0) {
    message("Error: Video Length is not divisible by bin size")
    stop()
  }
  #Make Tidy by returning a datframe or tibble depending on input
  fish_names<- sort_species(unique(merged_cvision_csv$Fish_Type))
  n_videos<- (end_vid-start_vid)+1
  vid_samp<- vid_length/bin_size #Number of rows per video
  bin_labels<- rep(NA, vid_samp)
  for (i in 1:vid_samp) {
    bin_labels[i]<- bin_size*(i-1)
  }
  new_df<- matrix(data = NA_integer_, nrow = vid_samp*n_videos, ncol=length(fish_names)+2, dimnames = list(c(),c("Video", "Seconds", fish_names))) %>% as_tibble() #Species by Site Matrix
  new_df$Video<- as.vector(sapply(start_vid:end_vid, rep, vid_samp))
  new_df$Seconds<- rep(bin_labels, nrow(new_df)/length(bin_labels))
  bin_ranges<-tibble(lower=bin_labels, upper=bin_labels+bin_size)
  merged_cvision_csv$bin<- NA #Add a bin column
  for (i in 1:nrow(merged_cvision_csv)) {
    idx<-merged_cvision_csv$Time_In_Video[i]>=bin_ranges$lower & merged_cvision_csv$Time_In_Video[i]<bin_ranges$upper
    merged_cvision_csv$bin[i]<- bin_labels[idx]
  } #Place each observation in a bin
  fish_counts<- merged_cvision_csv %>% group_by(video,bin) %>% count(Fish_Type) %>% ungroup()
  for (i in 1:nrow(fish_counts)) {
    r_idx<- which(new_df$Video==fish_counts$video[i] & new_df$Seconds==fish_counts$bin[i]) #row index
    c_idx<- which(names(new_df)==fish_counts$Fish_Type[i]) #Col index
    new_df[r_idx,c_idx]<- fish_counts$n[i] #Place count in proper part of dataframe
  }
  new_df[is.na(new_df)]<- 0 #Replace NA's with 0
  return(new_df)
}
