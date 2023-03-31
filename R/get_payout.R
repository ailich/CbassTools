#' Gets Payout data in single table from winch data folder
#'
#' Gets Payout data in single table from winch data folder.
#' @param winch_dir directory containing winch data
#' @param trim_rows A logical that if TRUE (default), will take the median of all values with the same timestamp so that each timestamp has one value
#' @import dplyr
#' @importFrom lubridate mdy_hms
#' @importFrom readr read_csv
#' @importFrom stats median
#' @export

get_payout<- function(winch_dir, trim_rows=TRUE){
  file_list<- list.files(path = winch_dir, pattern = "\\.csv$", full.names = TRUE) #List files
  winch_acc<- tibble(timestamp = NA, Payout_m =NA)[0,] #Create Accumulator Variable
  for (i in 1:length(file_list)) {
    winch_df<- suppressMessages(read_csv(file_list[i], skip = 11, col_names = FALSE)) #Read in relevant rows of data
    if (nrow(winch_df) > 0) {
      winch_names<- read.csv(file_list[i], skip = 7, header = FALSE,nrows=3, colClasses = "character")
      if(!(winch_names$V2[3]=="Clock" & grepl(pattern = "Block", x = winch_names$V14[1], ignore.case = TRUE) & winch_names$V14[3]=="Payout"))
        {stop("Winch data not formatted as expected")} #Ensure winch data formatted as expected
      winch_df<- winch_df[,c(2,14)] #Select Relevant Columns
      names(winch_df)<- c("timestamp", "Payout_m") #Name Columns
      winch_acc<- rbind(winch_acc, winch_df) #Append to accumulator
    }
  }
  if (class(winch_acc$Payout_m)!="numeric"){
    winch_acc$Payout_m<- as.numeric(winch_acc$Payout_m)} #ensure payout is a numeric class
  if (trim_rows){
    winch_acc<- winch_acc %>% group_by(timestamp) %>% summarize(Payout_m= median(Payout_m, na.rm=TRUE)) %>% ungroup()} #Average readings by timestamp
  winch_acc<- winch_acc %>% mutate(timestamp=lubridate::mdy_hms(timestamp)) %>% arrange(timestamp) %>% select(timestamp, Payout_m) #Sort rows by timestamp and order columns
  return(winch_acc)}
