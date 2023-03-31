#' Reformat postion from Northstar-941X--GGA.lab files to look like those from an EK cruise track csv
#' @param file_name path to Northstar-941X--GGA.lab or Northstar-941X--GGA.aco file or Northstar-941X---GGA_*.Raw
#' @import dplyr
#' @importFrom readr read_delim
#' @importFrom lubridate ymd
#' @importFrom lubridate ddays
#' @importFrom lubridate dseconds
#' @importFrom readr read_csv
#' @importFrom readr col_character

NorthstarGGA2EK<-function(file_name){
  if(basename(file_name)=="Northstar-941X---GGA.lab" | basename(file_name)=="Northstar-941X---GGA.aco"){
    if(basename(file_name)=="Northstar-941X---GGA.lab"){delim<- " "}
    if(basename(file_name)=="Northstar-941X---GGA.aco"){delim<- ","}
    col_types<- paste0(strrep("d", 6), "c", strrep("d", 6), "c")
    GGA<- readr::read_delim(file_name, delim = delim, col_names = FALSE, col_types = col_types)
    names(GGA)[c(1,3, 4, 9, 10)] <- c("Year", "Day", "Decimal_Day", "Latitude", "Longitude")
    GGA<- GGA[,!grepl(pattern = "X\\d", names(GGA))]
    GGA<- GGA %>% mutate(time_origin= lubridate::ymd(paste(as.character(Year), "-1-1")))
    GGA<- GGA %>% mutate(GPS_date=time_origin+lubridate::ddays(Day-1))
    GGA<- GGA %>% mutate(seconds_in_day=Decimal_Day*24*60*60)
    GGA<- GGA %>% mutate(GPS_milliseconds=(seconds_in_day-floor(seconds_in_day))*1000)
    GGA<- GGA %>% mutate(datetime=GPS_date+lubridate::dseconds(floor(seconds_in_day)))
    GGA<- GGA %>% mutate(GPS_time= strftime(datetime,format="%H:%M:%S", tz="UTC"))
    output<- GGA %>% select(GPS_date, GPS_time, GPS_milliseconds, Latitude, Longitude)
    output<- output %>% mutate(GPS_fix= 0:(nrow(output)-1), GPS_status=0, GPS_filename=file_name, Distance=NaN)
    output<- output %>% select(GPS_fix, GPS_date, GPS_time, GPS_milliseconds, Latitude, Longitude, GPS_status, GPS_filename, Distance)} else if(grepl(pattern= "Northstar-941X---GGA_.*\\.Raw$", file_name)){
      col_types<- "cccdccccddddcccdc"
      GGA<- readr::read_csv(file_name, col_names = FALSE, col_types = list(.default=readr::col_character()))
      names(GGA)[c(1,2,5,6,7,8)]<- c("GGA_Date", "GGA_time","GGA_Latitude", "NS", "GGA_Longitude", "EW")

      GGA$GGA_Date[nchar(GGA$GGA_Date)!=10]<-NA
      GGA$GGA_Date[!grepl(pattern = "\\d{2}/\\d{2}/\\d{4}", GGA$GGA_Date)]<- NA

      GGA$GGA_time[nchar(GGA$GGA_time)!=12]<-NA
      GGA$GGA_time[!grepl(pattern = "\\d{2}:\\d{2}:\\d{2}\\.\\d{3}", GGA$GGA_time)]<- NA

      GGA$GGA_Latitude[nchar(GGA$GGA_Latitude)!=9]<-NA
      GGA$GGA_Latitude[!grepl(pattern = "\\d{4}\\.\\d{4}", GGA$GGA_Latitude)]<- NA

      GGA$GGA_Longitude[nchar(GGA$GGA_Longitude)!=10]<-NA
      GGA$GGA_Longitude[!grepl(pattern = "\\d{5}\\.\\d{4}", GGA$GGA_Longitude)]<- NA

      GGA$NS[GGA$NS!="N" & GGA$NS!="S"]<- NA
      GGA$EW[GGA$EW!="E" & GGA$EW!="W"]<- NA #Replace any wrong values with NA
      GGA$NS[GGA$NS=="N"]<-"1"
      GGA$NS[GGA$NS=="S"]<- "-1"
      GGA$NS<- as.numeric(GGA$NS)
      GGA$EW[GGA$EW=="E"]<-"1"
      GGA$EW[GGA$EW=="W"]<- "-1"
      GGA$EW<- as.numeric(GGA$EW)

      GGA<- GGA %>% mutate(GPS_date= paste(substr(GGA_Date, 7,10), substr(GGA_Date, 1,2), substr(GGA_Date, 4,5), sep="-"))
      GGA<- GGA %>% mutate(GPS_time= substr(GGA_time, 1,8))
      GGA<- GGA %>% mutate(GPS_milliseconds= as.numeric(substr(GGA_time, 9,12))*1000)
      GGA<- GGA %>% mutate(Latitude= (as.numeric(substr(GGA_Latitude,1,2)) + (as.numeric(substr(GGA_Latitude,3,9))/60))*NS)
      GGA<- GGA %>% mutate(Longitude= (as.numeric(substr(GGA_Longitude,1,3)) + (as.numeric(substr(GGA_Longitude,4,10))/60))*EW)
      output<- GGA %>% select(GPS_date, GPS_time, GPS_milliseconds, Latitude, Longitude)
      output<- output %>% mutate(GPS_fix= 0:(nrow(output)-1), GPS_status=0, GPS_filename=file_name, Distance=NaN)
      output<- output %>% select(GPS_fix, GPS_date, GPS_time, GPS_milliseconds, Latitude, Longitude, GPS_status, GPS_filename, Distance)
    } else{message("filename not supported by NorthstarGGA2EK function")
      stop()}
  return(output)}
