#' Generates Cruise Files and diagnostic plots
#'
#' Generates 1Hz tables for all transects within a cruise (Ship Speed, Ship position, CBASS layback, CBASS depth, CBASS altitude, CBASS Pitch, CBASS Position) as well as diagnostic plots. Can also be used to generate this for a specific transect rather than full cruise using "CBASS_transect_name" and "CBASS_transect_subdir" arguments. Will also output a file "messages.txt" that logs inputs/warnings/errors. Note: If there is an error and you cannot delete "messages.txt" you may need to run "closeAllConnections()"
#' @param output_dir Directory to output files to
#' @param EK_dir Directory of Ek Ship track file(s). Shiptrack files must be the only files in that directory that end in ".gps.csv"
#' @param CBASS_dir Directory of CBASS transects
#' @param Ship_dir Directory containing "Northstar-941X---GPVTG" file(s) (.lab, .aco, or .Raw)
#' @param winch_dir Directory of winch data or path to Alex #2's payout tsv files
#' @param altimeter_file name of altimeter files in CBASS directory. Default is "altimeter_readings.tsv" (only change if there is a default in original data table)
#' @param ctd_file name of ctd files under CBASS directory. Default is "ctd_readings.tsv" (only change if there is a mistake in default data table)
#' @param compass_file name of compass files under CBASS directory. Default is "compass_readings.tsv" (only change if there is a mistake in original default table)
#' @param zeroed Where winch is zeroed ("water" or "block", default is water)
#' @param GPS_Source Adds offset forward/aft offset between trawl block and GPS source. Must be "None", "CoG" (Center of Gravity), "Furuno" or "Northstar". Default is CoG.
#' @param CBASS_transect_name Name of CBASS transect. Only use if you want to generate files for just one transect.
#' @param CBASS_transect_subdir Name of sub_dir for CBASS transect. Only use if you want to generate files for just one transect.
#' @param alt_pos File, vector of file names, or directory containing alternative positioning source (instead of EK). Currently only files with a basename of "Northstar-941X---GGA.lab", "Northstar-941X---GGA.aco", and Northstar-941X---GGA_*.Raw are supported. If a directory is supplied lab files will be preferentially used, then aco, then Raw.
#' @importFrom lubridate mdy_hms
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate round_date
#' @importFrom lubridate ddays
#' @importFrom lubridate dminutes
#' @importFrom lubridate dseconds
#' @importFrom lubridate dmilliseconds
#' @importFrom lubridate dmicroseconds
#' @importFrom lubridate date
#' @importFrom lubridate with_tz
#' @import dplyr
#' @import readr
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom tidyr separate
#' @importFrom stats median
#' @importFrom writexl write_xlsx
#' @export

generate_cruise_files<- function(output_dir,EK_dir,CBASS_dir, Ship_dir, winch_dir, altimeter_file= "altimeter_readings.tsv", ctd_file= "ctd_readings.tsv",compass_file= "compass_readings.tsv", zeroed = "water", GPS_Source = "CoG", CBASS_transect_name=NULL, CBASS_transect_subdir="", alt_pos=""){
  #Set up messages file (DO NOT EDIT THIS)
  warn_behavior<- getOption("warn")
  options(warn = 1) #Set warnings behavior
  message_file_name<- paste(output_dir,"messages.txt", sep = "/")
  message_file <- file(message_file_name, open="wt")
  sink(message_file, type = "message")
  message(paste("Date and Time:", Sys.time()))
  message("")
  message("Inputs")
  message(paste("output_dir = ", output_dir))
  message(paste("EK_dir = ", EK_dir))
  message(paste("CBASS_dir = ", CBASS_dir))
  message(paste("Ship_dir = ", Ship_dir))
  message(paste("winch_dir = ", winch_dir))
  message(paste("altimeter_file =", altimeter_file))
  message(paste("ctd_file =", ctd_file))
  message(paste("compass_file =", compass_file))
  message(paste("zeroed =", zeroed))
  message(paste("GPS_Source =", GPS_Source))
  message(paste("CBASS_transect_name =", CBASS_transect_name))
  message(paste("CBASS_transect_subdir =", CBASS_transect_subdir))
  message(paste("alt_pos =", alt_pos))
    message("")
  message("Warnings and other messages")

  ####################################################################################
  #Read in EK File(s)
  if(alt_pos==""){
    cruise_tracks<- list.files(path= EK_dir,pattern = "\\.gps\\.csv", ignore.case = FALSE, full.names = TRUE) #List all EK cruise track files
    EK_pos<- read_csv(cruise_tracks[1], col_types = list(.default=col_double(), GPS_date=col_character(),GPS_time=col_character(),GPS_filename=col_character()))
    if (length(cruise_tracks)>1){
      for (r in 2:length(cruise_tracks)){
        curr_cruise_track<- read_csv(cruise_tracks[r], col_types = list(.default=col_double(), GPS_date=col_character(),GPS_time=col_character(),GPS_filename=col_character()))
        EK_pos<- EK_pos %>% bind_rows(curr_cruise_track)
      }}}
  if(alt_pos!=""){
    if(dir.exists(alt_pos)){ #if alt_pos is a directory rather than a file
      alt_pos2<- list.files(alt_pos, full.names = TRUE, pattern = "GGA.*\\.lab$")
      if(length(alt_pos2)==0){
        alt_pos2<- list.files(alt_pos, full.names = TRUE, pattern = "GGA.*\\.aco$")
      }
      if(length(alt_pos2)==0){
        alt_pos2<- list.files(alt_pos, full.names = TRUE, pattern = "GGA.*\\.Raw$")
      }
      alt_pos<- alt_pos2
      }
      EK_pos<- mytools:::NorthstarGGA2EK(alt_pos[1])
      if(length(alt_pos)>1){
        for (s in 2:length(alt_pos)){
          EK_pos<- bind_rows(EK_pos, mytools:::NorthstarGGA2EK(alt_pos[s]))
        }
      }
    EK_pos$GPS_date=as.character(EK_pos$GPS_date)} #Format alt_pos source like EK

  #Read in Ship File
  GPVTG_File_list<- list.files(Ship_dir, pattern = "Northstar-941X---GPVTG.*lab", full.names = TRUE)
  GPVTG_delim<- " "
  if(length(GPVTG_File_list)==0){
    GPVTG_File_list<- list.files(Ship_dir, pattern = "Northstar-941X---GPVTG.*aco", full.names = TRUE)
    GPVTG_delim<- ","
    } #if no lab files use aco files
  Ship_File<- Ship_File<- tibble(timestamp=as.POSIXct(as.character(), tz = "UTC"), Ship_Speed_kph=as.numeric()) #Initialize Ship File dataframe
  if(length(GPVTG_File_list)>0){ #If GPVTG file(s) exist
    for (q in 1:length(GPVTG_File_list)) {
      GPVTG_File<- read_delim(GPVTG_File_list[q], delim = GPVTG_delim, col_names = FALSE, col_types = list(.default=col_double()))
      names(GPVTG_File)[c(1,3,4,7)]<- c("Year", "Julian_Day", "Decimal_Day", "Ship_Speed_kph")
      GPVTG_File<- GPVTG_File %>% mutate(Seconds_In_Day = round((Decimal_Day * 24 *60 * 60),0))
      GPVTG_File<- GPVTG_File %>% mutate(time_origin = lubridate:: mdy_hms(paste0("1-1-",as.character(Year),"-00:00:00"))) #Time orgin for Julian Day
      GPVTG_File<- GPVTG_File %>% mutate(timestamp = time_origin+lubridate::ddays(Julian_Day-1)+lubridate::dseconds(Seconds_In_Day))
      GPVTG_File<- GPVTG_File %>% select(timestamp, Ship_Speed_kph)
      Ship_File<- bind_rows(Ship_File, GPVTG_File)
    }}
  if(length(GPVTG_File_list)==0){
    Raw_File_list<- list.files(Ship_dir, pattern = "Northstar-941X---GPVTG.*Raw", full.names = TRUE)
    for (p in 1:length(Raw_File_list)) {
      Raw_File<- read_csv(Raw_File_list[p], col_names = FALSE, col_types = list(.default = col_character()))
      names(Raw_File)[c(1,2,10)]<- c("Date", "Time", "Ship_Speed_kph")
      Raw_File$Ship_Speed_kph<- as.numeric(Raw_File$Ship_Speed_kph)
      Raw_File<- Raw_File %>% separate(Time, into = c("Time1","Time2"), sep = "\\.")
      Raw_File<- Raw_File %>% mutate(decimal_secs = as.numeric(paste0("0.", Time2)))
      Raw_File<- Raw_File %>% mutate(timestamp = lubridate:: mdy_hms(paste(Date, Time1, sep = "_")) + lubridate::dseconds(round(decimal_secs, digits = 0)))
      Raw_File<- Raw_File %>% select(timestamp, Ship_Speed_kph)
      Ship_File<- bind_rows(Ship_File, Raw_File)
    }}

  #Edit Ship File
  Ship_File<- Ship_File %>% mutate(Ship_Speed_mps= Ship_Speed_kph *(1000/(60*60)))
  Ship_File<- Ship_File %>% select(timestamp, Ship_Speed_mps)
  Ship_File<- Ship_File %>% filter(Ship_Speed_mps < 10) #remove any bad readings (and NA's) by setting upper threshold
  Ship_File<- Ship_File[!duplicated(Ship_File$timestamp),] #Only retain first timestamp if there are duplicates

  ##################################################################
  #Edit EK File
  EK_pos<- EK_pos %>% mutate(timestamp=lubridate::round_date(lubridate::ymd_hms(paste(GPS_date, GPS_time, sep="_")) + lubridate::dmilliseconds(GPS_milliseconds), unit = "second")) #Round timestamps to nearest second
  EK_pos<- EK_pos[!duplicated(EK_pos$timestamp),] #Only retain first timestamp if there are duplicates
  #################################################################################################
  #Make EK and Shipfile at exactly 1Hz
  EK_pos2<- data.frame(timestamp = seq.POSIXt(from = min(EK_pos$timestamp, na.rm = TRUE), max(EK_pos$timestamp, na.rm=TRUE), by="sec")) #Create 1Hz Table
  EK_pos2<- EK_pos2 %>% left_join(EK_pos, by="timestamp")
  EK_pos2<- EK_pos2 %>% mutate(Latitude_interp = interp(EK_pos2$Latitude, na.rm=TRUE)) #Interp Lat
  EK_pos2<- EK_pos2 %>% mutate(Longitude_interp = interp(EK_pos2$Longitude, na.rm=TRUE)) #Interp Lon

  Ship_File2<- data.frame(timestamp = seq.POSIXt(from = min(Ship_File$timestamp, na.rm=TRUE), to = max(Ship_File$timestamp, na.rm=TRUE), by="sec")) #Create 1Hz Table
  Ship_File2<- Ship_File2 %>% left_join(Ship_File, by="timestamp")
  ##############################################################################################
  #Get Payout
  if(grepl(pattern = "\\.tsv$", x = winch_dir)){ #Get payout from Alex #2 table
    payout<- read_tsv(winch_dir, col_types = list(.default=col_double(), timestamp= col_datetime()))
    payout<- payout %>%
      mutate(timestamp= lubridate::round_date(timestamp + lubridate::dmicroseconds(u_second), unit = "second"))
    payout<- payout %>% select(timestamp, payout)
    payout<- payout %>% group_by(timestamp) %>%
      summarise(Payout_m = stats::median(payout, na.rm=TRUE)) %>%
      ungroup()
  } else{
    payout<- get_payout(winch_dir = winch_dir, trim_rows = TRUE)} #Get Payout from winch data
  ##############################################################################################
  #Get list of CBASS Transects
  if (!is.null(CBASS_transect_name)){ #If hust one transect
    CBASS_transects<-CBASS_transect_name #Set to directory specified
    sub_dirs<- CBASS_transect_subdir
  } else{ #If whole cruise
    #Get list of CBASS transect folders
    CBASS_transects<- list.files(CBASS_dir) #Get all CBASS Transect Names from Cruise
    CBASS_transects<- CBASS_transects[!grepl(pattern = "\\.", CBASS_transects)] #Removes non transect files (e.g. pdf files) from transect list
    CBASS_transects<- CBASS_transects[!grepl(pattern = "Sensor Excel", CBASS_transects)] #Remove Sensor Excel File from transect list
  }

  #Loop through directories, match tables, and generate files
  for (i in 1:length(CBASS_transects)) { #Loop through transect directories
    transect_dir<- paste(CBASS_dir, CBASS_transects[i], "tables", sep="/") #Directory of transect
    if (is.null(CBASS_transect_name)){  #If just one transect
      sub_dirs<- list.files(transect_dir) #Subsirectories within transect directory
      sub_dirs<- sub_dirs[!grepl(pattern = "\\.", sub_dirs)]} #Remove stray files
    if(length(sub_dirs)==0){sub_dirs=""} #Deal with no subdirs
    for (j in 1:length(sub_dirs)) { #Loop through subdirectories
      curr_dir<- paste(transect_dir, sub_dirs[j], sep="/") #Current directory
      message(paste0("Begin ", CBASS_transects[i], "_", sub_dirs[j]))
      check_val<- sum(grepl(pattern = altimeter_file, x = list.files(curr_dir)))>0 &
        sum(grepl(pattern = ctd_file, x = list.files(curr_dir)))>0 &
        sum(grepl(pattern = compass_file, x = list.files(curr_dir)))>0
      if(check_val==FALSE){ #Check necessary files exist
        warning(paste(curr_dir, "does not have necessary files"))
        next} #Advance to next iteration if doesn't have necessary files
      alt<- read_tsv(paste(curr_dir, altimeter_file, sep="/"),col_types = list(.default = col_double(),timestamp = col_datetime())) #read in altimeter data
      ctd<- read_tsv(paste(curr_dir, ctd_file, sep="/"), col_types = list(.default= col_double(),timestamp=col_datetime())) #Read in ctd data
      compass<- read_tsv(paste(curr_dir, compass_file, sep="/"), col_types = list(.default=col_double(), timestamp=col_datetime())) #Read in compass data
      if (nrow(alt)==0 | nrow(ctd)==0 | nrow(compass)==0){
        warning(paste(curr_dir, "has empty CBASS tables"))
        next} #Advance to next iteration if CBASS tables are unpopulated
      alt<- alt %>%
        mutate(timestamp = lubridate::round_date(timestamp+lubridate::dmicroseconds(u_second), unit = "second"))
      ctd<- ctd %>%
        mutate(timestamp = lubridate::round_date(timestamp+lubridate::dmicroseconds(u_second), unit = "second"))
      compass<- compass %>%
        mutate(timestamp = lubridate::round_date(timestamp+lubridate::dmicroseconds(u_second), unit = "second"))#Round to nearest second

      alt<- alt %>%
        group_by(timestamp) %>%
        summarise(altitude= stats::median(altitude, na.rm = TRUE)) %>%
        ungroup() #Get median value for each timestamp
      ctd<- ctd %>%
        group_by(timestamp) %>%
        summarise(depth= stats::median(depth, na.rm = TRUE)) %>%
        ungroup() #Get median value for each timestamp
      compass<- compass %>%
        group_by(timestamp) %>%
        summarise(pitch= stats::median(pitch, na.rm = TRUE)) %>%
        ungroup() #Get median value for each timestamp

      st_time<- min(with_tz(c(alt$timestamp, ctd$timestamp, compass$timestamp),"UTC"), na.rm = TRUE) #Get Transect Start Time
      st_time<- st_time - lubridate::dminutes(3) #Add 3 minute buffer
      end_time<- max(with_tz(c(alt$timestamp, ctd$timestamp, compass$timestamp),"UTC"), na.rm=TRUE) #Get Transect end Time
      transect_df<- data.frame(timestamp= seq.POSIXt(from = st_time, to = end_time, by = "sec")) #Create 1Hz table for transect

      transect_df<- transect_df %>%
        left_join(alt, by="timestamp") %>%
        left_join(ctd, by="timestamp") %>%
        left_join(compass, by="timestamp") %>%
        left_join(EK_pos2, by= "timestamp") %>%
        left_join(payout, by ="timestamp") %>%
        left_join(Ship_File2, by="timestamp") #Join tables to transect_df

      transect_df <- transect_df %>% mutate(Ship_Speed_mps_1minAvg=NA_real_)
      for (m in 60:nrow(transect_df)) {
        idx_speed<- seq(from = m-59, to = m, by = 1)
        transect_df$Ship_Speed_mps_1minAvg[m]<- mean(transect_df$Ship_Speed_mps[idx_speed], na.rm=TRUE)
        } #Calculate 1 minute average (one-sided backwards) of Ship Speed
      transect_df<- transect_df %>%
        select(timestamp, pitch, altitude, depth, Latitude, Longitude, Latitude_interp, Longitude_interp, Payout_m, Ship_Speed_mps, Ship_Speed_mps_1minAvg)
      names(transect_df)[2:8]<- c("CBASS_Pitch", "CBASS_Alt", "CBASS_Depth", "Ship_Lat", "Ship_Lon", "Ship_Lat_Interp", "Ship_Lon_Interp")
      transect_df$CBASS_Pitch<- interp(transect_df$CBASS_Pitch, na.rm = TRUE) #interpolate to fill gaps
      transect_df$CBASS_Alt<- interp(transect_df$CBASS_Alt, na.rm = TRUE)
      transect_df$CBASS_Depth<- interp(transect_df$CBASS_Depth, na.rm = TRUE)
      transect_df$Payout_m<- interp(transect_df$Payout_m, na.rm = TRUE) #interpolate to fill gaps
      transect_df$CBASS_Pitch[180]<- NA #Preserve 3 min buffer (filled by interp)
      transect_df$CBASS_Alt[180]<- NA
      transect_df$CBASS_Depth[180]<- NA



      transect_df<- transect_df %>% mutate(k1_Layback_m = calc_layback(payout = Payout_m, depth = CBASS_Depth, GPS_Source = GPS_Source, zeroed = zeroed, cat_fact = 1))
      if(length(which(is.nan(transect_df$k1_Layback_m)))>0){
        warning(paste("Depth may exceed payout. Nan's produced in Layback_m for", curr_dir))}
      transect_df<- transect_df %>% mutate(k1_Layback_sec = k1_Layback_m/Ship_Speed_mps_1minAvg)
      transect_df<- transect_df %>% mutate(Time_To_Match= lubridate::round_date(timestamp - lubridate::dseconds(k1_Layback_sec), unit = "second"))
      transect_df<- transect_df %>% mutate(k1_CBASS_Lat=NA_real_) %>% mutate(k1_CBASS_Lon=NA_real_)
      for (k in 1:nrow(transect_df)) {
        idx<- which(EK_pos2$timestamp == transect_df$Time_To_Match[k])
        if (length(idx)>0){
          transect_df$k1_CBASS_Lat[k]<- EK_pos2$Latitude_interp[idx]
          transect_df$k1_CBASS_Lon[k]<- EK_pos2$Longitude_interp[idx]
        }} #Get Ship Position lagged by layback time
      transect_df<- transect_df %>% select(-Time_To_Match)
      output_filename<- paste0(CBASS_transects[i], "_", sub_dirs[j])
      start_date<- as.character(lubridate::date(transect_df$timestamp[1]))
      my_data<- transect_df %>% select(timestamp, Payout_m, CBASS_Depth, Ship_Speed_mps, Ship_Speed_mps_1minAvg) %>% gather(key = "type", value = "value", -timestamp) #Format data for plot
      depth_payout_idx<- my_data$type=="CBASS_Depth" | my_data$type=="Payout_m"
      my_data$value[depth_payout_idx]<- -1* my_data$value[depth_payout_idx]
      my_data<- my_data %>% mutate(my_facets= "Ship_Speed_mps")
      my_data$my_facets[depth_payout_idx]<- "CBASS Depth (m) and Payout (m)"
      my_data$my_facets<- factor(my_data$my_facets, levels = c("CBASS Depth (m) and Payout (m)", "Ship_Speed_mps"))
      my_data$type<- factor(my_data$type, levels = c("CBASS_Depth", "Payout_m", "Ship_Speed_mps", "Ship_Speed_mps_1minAvg"))
      my_plot<- ggplot(data = my_data, mapping = aes(x = timestamp, y= value, color = type))+
        geom_point(na.rm = TRUE, size=1)+
        scale_color_manual(values = c("blue", "orange", "black", "red"))+
        xlab(start_date)+
        facet_wrap(facets = "my_facets", ncol = 1, scales = "free_y")+
        ggtitle(label = output_filename)
      write_xlsx(x = transect_df,path = paste0(output_dir, "/", output_filename, ".xlsx"))
      ggsave(plot = my_plot, device = "png", filename = paste0(output_filename, ".png"), path = output_dir, width=16, height = 9, units = "in", dpi = 300)
      message("")
    }}
  options(warn = warn_behavior) #Reset warning behavior
  print(readLines(message_file_name))
  closeAllConnections() #Close connection to message file
}
