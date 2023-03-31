#' Calculates layback of CBASS
#'
#' Calculates layback of CBASS in meters
#'
#' @param payout line out from boat (meters)
#' @param depth depth of CBASS (meters)
#' @param zeroed "water" or "block"
#' @param cat_fact catenary factor (from 0-1)
#' @param GPS_Source Adds offset forward/aft offset between trawl block and GPS source. Must be "None", "CoG" (Center of Gravity), "Furuno" or "Northstar"
#' @export

calc_layback<- function(payout, depth, GPS_Source= "None", zeroed, cat_fact=1){
  if (GPS_Source != "Northstar" & GPS_Source != "None" & GPS_Source != "CoG" & GPS_Source != "Furuno"){stop('GPS_Source must be "None" "CoG" or "Northstar" or "Furuno"' )}
  if (zeroed != "water" & zeroed != "block"){stop('zeroed must be "water" or "block"' )}
  if (cat_fact > 1 | cat_fact <=0){stop("catenary factor must be between 0-1" )}
  Block_height<- 6.065 #Height of trawl block above water
  if (zeroed == "water"){payout<- payout + Block_height} #Approximately this much line would be let out to reach water
  if (GPS_Source == "None") {y_offset<- 0} #No offset
  if (GPS_Source == "Northstar") {y_offset<-  ((164.24-100)/3.281) - (-4.846)} #Forward/Aft offset between Northstar antenna (pt 5005 on schematic) and trawl block
  if (GPS_Source == "Furuno") {y_offset<-  ((168.7-100)/3.281) - (-4.846)} #Forward/Aft offset between Furuno antenna (pt 5013 on schematic) and trawl block
  if (GPS_Source == "CoG") {y_offset<- 16.206 - (-4.846)}
  neg_idx<- which(payout< 0)
  if(length(neg_idx)>0){
    warning("Some of adjuted payout is negative. Replacing with NA's")
    payout[neg_idx]<- NA
  }
  c2<- (cat_fact * payout)^2
  b2<- (depth + Block_height)^2
  a<- sqrt(c2-b2) #Pythagorean
  layback<- y_offset + a #Add y offset to get layback
  return(layback)
}

