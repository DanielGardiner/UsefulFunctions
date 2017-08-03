

#################################################
# function to convert dates into useful date intervals 
# day, year, quarter, month, year.quarter, year.month, iso.year, iso.week, iso.year.week
#
#
# Author: Daniel Gardiner
# Contact: daniel.gardiner@phe.gov.uk
# date: 26.03.2015
#
# Arguments: 
# x: vector of dates
#
# outputs: a dataframe including day, year, quarter, month, year.quarter, 
#         year.month, iso.year, iso.week, iso.year.week



get.dates = function(x){  
  if(class(x) == "Date"){
    NULL
  } else{
    stop("x is not a date")
  } 
  library(ISOweek)
  df = data.frame(day = as.character(x),
                  year = format(x, "%Y"), 
                  month = format(x, "%m"))
  df$year.month = paste0(df$year, df$month)
  df$iso.year = sapply(strsplit(ISOweek(x), "-W"), function(x) x[1])
  df$iso.week = sapply(strsplit(ISOweek(x), "-W"), function(x) x[2])
  df$iso.year.week = gsub("-W", "", ISOweek(x))
  df$quarter = NA
  df$quarter[!is.na(df$day)] = sprintf("%02d", ceiling(as.numeric(as.character(df$month[!is.na(df$day)]))/3))
  df$year.quarter = paste0(df$year, df$quarter)
  df[is.na(df$day), ] = NA
  df  
}



#################################################
# EXAMPLE
#
#dates <- c(as.Date(paste(1999:2011, "-12-31", sep = "")), NA)
#
#data.frame(dates = dates, get.dates(dates))

