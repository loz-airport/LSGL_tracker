source("_helper.R")

# SETTINGS
verbose <- T

# tz  <- "Europe/Zurich"
# timeRes <- 180
# timeOut <- 7


nStartday <- 30 #32
end <- str_c(Sys.Date()-1, " 23:59:59")
start <- str_c(Sys.Date() - nStartday, " 00:00:00")

qDates <- seq(as.Date(start), as.Date(end), 1)


missingDates <- DatesMissingData(qDates)

if(length(missingDates) > 0) {
  startEnd_df <- 1:length(missingDates) %>% 
    map_df(function(dd) 
      tibble(
        start = str_c(missingDates[dd], " 00:00:00"),
        end = str_c(missingDates[dd], " 23:59:59")  
        ))
  
  if(verbose) cat("\nAbout to fetch data for these ", length(missingDates),
                  " days: ",
                  as.character(missingDates))
  
  ## main
  1:nrow(startEnd_df) %>% 
    map(function(ii) {
      getSaveArrivalDeparture(
        start = startEnd_df$start[ii], 
        end = startEnd_df$end[ii], 
        verbose = verbose)      
    })

}