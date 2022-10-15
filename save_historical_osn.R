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

## concatenate everything available
concatFiles(dir = "data", reg = "^bl_dep_\\d+",
              col_spec = cols(
                ICAO24 = col_character(),
                call_sign = col_character(),
                departure_time = col_datetime(format = ""),
                departure_date = col_date(format = ""),
                arrival_time = col_datetime(format = ""),
                arrival_date = col_date(format = ""),
                departure_airport_ICAO = col_character(),
                destination_airport_ICAO = col_character(),
                id = col_character())
) %>% 
  arrange(departure_time) %>% 
  write_csv("data/bl_dep_all.csv")

concatFiles(dir = "data", reg = "^bl_arr_\\d+",
            col_spec = cols(
              ICAO24 = col_character(),
              call_sign = col_character(),
              departure_time = col_datetime(format = ""),
              departure_date = col_date(format = ""),
              arrival_time = col_datetime(format = ""),
              arrival_date = col_date(format = ""),
              departure_airport_ICAO = col_character(),
              destination_airport_ICAO = col_character(),
              id = col_character())
) %>% 
  arrange(departure_time) %>% 
  write_csv("data/bl_arr_all.csv")

# STATE VECTORS
# Take the last 4 months
q_y_m <- seq(Sys.Date () -30 * 4, Sys.Date(), 1) %>% 
  format("%Y_%m") %>% unique()

concatFiles("data", 
            str_c("^bl_dep_SV_(", str_c(q_y_m, collapse = "|"), ")"), 
            col_spec = cols(
  ICAO24 = col_character(),
  longitude = col_double(),
  latitude = col_double(),
  requested_time = col_double(),
  geo_altitude = col_double(),
  velocity = col_double(),
  special_purpose_indicator = col_logical(),
  origin_country = col_logical(),
  id = col_character(),
  arrival_date = col_date(format = ""),
  departure_date = col_date(format = "")
)) %>% 
  arrange(departure_date) %>% 
  write_csv("data/bl_dep_SV_all.csv")


concatFiles("data", 
            str_c("^bl_arr_SV_(", str_c(q_y_m, collapse = "|"), ")"), 
            col_spec = cols(
              ICAO24 = col_character(),
              longitude = col_double(),
              latitude = col_double(),
              requested_time = col_double(),
              geo_altitude = col_double(),
              velocity = col_double(),
              special_purpose_indicator = col_logical(),
              origin_country = col_logical(),
              id = col_character(),
              arrival_date = col_date(format = ""),
              departure_date = col_date(format = "")
            )) %>% 
  arrange(arrival_date) %>% 
  write_csv("data/bl_arr_SV_all.csv")





