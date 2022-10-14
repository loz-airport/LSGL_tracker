library(tidyverse)
library(purrr)
library(openSkies)
library(lubridate)

usr <-  Sys.getenv("usr_osn")
pwd <-  Sys.getenv("pwd_osn")


get_osf_details <- function(ii, timez = "CET") {
  tibble(
    ICAO24 = ii$ICAO24,
    call_sign = ii$call_sign,
    departure_time = with_tz(ii$departure_time, timez),
    departure_date = as.Date(departure_time,tz = timez),
    arrival_time = with_tz(ii$arrival_time, timez),
    arrival_date = as.Date(arrival_time,tz = timez),
    departure_airport_ICAO = ii$origin_airport,
    destination_airport_ICAO = ii$destination_airport,
    id = str_c(ICAO24, "_", departure_time)
  )
}

os_airport2df <- function(airl) {
  1:length(airl) %>%
    map_df(function(iii) {
      get_osf_details(airl[[iii]])
    }) %>% 
    arrange(departure_time)
}


os_aircraft_SV_2df <- function(aircr, timez = "CET") {
  1:length(aircr) %>%
    map_df(function(ii) {
      
      if(!is.null(aircr[[ii]])) {
        aircr[[ii]]$get_values(
          c("ICAO24",
            "longitude", "latitude", "requested_time", 
            "geo_altitude", "velocity", 
            "special_purpose_indicator", "origin_country")) %>% 
          as_tibble() %>% 
          mutate(idx = ii)
      } else {
        NULL
      }
    })
}


# 
# ldf <- bl_dep_df %>% 
#   group_split(departure_date)
# base_file_name <- "bl_dep"
# output_dir <- "data"
# ii <- 1 

# Take a list data.frame, split by date & save them by date
ldf2files <- function(
  ldf, 
  base_file_name ="bl_dep",
  output_dir = "data",
  verbose = F
  ) {
  if(length(ldf) > 0) {
    1:length(ldf) %>% 
      map(function(ii) {
        
        tdate <- ldf[[ii]]$departure_date %>% unique() %>% max()
        if(length(tdate) > 1) {
          warning ("\nThis chunk has different dates!\n")
        }
        
        out_file <- str_c(output_dir, "/", base_file_name, "_",
                          format(tdate, "%Y_%m_%d"), ".csv")
        if(file.exists(out_file)) {
          tmp_read <- read_csv(out_file, show_col_types = F) 
          tmp_nrow <- tmp_read %>% nrow()
        } else {
          tmp_nrow <- 0
        }
        if(nrow(ldf[[ii]]) > tmp_nrow) {
          if(verbose) cat(
            "\n nrow(ldf[[ii]]: ", nrow(ldf[[ii]]), " > ", 
            tmp_nrow, " (tmp_nrow)")
          
          ldf[[ii]] %>% 
            write_csv(out_file)
        } else {
          if(verbose) cat("\n tmp_nrow:", tmp_nrow)
        } 
      })
  }
}

checkFileData <- function(out_file, nrow_threshold = 1) {
  tmp_nrow <- 0
  if(file.exists(out_file)) {
    tmp_read <- read_csv(out_file, show_col_types = F) 
    tmp_nrow <- tmp_read %>% nrow()
  } else {
    tmp_nrow <- 0
  }
  if(tmp_nrow > nrow_threshold) {
    T
  } else {
    F
  }
}

DatesMissingData <- function(
  qDates, 
  base_file_name = c("bl_dep", "bl_arr", "bl_dep_SV", "bl_arr_SV"),
  output_dir = "data",
  verbose = F
) {
  stopifnot(is.Date(qDates))
  
  out_files <- qDates %>% 
    map_df(function(dd) {
      out_files <- str_c(output_dir, "/", base_file_name, "_",
            format(dd, "%Y_%m_%d"), ".csv")
      tibble(
        date = dd,
        dep = out_files[1], arr = out_files[2],
        depSV = out_files[3], arrSV = out_files[4])
    }) 
  out_check <- 1:nrow(out_files) %>% 
    map_lgl(function(ii) {
      out_files %>% 
        slice(ii) %>% 
        select(-date) %>% 
        unlist() %>% map_lgl(checkFileData) %>% 
        all()
    }) 
  
  qDates[!out_check]
}


## main wrapper get and save arrival/departurs

getSaveArrivalDeparture <- function(
  start, 
  end,
  airport = "LSGL",
  tz  = "Europe/Zurich",
  timeRes = 180,
  timeOut = 7,
  verbose = F
) {

  stopifnot(exists("usr"))
  stopifnot(exists("pwd"))
  
  # Get airport arrivals/departures without state vectors 
  # Get stateVectors separately

  if(verbose) cat("\n\nGET ",  airport, " stuff from ", start, " until ", end)  
  if(verbose) cat("\nGet ",  airport, " departures")
  ### Departures
  bl_dep<- getAirportDepartures(
    airport=airport,
    endTime=end,
    startTime=start,
    timeZone = tz,
    username = usr,
    password = pwd
  )
  if(!is.null(bl_dep) & length(bl_dep) > 0) {
    bl_dep_df <- os_airport2df(bl_dep)
    # SV
    if(verbose) cat("\n\tGet for these departures the corresponding state vectors")
    bl_dep_sv_l <- 1:nrow(bl_dep_df) %>% 
      map(function(ii) {
        if(verbose) cat("\tFetching SV for ", ii, "/", nrow(bl_dep_df))
        getAircraftStateVectorsSeries(
          bl_dep_df$ICAO24[ii],
          bl_dep_df$departure_time[ii],
          bl_dep_df$arrival_time[ii],
          timeZone = tz,
          timeResolution = timeRes,
          username = usr,
          password = pwd,
          useImpalaShell = T,
          timeOut = timeOut,
          maxQueryAttempts = 2
        ) 
      })
    bl_dep_sv_df <- os_aircraft_SV_2df(bl_dep_sv_l)
    bl_dep_sv_df <- left_join(
      bl_dep_sv_df,
      bl_dep_df %>% 
        mutate(idx = row_number()) %>% 
        select(idx, id)
    ) %>% 
      select(-idx)
    
    stopifnot(all(unique(bl_dep_sv_df$id) %in% bl_dep_df$id))
    
    # Save each day as a different file
    if(verbose) cat("\nSave as different files")
    
    ldf2files(
      ldf = bl_dep_df %>% 
        group_split(departure_date),
      base_file_name <- "bl_dep",
      verbose = verbose
    )
    
    if(exists("bl_dep_sv_df")) {
      ldf2files(
        ldf = bl_dep_sv_df %>% 
          left_join(bl_dep_df %>% select(arrival_date, departure_date, id)) %>% 
          group_split(departure_date),
        base_file_name <- "bl_dep_SV",
        verbose = verbose
      )  
    }

  } 


  if(verbose) cat("\nGet ",  airport, " arrivals")
  ### Arrivals 
  bl_arr <- getAirportArrivals(
    airport=airport,
    endTime=end,
    startTime=start,
    timeZone = tz,
    username = usr,
    password = pwd
  )
  
  if(!is.null(bl_arr) & length(bl_arr) > 0) {
    
    bl_arr_df <- os_airport2df(bl_arr) 
    
    # SV
    if(verbose) cat("\n\tGet for these arrivals the corresponding state vectors")
    bl_arr_sv_l <- 1:nrow(bl_arr_df) %>% 
      map(function(ii) {
        if(verbose) cat("\tFetching SV for ", ii, "/", nrow(bl_arr_df))
        getAircraftStateVectorsSeries(
          bl_arr_df$ICAO24[ii],
          bl_arr_df$departure_time[ii],
          bl_arr_df$arrival_time[ii],
          timeZone = tz,
          timeResolution = timeRes,
          username = usr,
          password = pwd,
          useImpalaShell = T,
          timeOut = timeOut,
          maxQueryAttempts = 2
        ) 
      })
    
    bl_arr_sv_df <- os_aircraft_SV_2df(bl_arr_sv_l)
    
    bl_arr_sv_df <- left_join(
      bl_arr_sv_df,
      bl_arr_df %>% 
        mutate(idx = row_number()) %>% 
        select(idx, id)
    ) %>% 
      select(-idx)
    
    stopifnot(all(unique(bl_arr_sv_df$id) %in% bl_arr_df$id))
    
    # Save each day as a different file
    ldf2files(
      ldf = bl_arr_df %>% 
        group_split(arrival_date),
      base_file_name <- "bl_arr"
    )
    
    if(exists("bl_arr_sv_df")) {
      ldf2files(
        ldf = bl_arr_sv_df %>% 
          left_join(bl_arr_df %>% select(arrival_date, departure_date, id)) %>% 
          group_split(arrival_date),
        base_file_name <- "bl_arr_SV"
      )
    }
    
  }
}
