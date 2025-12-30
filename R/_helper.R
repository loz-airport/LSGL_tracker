# library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(openSkies)
library(lubridate)
library(jsonlite)

# usr <-  Sys.getenv("usr_osn")
# pwd <-  Sys.getenv("pwd_osn")

usr <- Sys.getenv("OPENSKY_USR")
pwd <- Sys.getenv("OPENSKY_PWD")


get_osf_details <- function(ii, timez = "CET") {
  tibble(
    ICAO24 = ii$ICAO24,
    call_sign = ii$call_sign,
    departure_time = with_tz(ii$departure_time, timez),
    departure_date = as.Date(departure_time, tz = timez),
    arrival_time = with_tz(ii$arrival_time, timez),
    arrival_date = as.Date(arrival_time, tz = timez),
    departure_airport_ICAO = ii$origin_airport,
    destination_airport_ICAO = ii$destination_airport,
    id = str_c(ICAO24, "_", departure_time)
  )
}

os_airport2df <- function(airl) {
  seq_along(airl) %>%
    map_df(function(iii) {
      get_osf_details(airl[[iii]])
    }) %>%
    arrange(departure_time)
}


os_aircraft_SV_2df <- function(aircr, timez = "CET") {
  seq_along(aircr) %>%
    map_df(function(ii) {
      if (!is.null(aircr[[ii]])) {
        aircr[[ii]]$get_values(
          c(
            "ICAO24",
            "longitude", "latitude", "requested_time",
            "geo_altitude", "velocity",
            "special_purpose_indicator", "origin_country"
          )
        ) %>%
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
# output_dir <- "data_raw"
# ii <- 1

# Take a list data.frame, split by date & save them by date
ldf2files <- function(
  ldf,
  base_file_name = "bl_dep",
  output_dir = "data_raw",
  verbose = F
) {
  if (length(ldf) > 0) {
    seq_along(ldf) %>%
      map(function(ii) {
        tdate <- ldf[[ii]]$departure_date %>%
          unique() %>%
          max()
        if (length(tdate) > 1) {
          warning("\nThis chunk has different dates!\n")
        }

        out_file <- str_c(
          output_dir, "/", base_file_name, "_",
          format(tdate, "%Y_%m_%d"), ".csv"
        )
        if (file.exists(out_file)) {
          tmp_read <- read_csv(out_file, show_col_types = F)
          tmp_nrow <- tmp_read %>% nrow()
        } else {
          tmp_nrow <- 0
        }
        if (nrow(ldf[[ii]]) > tmp_nrow) {
          if (verbose) {
            cat(
              "\n nrow(ldf[[ii]]: ", nrow(ldf[[ii]]), " > ",
              tmp_nrow, " (tmp_nrow)"
            )
          }

          ldf[[ii]] %>%
            write_csv(out_file)
        } else {
          if (verbose) {
            cat(
              "\nNothing to save because tmp_nrow:", tmp_nrow,
              "\tcurrently fetched: ", nrow(ldf[[ii]]), " rows"
            )
          }
        }
      })
  }
}

checkFileData <- function(out_file, nrow_threshold = 1) {
  tmp_nrow <- 0
  if (file.exists(out_file)) {
    tmp_read <- read_csv(out_file, show_col_types = F)
    tmp_nrow <- tmp_read %>% nrow()
  } else {
    tmp_nrow <- 0
  }
  if (tmp_nrow > nrow_threshold) {
    T
  } else {
    F
  }
}

DatesMissingData <- function(
  qDates,
  base_file_name = c("bl_dep", "bl_arr", "bl_dep_SV", "bl_arr_SV"),
  output_dir = "data_raw",
  verbose = F
) {
  stopifnot(is.Date(qDates))

  out_files <- qDates %>%
    map_df(function(dd) {
      out_files <- str_c(
        output_dir, "/", base_file_name, "_",
        format(dd, "%Y_%m_%d"), ".csv"
      )
      tibble(
        date = dd,
        dep = out_files[1], arr = out_files[2],
        depSV = out_files[3], arrSV = out_files[4]
      )
    })
  out_check <- seq_len(nrow(out_files)) %>%
    map_lgl(function(ii) {
      out_files %>%
        slice(ii) %>%
        select(-date) %>%
        unlist() %>%
        map_lgl(checkFileData, nrow_threshold = 0) %>%
        all()
    })

  qDates[!out_check]
}


## main wrapper get and save arrival/departurs

getSaveArrivalDeparture <- function(
  start,
  end,
  airport = "LSGL",
  tz = "Europe/Zurich",
  timeRes = 180,
  timeOut = 7,
  verbose = F
) {
  stopifnot(exists("usr"))
  stopifnot(exists("pwd"))

  # Get airport arrivals/departures without state vectors
  # Get stateVectors separately

  if (verbose) cat("\n\nGET ", airport, " stuff from ", start, " until ", end)
  if (verbose) cat("\nGet ", airport, " departures")
  ### Departures
  bl_dep <- getAirportDepartures(
    airport = airport,
    endTime = end,
    startTime = start,
    timeZone = tz,
    username = usr,
    password = pwd
  )
  if (!is.null(bl_dep) && length(bl_dep) > 0) {
    bl_dep_df <- os_airport2df(bl_dep)
    # SV
    if (verbose) cat("\n\tGet for these departures the corresponding state vectors")
    bl_dep_sv_l <- seq_len(nrow(bl_dep_df)) %>%
      map(function(ii) {
        if (verbose) cat("\tFetching SV for ", ii, "/", nrow(bl_dep_df))
        tryCatch(
          {
            getAircraftStateVectorsSeries(
              bl_dep_df$ICAO24[ii],
              bl_dep_df$departure_time[ii],
              bl_dep_df$arrival_time[ii],
              timeZone = tz,
              timeResolution = timeRes,
              username = usr,
              password = pwd,
              timeOut = timeOut,
              maxQueryAttempts = 2
            )
          },
          error = function(e) {
            if (verbose) cat("\n\tError fetching SV: ", e$message)
            NULL
          }
        )
      })
    bl_dep_sv_df <- os_aircraft_SV_2df(bl_dep_sv_l)

    # Only process state vectors if we have data
    if (nrow(bl_dep_sv_df) > 0) {
      bl_dep_sv_df <- left_join(
        bl_dep_sv_df,
        bl_dep_df %>%
          mutate(idx = row_number()) %>%
          select(idx, id)
      ) %>%
        select(-idx)

      stopifnot(all(unique(bl_dep_sv_df$id) %in% bl_dep_df$id))
    } else {
      if (verbose) cat("\n\tNo state vectors available for departures")
    }

    # Save each day as a different file
    if (verbose) cat("\nSave as different files")

    ldf2files(
      ldf = bl_dep_df %>%
        group_split(departure_date),
      base_file_name <- "bl_dep",
      verbose = verbose
    )

    if (exists("bl_dep_sv_df") && nrow(bl_dep_sv_df) > 0) {
      ldf2files(
        ldf = bl_dep_sv_df %>%
          left_join(bl_dep_df %>% select(arrival_date, departure_date, id)) %>%
          group_split(departure_date),
        base_file_name <- "bl_dep_SV",
        verbose = verbose
      )
    }
  }


  if (verbose) cat("\nGet ", airport, " arrivals")
  ### Arrivals
  bl_arr <- getAirportArrivals(
    airport = airport,
    endTime = end,
    startTime = start,
    timeZone = tz,
    username = usr,
    password = pwd
  )

  if (!is.null(bl_arr) && length(bl_arr) > 0) {
    bl_arr_df <- os_airport2df(bl_arr)

    # SV
    if (verbose) cat("\n\tGet for these arrivals the corresponding state vectors")
    bl_arr_sv_l <- seq_len(nrow(bl_arr_df)) %>%
      map(function(ii) {
        if (verbose) cat("\tFetching SV for ", ii, "/", nrow(bl_arr_df))
        tryCatch(
          {
            getAircraftStateVectorsSeries(
              bl_arr_df$ICAO24[ii],
              bl_arr_df$departure_time[ii],
              bl_arr_df$arrival_time[ii],
              timeZone = tz,
              timeResolution = timeRes,
              username = usr,
              password = pwd,
              timeOut = timeOut,
              maxQueryAttempts = 2
            )
          },
          error = function(e) {
            if (verbose) cat("\n\tError fetching SV: ", e$message)
            NULL
          }
        )
      })

    bl_arr_sv_df <- os_aircraft_SV_2df(bl_arr_sv_l)

    # Only process state vectors if we have data
    if (nrow(bl_arr_sv_df) > 0) {
      bl_arr_sv_df <- left_join(
        bl_arr_sv_df,
        bl_arr_df %>%
          mutate(idx = row_number()) %>%
          select(idx, id)
      ) %>%
        select(-idx)

      stopifnot(all(unique(bl_arr_sv_df$id) %in% bl_arr_df$id))
    } else {
      if (verbose) cat("\n\tNo state vectors available for arrivals")
    }

    # Save each day as a different file
    ldf2files(
      ldf = bl_arr_df %>%
        group_split(arrival_date),
      base_file_name <- "bl_arr"
    )

    if (exists("bl_arr_sv_df") && nrow(bl_arr_sv_df) > 0) {
      ldf2files(
        ldf = bl_arr_sv_df %>%
          left_join(bl_arr_df %>% select(arrival_date, departure_date, id)) %>%
          group_split(arrival_date),
        base_file_name <- "bl_arr_SV"
      )
    }
  }
}

# concatenate files as a single data.frame
concatFiles <- function(
  dir = "data_raw",
  reg = "^bl_dep_\\d+",
  col_spec = cols(
    ICAO24 = col_character(),
    call_sign = col_character(),
    departure_time = col_datetime(format = ""),
    departure_date = col_date(format = ""),
    arrival_time = col_datetime(format = ""),
    arrival_date = col_date(format = ""),
    departure_airport_ICAO = col_character(),
    destination_airport_ICAO = col_character(),
    id = col_character()
  )
) {
  fls <- list.files(dir, reg, full.names = T)

  if (length(fls) == 0) {
    header <- paste(names(col_spec$cols), collapse = ",")
    return(read_csv(I(paste0(header, "\n")), col_types = col_spec, show_col_types = FALSE))
  }

  fls %>%
    map(~ read_csv(.x, col_types = col_spec, show_col_types = FALSE)) %>%
    bind_rows()
}

# Fetch aircraft metadata from OpenSky and Planespotters
get_aircraft_metadata <- function(icao24, verbose = FALSE) {
  if (verbose) cat("\nFetching metadata for:", icao24)

  # 1. OpenSky Metadata
  os_meta <- tryCatch(
    {
      getAircraftMetadata(icao24)
    },
    error = function(e) {
      if (verbose) cat("\n\tOpenSky error:", e$message)
      NULL
    }
  )

  # 2. Planespotters Photo
  photo_url <- tryCatch(
    {
      ps_url <- paste0("https://api.planespotters.net/pub/photos/hex/", icao24)
      res <- fromJSON(ps_url)
      if (!is.null(res) && !is.null(res$photos) && length(res$photos) > 0) {
        res$photos$thumbnail_large$src[1]
      } else {
        NA_character_
      }
    },
    error = function(e) {
      if (verbose) cat("\n\tPlanespotters error:", e$message)
      NA_character_
    }
  )

  if (is.null(os_meta)) {
    return(tibble(
      ICAO24 = icao24,
      model = NA_character_,
      origin_country = NA_character_,
      photo_url = photo_url
    ))
  }

  tibble(
    ICAO24 = icao24,
    model = ifelse(is.null(os_meta$model), NA_character_, os_meta$model),
    origin_country = ifelse(is.null(os_meta$origin_country), NA_character_, os_meta$origin_country),
    photo_url = photo_url
  )
}

# Fetch airport metadata from OpenSky
get_airport_metadata_safe <- function(airport_icao, verbose = FALSE) {
  if (verbose) cat("\nFetching metadata for:", airport_icao)

  # Check if airport_icao is valid
  if (is.na(airport_icao) || airport_icao == "") {
    return(NULL)
  }

  meta <- tryCatch(
    {
      getAirportMetadata(airport_icao)
    },
    error = function(e) {
      if (verbose) cat("\n\tOpenSky error for airport:", airport_icao, "-", e$message)
      NULL
    }
  )

  if (is.null(meta)) {
    return(tibble(
      ICAO = airport_icao,
      IATA = NA_character_,
      name = NA_character_,
      city = NA_character_,
      country = NA_character_,
      longitude = NA_real_,
      latitude = NA_real_,
      altitude = NA_real_
    ))
  }

  # Helper to fix common encoding issues (UTF-8 bytes read as Latin-1/Windows-1252)
  # e.g. "Ã¨" (C3 A8) should be "è" (byte C3 A8)
  fix_mojibake <- function(x) {
    if (length(x) == 0 || all(is.na(x))) {
      return(x)
    }
    x_fixed <- tryCatch(
      {
        # Attempt to reverse the misinterpretation:
        # Convert from current (misinterpreted) chars back to bytes (as Windows-1252)
        # Then mark those bytes as UTF-8
        repaired <- iconv(x, from = "UTF-8", to = "WINDOWS-1252")
        # Check if repair produced valid UTF-8
        if (!any(is.na(repaired)) && validEnc(repaired)) {
          Encoding(repaired) <- "UTF-8"
          repaired
        } else {
          x
        }
      },
      error = function(e) x
    )

    # Only return repaired if it didn't turn into NAs where x wasn't NA
    ifelse(!is.na(x) & is.na(x_fixed), x, x_fixed)
  }

  # Simple validity check
  validEnc <- function(x) {
    # If iconv to itself works, it's valid in that encoding
    # or just trust the previous step didn't fail
    TRUE
  }

  tibble(
    ICAO = meta$ICAO,
    IATA = ifelse(is.null(meta$IATA) || meta$IATA == "" || length(meta$IATA) == 0, NA_character_, meta$IATA),
    name = ifelse(is.null(meta$name) || length(meta$name) == 0, NA_character_, fix_mojibake(meta$name)),
    city = ifelse(is.null(meta$city) || length(meta$city) == 0, NA_character_, fix_mojibake(meta$city)),
    country = ifelse(is.null(meta$country) || length(meta$country) == 0, NA_character_, fix_mojibake(meta$country)),
    longitude = ifelse(is.null(meta$longitude) || length(meta$longitude) == 0, NA_real_, as.numeric(meta$longitude)),
    latitude = ifelse(is.null(meta$latitude) || length(meta$latitude) == 0, NA_real_, as.numeric(meta$latitude)),
    altitude = ifelse(is.null(meta$altitude) || length(meta$altitude) == 0, NA_real_, as.numeric(meta$altitude))
  )
}
