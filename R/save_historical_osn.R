source("R/_helper.R")

# SETTINGS
verbose <- T

# tz  <- "Europe/Zurich"
# timeRes <- 180
# timeOut <- 7


nStartday <- 30 # 32
end <- str_c(Sys.Date() - 1, " 23:59:59")
start <- str_c(Sys.Date() - nStartday, " 00:00:00")

qDates <- seq(as.Date(start), as.Date(end), 1)


missingDates <- DatesMissingData(qDates)

if (length(missingDates) > 0) {
  startEnd_df <- seq_along(missingDates) %>%
    map_df(function(dd) {
      tibble(
        start = str_c(missingDates[dd], " 00:00:00"),
        end = str_c(missingDates[dd], " 23:59:59")
      )
    })

  if (verbose) {
    cat(
      "\nAbout to fetch data for these ", length(missingDates),
      " days: ",
      as.character(missingDates)
    )
  }

  ## main
  seq_len(nrow(startEnd_df)) %>%
    map(function(ii) {
      getSaveArrivalDeparture(
        start = startEnd_df$start[ii],
        end = startEnd_df$end[ii],
        verbose = verbose
      )
    })
}

## concatenate everything available
concatFiles(
  dir = "data_raw", reg = "^bl_dep_\\d+",
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
) %>%
  arrange(departure_time) %>%
  write_csv("data_raw/bl_dep_all.csv")

concatFiles(
  dir = "data_raw", reg = "^bl_arr_\\d+",
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
) %>%
  arrange(departure_time) %>%
  write_csv("data_raw/bl_arr_all.csv")

# STATE VECTORS
# Take the last 4 months
q_y_m <- seq(Sys.Date() - 30 * 4, Sys.Date(), 1) %>%
  format("%Y_%m") %>%
  unique()

concatFiles("data_raw",
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
  )
) %>%
  arrange(departure_date) %>%
  write_csv("data_raw/bl_dep_SV_all.csv")


concatFiles("data_raw",
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
  )
) %>%
  arrange(arrival_date) %>%
  write_csv("data_raw/bl_arr_SV_all.csv")


# AIRCRAFT METADATA
# Collect unique aircraft seen in the last 30 days
if (verbose) cat("\n\n--- AIRCRAFT METADATA ---")

# Define column specs for flight data
flight_col_spec <- cols(
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

all_flights <- bind_rows(
  read_csv("data_raw/bl_dep_all.csv", col_types = flight_col_spec, show_col_types = F),
  read_csv("data_raw/bl_arr_all.csv", col_types = flight_col_spec, show_col_types = F)
) %>%
  filter(
    !is.na(ICAO24),
    (departure_date >= Sys.Date() - 30) | (arrival_date >= Sys.Date() - 30)
  )

if (nrow(all_flights) > 0) {
  unique_aircraft <- all_flights %>%
    group_by(ICAO24) %>%
    summarize(
      call_sign = last(call_sign),
      .groups = "drop"
    )

  # Load existing metadata
  meta_file <- "data_raw/aircraft_metadata.csv"
  meta_col_spec <- cols_only(
    ICAO24 = col_character(),
    call_sign = col_character(),
    model = col_character(),
    origin_country = col_character(),
    photo_url = col_character()
  )

  if (file.exists(meta_file)) {
    existing_meta <- read_csv(meta_file, col_types = meta_col_spec, show_col_types = F)
  } else {
    existing_meta <- tibble(
      ICAO24 = character(),
      call_sign = character(),
      model = character(),
      origin_country = character(),
      photo_url = character()
    )
  }

  # Identify aircraft needing metadata (new or missing info)
  to_fetch <- unique_aircraft %>%
    anti_join(
      existing_meta %>% filter(!is.na(model)),
      by = "ICAO24"
    )

  if (nrow(to_fetch) > 0) {
    if (verbose) cat("\nFetching metadata for ", nrow(to_fetch), " aircraft")
    new_meta <- to_fetch$ICAO24 %>%
      map_df(~ get_aircraft_metadata(.x, verbose = verbose))

    # Merge with existing, prioritize new data
    updated_meta <- bind_rows(
      existing_meta %>% filter(!(ICAO24 %in% new_meta$ICAO24)),
      new_meta
    ) %>%
      # Update call signs from our 30-day window
      select(-any_of("call_sign")) %>%
      left_join(unique_aircraft, by = "ICAO24") %>%
      select(ICAO24, call_sign, model, origin_country, photo_url) %>%
      arrange(ICAO24)

    write_csv(updated_meta, meta_file)
    if (verbose) cat("\nMetadata saved to", meta_file, "\n")
  } else {
    if (verbose) cat("\nNo new aircraft metadata to fetch")

    # Still update call signs for existing ones if they've changed in the 30-day window
    updated_meta <- existing_meta %>%
      select(-any_of("call_sign")) %>%
      left_join(unique_aircraft, by = "ICAO24") %>%
      select(ICAO24, call_sign, model, origin_country, photo_url) %>%
      arrange(ICAO24)

    write_csv(updated_meta, meta_file)
    if (verbose) cat("\nMetadata updated with latest call signs.\n")
  }
}

# AIRPORT METADATA
# Collect unique airports seen in the last 30 days
if (verbose) cat("\n\n--- AIRPORT METADATA ---")

if (nrow(all_flights) > 0) {
  # Extract unique airports (both departure and destination) from the last 30 days
  unique_airports <- c(all_flights$departure_airport_ICAO, all_flights$destination_airport_ICAO) %>%
    unique() %>%
    na.omit() %>%
    as.character()

  # Remove empty strings if any
  unique_airports <- unique_airports[unique_airports != ""]

  # Load existing airport metadata
  airport_meta_file <- "data_raw/airport_metadata.csv"
  airport_meta_col_spec <- cols_only(
    ICAO = col_character(),
    IATA = col_character(),
    name = col_character(),
    city = col_character(),
    country = col_character(),
    longitude = col_double(),
    latitude = col_double(),
    altitude = col_double()
  )

  if (file.exists(airport_meta_file)) {
    existing_airport_meta <- read_csv(airport_meta_file, col_types = airport_meta_col_spec, show_col_types = F)
  } else {
    existing_airport_meta <- tibble(
      ICAO = character(),
      IATA = character(),
      name = character(),
      city = character(),
      country = character(),
      longitude = numeric(),
      latitude = numeric(),
      altitude = numeric()
    )
  }

  # Identify airports needing metadata
  airports_to_fetch <- setdiff(unique_airports, existing_airport_meta$ICAO)

  if (length(airports_to_fetch) > 0) {
    if (verbose) cat("\nFetching metadata for ", length(airports_to_fetch), " airports")

    new_airport_meta <- airports_to_fetch %>%
      map_df(~ get_airport_metadata_safe(.x, verbose = verbose))

    # Merge with existing, prioritize new data
    updated_airport_meta <- bind_rows(
      existing_airport_meta,
      new_airport_meta
    ) %>%
      arrange(ICAO)

    write_csv(updated_airport_meta, airport_meta_file)
    if (verbose) cat("\nAirport metadata saved to", airport_meta_file, "\n")
  } else {
    if (verbose) cat("\nNo new airport metadata to fetch")
    # Write back anyway to clean up potential extra columns from the file
    write_csv(existing_airport_meta %>% arrange(ICAO), airport_meta_file)
  }
}
