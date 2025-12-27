
source("renv/activate.R")
library(openSkies)
library(dplyr)
library(lubridate)

# Helper to print results
check_airport <- function(airport_code, date_str) {
  cat("\n------------------------------------------------\n")
  cat("Checking", airport_code, "for date:", date_str, "\n")
  
  start_time <- paste0(date_str, " 12:00:00")
  end_time <- paste0(date_str, " 13:00:00") # 1 hour window to be quick
  
  cat("Querying", start_time, "to", end_time, "...\n")
  
  tryCatch({
    deps <- getAirportDepartures(
      airport = airport_code,
      startTime = start_time,
      endTime = end_time,
      timeZone = "Europe/Zurich",
      username = Sys.getenv("OPENSKY_USR"),
      password = Sys.getenv("OPENSKY_PWD")
    )
    
    if (is.null(deps) || length(deps) == 0) {
      cat("No departures found.\n")
    } else {
      cat("Found", length(deps), "departures.\n")
      print(head(deps))
    }
  }, error = function(e) {
    cat("Error querying departures:", e$message, "\n")
  })
}

# 1. Check LSGG (Geneva) - should have data
check_airport("LSGG", as.character(Sys.Date() - 2))

# 2. Check LSGL (Lausanne) - reported missing
check_airport("LSGL", as.character(Sys.Date() - 2))

# 3. Check Bounding Box for LSGL
cat("\n------------------------------------------------\n")
cat("Checking Bounding Box for LSGL (Lausanne)...\n")
# Approx box around Lausanne Blécherette
min_lat <- 46.54
max_lat <- 46.56
min_lon <- 6.60
max_lon <- 6.63

start_time <- paste0(as.character(Sys.Date() - 2), " 12:00:00")
end_time <- paste0(as.character(Sys.Date() - 2), " 12:10:00") # 10 mins

cat("Querying state vectors in box", min_lat, min_lon, max_lat, max_lon, "\n")

tryCatch({
  # getIntervalStateVectors is expensive, checking single state vectors first
  # actually getIntervalStateVectors is deprecated or heavy, let's use getAircraftStateVectorsSeries for a known aircraft if we had one, 
  # or better: we use `getBoxStateVectors`? No, that's instantaneous.
  # Let's try `openSkies` equivalent?
  # Actually `getAirportDepartures` relies on `flight` data, which comes from ADSB agg.
  # If that's empty, we might need to rely on `getIntervalStateVectors` restricted to a box?
  # NOTE: openSkies function names have changed over time. Let's stick to airport query for now in this repro 
  # to confirm the basic point.
  NULL
}, error = function(e) {
  cat("Error:", e$message, "\n")
})
