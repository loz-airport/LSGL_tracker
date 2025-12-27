source("renv/activate.R")
library(openSkies)
library(dplyr)
library(lubridate)

# Test for various recent dates to see if LSGL has ANY flights
test_dates <- seq(Sys.Date() - 30, Sys.Date() - 1, by = 7) # Sample every 7 days

cat("\nTesting LSGL flights for various dates over last 30 days...\n")
cat("=", rep("=", 60), "\n", sep = "")

for (i in seq_along(test_dates)) {
    test_date <- test_dates[i]
    date_str <- format(test_date, "%Y-%m-%d")
    start_time <- paste0(date_str, " 00:00:00")
    end_time <- paste0(date_str, " 23:59:59")

    cat("\nDate:", date_str, "\n")

    tryCatch(
        {
            # Test departures
            deps <- getAirportDepartures(
                airport = "LSGL",
                startTime = start_time,
                endTime = end_time,
                timeZone = "Europe/Zurich",
                username = Sys.getenv("OPENSKY_USR"),
                password = Sys.getenv("OPENSKY_PWD")
            )

            dep_count <- if (is.null(deps) || length(deps) == 0) 0 else length(deps)
            cat("  Departures:", dep_count, "\n")

            # Test arrivals
            arrs <- getAirportArrivals(
                airport = "LSGL",
                startTime = start_time,
                endTime = end_time,
                timeZone = "Europe/Zurich",
                username = Sys.getenv("OPENSKY_USR"),
                password = Sys.getenv("OPENSKY_PWD")
            )

            arr_count <- if (is.null(arrs) || length(arrs) == 0) 0 else length(arrs)
            cat("  Arrivals:", arr_count, "\n")

            if (dep_count > 0 || arr_count > 0) {
                cat("  ✓ FOUND DATA!\n")
            }
        },
        error = function(e) {
            cat("  Error:", e$message, "\n")
        }
    )

    Sys.sleep(2) # Be nice to the API
}

cat("\n")
cat("=", rep("=", 60), "\n", sep = "")
cat("\nTest complete.\n")
