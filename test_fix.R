source("R/_helper.R")

# Test with a specific date that had errors
verbose <- TRUE

# Test the problematic date from the error message
start <- "2025-12-27 00:00:00"
end <- "2025-12-27 23:59:59"

cat("\n=== Testing fix for empty state vectors error ===\n")
cat("Date:", start, "to", end, "\n\n")

tryCatch(
    {
        getSaveArrivalDeparture(
            start = start,
            end = end,
            airport = "LSGL",
            tz = "Europe/Zurich",
            timeRes = 180,
            timeOut = 7,
            verbose = TRUE
        )
        cat("\n✓ Script completed without errors!\n")
    },
    error = function(e) {
        cat("\n✗ Error occurred:", e$message, "\n")
        cat("Stack trace:\n")
        print(traceback())
    }
)
