# Lausanne Airport Flight Tracker 🦁✈️

![Banner](DALL·E%202022-10-16%2021.10.11%20-%20A%20standing%20lion%20looking%20at%20a%20flying%20airplane%20in%20the%20style%20of%20vaporware.png)

A data collection project that tracks all flights arriving and departing from Lausanne-Blécherette Airport (LSGL) using the OpenSky Network API. The project automatically collects historical flight data including aircraft positions, call signs, and flight paths.

## 📊 What This Repository Does

This repository:
- **Automatically fetches** flight data from the [OpenSky Network API](https://opensky-network.org) for LSGL airport
- **Runs daily** via GitHub Actions to collect and update historical flight records
- **Stores** both basic flight information (arrivals/departures) and detailed position data (state vectors)
- **Maintains** a comprehensive historical dataset dating back to September 2022

## 🗂️ Repository Structure

```
.
├── data_raw/                  # All collected flight data (CSV files)
│   ├── bl_arr_YYYY_MM_DD.csv  # Daily arrival flights
│   ├── bl_dep_YYYY_MM_DD.csv  # Daily departure flights
│   ├── bl_arr_SV_YYYY_MM_DD.csv  # Daily arrival state vectors (GPS positions)
│   ├── bl_dep_SV_YYYY_MM_DD.csv  # Daily departure state vectors
│   ├── bl_arr_all.csv         # Consolidated arrivals
│   ├── bl_dep_all.csv         # Consolidated departures
│   ├── bl_arr_SV_all.csv      # Consolidated arrival state vectors
│   └── bl_dep_SV_all.csv      # Consolidated departure state vectors
├── R/                         # R scripts for data collection
│   ├── _helper.R              # Helper functions for OpenSky API
│   ├── save_historical_osn.R  # Main script to fetch and save data
│   └── getAndViz.Rmd          # Data exploration and visualization notebook
├── .github/workflows/         # Automated data collection
│   └── main.yml               # GitHub Actions workflow (runs twice daily)
└── README.md                  # This file
```

## 📁 Data Structure

### Flight Data Files

#### Arrivals and Departures (`bl_arr_*.csv` and `bl_dep_*.csv`)

Basic flight information for each arrival or departure.

**Columns:**
| Column | Type | Description |
|--------|------|-------------|
| `ICAO24` | String | Unique 24-bit ICAO aircraft identifier (e.g., `4b4326`) |
| `call_sign` | String | Flight call sign (e.g., `HBZLE`) |
| `departure_time` | DateTime | Actual departure time (UTC) |
| `departure_date` | Date | Departure date |
| `arrival_time` | DateTime | Actual arrival time (UTC) |
| `arrival_date` | Date | Arrival date |
| `departure_airport_ICAO` | String | ICAO code of departure airport (e.g., `LSGL`) |
| `destination_airport_ICAO` | String | ICAO code of destination airport |
| `id` | String | Unique flight identifier: `{ICAO24}_{departure_time}` |

**Example:**
```csv
ICAO24,call_sign,departure_time,departure_date,arrival_time,arrival_date,departure_airport_ICAO,destination_airport_ICAO,id
4b4326,HBZLE,2022-09-10T07:12:33Z,2022-09-10,2022-09-10T08:11:58Z,2022-09-10,LSZM,LSGL,4b4326_2022-09-10 09:12:33
```

#### State Vectors (`bl_arr_SV_*.csv` and `bl_dep_SV_*.csv`)

GPS position data sampled at regular intervals (typically every 3 minutes) during each flight.

**Columns:**
| Column | Type | Description |
|--------|------|-------------|
| `ICAO24` | String | Aircraft identifier |
| `longitude` | Float | GPS longitude (decimal degrees) |
| `latitude` | Float | GPS latitude (decimal degrees) |
| `requested_time` | Integer | Unix timestamp of position measurement |
| `geo_altitude` | Float | Altitude in meters (barometric) |
| `velocity` | Float | Ground speed in m/s |
| `special_purpose_indicator` | Boolean | Special purpose indicator |
| `origin_country` | String | Country of aircraft registration |
| `id` | String | Flight identifier (matches flight data files) |
| `arrival_date` | Date | Flight arrival date |
| `departure_date` | Date | Flight departure date |

**Example:**
```csv
ICAO24,longitude,latitude,requested_time,geo_altitude,velocity,special_purpose_indicator,origin_country,id,arrival_date,departure_date
4b43ad,6.22727,46.38366,1666246299,NA,0,FALSE,NA,4b43ad_2022-10-20 08:11:39,2022-10-20,2022-10-20
```

### File Naming Convention

- `bl_arr_YYYY_MM_DD.csv` - Arrivals for specific date
- `bl_dep_YYYY_MM_DD.csv` - Departures for specific date
- `bl_arr_SV_YYYY_MM_DD.csv` - State vectors for arrivals on specific date
- `bl_dep_SV_YYYY_MM_DD.csv` - State vectors for departures on specific date
- `*_all.csv` files - Consolidated data across all dates

## 🚀 Using the Data

### Quick Start with R

```r
# Load arrivals data
library(readr)
arrivals <- read_csv("data_raw/bl_arr_all.csv")

# Load departures data
departures <- read_csv("data_raw/bl_dep_all.csv")

# Load state vectors for arrivals
arrival_positions <- read_csv("data_raw/bl_arr_SV_all.csv")
```

### Quick Start with Python

```python
import pandas as pd

# Load arrivals data
arrivals = pd.read_csv("data_raw/bl_arr_all.csv")

# Load departures data
departures = pd.read_csv("data_raw/bl_dep_all.csv")

# Load state vectors for arrivals
arrival_positions = pd.read_csv("data_raw/bl_arr_SV_all.csv")
```

### Common Use Cases

#### 1. Count flights per day
```r
library(dplyr)

daily_traffic <- departures %>%
  group_by(departure_date) %>%
  summarize(
    num_flights = n(),
    unique_aircraft = n_distinct(ICAO24)
  )
```

#### 2. Find most common routes
```r
popular_routes <- departures %>%
  count(departure_airport_ICAO, destination_airport_ICAO) %>%
  arrange(desc(n))
```

#### 3. Plot flight paths
```r
library(ggplot2)

# Get state vectors for a specific flight
flight_path <- arrival_positions %>%
  filter(id == "4b43ad_2022-10-20 08:11:39")

# Plot the trajectory
ggplot(flight_path, aes(x = longitude, y = latitude)) +
  geom_path() +
  geom_point() +
  theme_minimal() +
  labs(title = "Flight Path", x = "Longitude", y = "Latitude")
```

## 🔄 Data Updates

The data is automatically updated **twice daily** at:
- 04:27 UTC
- 08:27 UTC

via GitHub Actions workflow. The workflow fetches data for the last 30 days and updates missing dates.

## 🔐 Running Locally

To run the data collection scripts locally:

1. **Install R dependencies:**
   ```r
   # Install renv for package management
   install.packages("renv")
   renv::restore()
   ```

2. **Set up OpenSky credentials:**
   Create a `.Renviron` file in the project root:
   ```bash
   OPENSKY_USR="your_username"
   OPENSKY_PWD="your_password"
   ```
   > **Note:** Sign up for a free account at [OpenSky Network](https://opensky-network.org) to access historical data

3. **Run the data collection script:**
   ```r
   Rscript R/save_historical_osn.R
   ```

> ⚠️ **Warning:** Full data collection with state vectors can take several hours for 30 days of data.

## 📖 About LSGL

**Lausanne-Blécherette Airport** (ICAO: LSGL) is a small regional airport in Switzerland primarily used for:
- General aviation
- Flight training
- Private aircraft
- Occasional small commercial flights

Given its nature, flight volume is relatively low compared to major airports (typically 5-20 flights per day).

## 🔗 Useful Links

- [OpenSky Network Airport Profile for LSGL](https://opensky-network.org/airport-profile?icao=LSGL)
- [OpenSky Network API Documentation](https://openskynetwork.github.io/opensky-api/)
- [R openSkies Package](https://CRAN.R-project.org/package=openSkies)

## 📝 License

GPL-3.0 License - See [LICENSE](LICENSE) file for details

## 🤝 Contributing

This is primarily a personal data collection project, but issues and suggestions are welcome!

---

**Data Source:** [OpenSky Network](https://opensky-network.org) - The OpenSky Network is a non-profit association that provides open air traffic data for research and non-commercial purposes.
