### 
library(lubridate)
library(sp)
library(sf)
library(mapview)
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(ggrepel)
library(scales)
library(adehabitatHR)
library(forcats)
##






# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
#### Clean GPS data #### 

## Make time coulm into hours, minutes, seconds....
# Change into Norwegian time format
df$Date<-lubridate::with_tz(df$UTC_datetime, "CET")
head (df)

# Add year, month, day, hour, min and sec in different columns.
df$Day<- as.numeric(format(df$Date, "%d"))
df$Month<- as.numeric(format(df$Date, "%m"))
df$Year<- as.numeric(format(df$Date, "%Y"))
df$Hour <- as.numeric(format(df$Date, "%H"))
df$Minute <- as.numeric(format(df$Date, "%M"))
df$Second <- as.numeric(format(df$Date, "%S"))


# Create Julian day and week number
df$JulianDay <- as.numeric(format(df$Date, "%j"))
head (df)

df$Week <- as.numeric(format(df$Date, "%W"))
head (df)

# Filtering out poor values of HDOP and SATCOUNT
coordinates(df) <- c("Longitude","Latitude")
proj4string(df) <- CRS("+proj=longlat +datum=WGS84")

range(df$hdop) 
range(df$satcount) 

# use subset to remove poor values. 
df <- subset (df, df$hdop <= 5) 
df <- subset (df, df$satcount > 3)

# Removing the first 6 hours from when the GPS was instrumented.
# Find the earliest time in the dataset 
start_time <- min(df$Date)

# Calculate the cutoff time 
cutoff_time <- start_time + hours(6)

# Subset the data to remove the first six hours
df <- subset(df, Date > cutoff_time)

# Check if GPS fixes looks correct/ no weird outliers outside the study area 
mapview (df)



# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@- 
# # # # When did the bird leave the nest for the last time? # # # 
# (This will be used to split incubation phase and chick rearing phse)

# Use st_cast("LINESTRING") with interactive mapview() to investigate the last fixes and line before leaving the nest site. 

# subset() each individual to corresponding leaving date.

# Export the linestring + GPS fixes to QGIS, find  lat and long for the center of nest


# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# incubation phase - buffer zones and trips #  

#read the pre subset -> incubation phase
Bird_ID <- readRDS("C:/your path here /name of file.rds")

# Create sf from lon/lat and correct the spatial reference 
Bird_ID <- st_as_sf(Bird_ID, coords = c("Longitude", "Latitude"), crs = 4326)
Bird_ID_transformed <- st_transform(Bird_ID, 25832)

# Build the LINESTRING 
Bird_ID_line <- Bird_ID_transformed %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("LINESTRING")

# Use the coordinates from the nest (obtained from QGIS)
lat <- 58.54952478 # (This is for the buffer zone)
long <- 9.00139276 

nest_wgs <- st_sfc(st_point(c(long, lat)), crs = 4326)
nest <- st_transform(nest_wgs, 25832)
nest_buf10 <- st_buffer(nest, 45) # 45= individual buffer zone.Tweak this until all gps drift is covered. 

# Trim the track inside the buffer and measure what is left 
line_outside <- st_difference(Bird_ID_line, nest_buf10) %>%
  st_collection_extract("LINESTRING")

total_distance_meters_outside <- st_length(line_outside)

# Results
total_distance_meters_outside

# check if the buffer zone is masking GPS drift (ensuring its not counted as a trip)
mapview::mapview(nest, col.regions = "red") +
  mapview::mapview(nest_buf10, alpha.regions = 0.2, col.regions = "red") +
  mapview::mapview(Bird_ID_line, color = "gray") +
  mapview::mapview(line_outside, color = "blue")

# Export the buffer zone to QGIS for illustrations. 
# Save the buffer size in a sheet (use values later). 
# Save the distance traveled in a sheet (use values later).
# (Random checks were done in QGIS with the ruler function to confirm that the lengths were correct)
# run this for each individuals (difference in drift = different need for buffer zone)


# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@- 

# # # # How much time is spent outside this buffer? 

# make a table with all RDS, add coord's from center of nest site, and add the individual buffer zone for each bird. 

eider_meta <- tribble(~ID, ~path, ~nest_lat, ~nest_lon, ~buffer_m,
"E75","C:/Users/your path + file.rds", 58.54952478,9.00139276, 45,
"E76","C:/Users/your path + file.rds", 58.54935132,9.00152537, 35,
"E77","C:/Users/your path + file.rds", 58.60055519,8.95001015, 50,
"E78","C:/Users/your path + file.rds", 58.60076179,8.95021586, 55,
"E79","C:/Users/your path + file.rds", 58.60051722, 8.94990783, 25,
# etc... /add all your individuals ..
)  # <--close 

calc_trips <- function(df, nest_lat, nest_lon, buffer_m = 25, time_col = "UTC_datetime", break_mins = 60, single_fix_minutes = 15) {
  pts <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
    st_transform(25832) %>%
    mutate(dt = as.POSIXct(.data[[time_col]], tz = "UTC")) %>%
    filter(!is.na(dt)) %>%
    arrange(dt)
  if (nrow(pts) == 0) return(tibble())
  nest <- st_sfc(st_point(c(nest_lon, nest_lat)), crs = 4326) %>% st_transform(25832)
  buf <- st_buffer(nest, buffer_m)
  coords <- st_coordinates(pts)
  pts <- pts %>%
    mutate(
      inside = st_within(geometry, buf, sparse = FALSE)[,1],
      outside = !inside,
      dist_from_nest_m = as.numeric(st_distance(geometry, nest)),
      step_m = c(NA, sqrt((coords[-1,1] - coords[-nrow(coords),1])^2 + (coords[-1,2] - coords[-nrow(coords),2])^2)),
      gap_min = as.numeric(difftime(dt, lag(dt), units = "mins")),
      big_gap = is.na(gap_min) | gap_min >= break_mins
    )
  pts$run_id <- cumsum(c(TRUE, pts$outside[-1] != pts$outside[-nrow(pts)] | pts$big_gap[-1]))
  pts %>%
    filter(outside) %>%
    group_by(run_id) %>%
    summarise(
      start_time_utc = first(dt),
      end_time_utc = last(dt),
      n_fixes = n(),
      minutes_outside = sum(ifelse(is.na(gap_min) | gap_min >= break_mins, 0, pmax(gap_min, 0)), na.rm = TRUE),
      distance_m = sum(step_m, na.rm = TRUE),
      max_dist_from_nest_m = max(dist_from_nest_m, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      minutes_outside = ifelse(n_fixes == 1, single_fix_minutes, minutes_outside),
      duration_h = minutes_outside / 60,
      distance_km = distance_m / 1000,
      max_dist_from_nest_km = max_dist_from_nest_m / 1000,
      trip_id = run_id
    ) %>%
    select(trip_id, everything(), -run_id)
}

all_trips <- lapply(seq_len(nrow(eider_meta)), function(i) {
  meta <- eider_meta[i, ]
  calc_trips(readRDS(meta$path), nest_lat = meta$nest_lat, nest_lon = meta$nest_lon, buffer_m = meta$buffer_m) %>%
    mutate(ID = meta$ID, buffer_m = meta$buffer_m)
}) %>% bind_rows()

summary_by_id <- all_trips %>%
  group_by(ID, buffer_m) %>%
  summarise(
    n_trips = n(),
    total_minutes = sum(minutes_outside, na.rm = TRUE),
    total_hours = total_minutes / 60,
    total_distance_m = sum(distance_m, na.rm = TRUE),
    total_distance_km = total_distance_m / 1000,
    max_trip_dist_km = max(max_dist_from_nest_km, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ID)

dataset_span_by_id <- lapply(seq_len(nrow(eider_meta)), function(i) {
  meta <- eider_meta[i, ]
  dt <- as.POSIXct(readRDS(meta$path)$UTC_datetime, tz = "UTC")
  dt <- dt[!is.na(dt)]
  tibble(
    ID = meta$ID,
    buffer_m = meta$buffer_m,
    n_fixes_total = length(dt),
    start_time_utc = min(dt),
    end_time_utc = max(dt),
    span_minutes = as.numeric(difftime(max(dt), min(dt), units = "mins")),
    span_hours = as.numeric(difftime(max(dt), min(dt), units = "mins")) / 60
  )
}) %>% bind_rows()

summary_with_pct <- summary_by_id %>%
  left_join(dataset_span_by_id, by = c("ID", "buffer_m")) %>%
  mutate(pct_outside_of_span = 100 * total_minutes / span_minutes)

summary_by_id # copy and paste to sheet ... 
summary_with_pct # or export as csv ... 



# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@- 

##  SPLITTING THE CHICK REARING DATA ###
# two phases for each individual= Early and Late.
# Using UTC_datetime to make sure data is sorted.
df <- df[order(df$UTC_datetime), ]
unique_dates <- unique(as.Date(df$UTC_datetime))

first_21_days <- unique_dates[1:21] # First 21 unique calendar days
after_21_days <- unique_dates[-(1:21)]# All days after the first 21

# Subset the data for the first 21 days
first21 <- df[as.Date(df$UTC_datetime) %in% first_21_days, ]
# Subset the data for all days except the first 21 days
after21 <- df[as.Date(df$UTC_datetime) %in% after_21_days, ]

# SAVE AS RDS ---> FIRST 
saveRDS (first21, file = "C://Users/insert your folder path/bird_21_FIRST.rds")
#  ----> AFTER 
saveRDS (after21, file = "C://Users/insert your folder path/bird_21_AFTER.rds")


#### KERNDAL DENSITY ESTIMATION (KDE) on chick rearing  #### 
# ReadRDS(from folder) ... bird_21_FIRST.rds & bird_21_AFTER.rds

# first: Subset latitude, Longitude, and device_id
id_of_bird <- subset(df, select = c(Latitude, Longitude, device_id))

# Step 1: Coffvert to SpatialPointsDataFrame
coordinates(id_of_bird) <- ~ Longitude + Latitude

# Step 2: projection
proj4string(id_of_bird) <- CRS("+proj=longlat +datum=WGS84")

# Step 3: Transform to correctt zone
id_of_bird <- spTransform(id_of_bird, CRS("+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# adehabitatHR 
kernel_id <- kernelUD(id_of_bird, h = "href", grid = 200)
id_KDE50 <- getverticeshr(kernel_id, 50)  

mapview (id_KDE50) # check 
print(id_KDE50$area) # save in google sheet     


#### EXPORT KDE FOR QGIS #### 
id_KDE50 <- st_as_sf(id_KDE50) 
st_write(id_KDE50, "C:/....../id_KDE50",driver = "ESRI Shapefile")




# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@- 
#### distance traveled plot ##### #    

# make a list of all df included,
# making sure id is present in both phases.
common_ids <- intersect(names(before_list), names(after_list))
before_list <- before_list[common_ids]
after_list  <- after_list[common_ids]


# running function on bird list
daily_dists <- function(data, bird_id) {
  data$Date <- as.Date(data$UTC_datetime)
  st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326) |>
    st_transform(25832) |>
    group_by(Date) |>
    summarise(geometry = st_combine(geometry), .groups = "drop") |>
    st_cast("LINESTRING") |>
    mutate(
      length_meters = as.numeric(st_length(geometry)),
      BirdID = bird_id
    ) |>
    st_drop_geometry() |>
    select(Date, BirdID, length_meters)
}

mk_period <- function(lst, label) {
  bind_rows(lapply(names(lst), \(b) {
    out <- daily_dists(lst[[b]], b)
    out$Period <- label
    out
  }))
}

combo <- bind_rows(
  mk_period(before_list, "Early"),
  mk_period(after_list,  "Late")
) |>
  arrange(BirdID, Date) |>
  group_by(BirdID) |>
  mutate(
    nDays_all = row_number(),
    cum_km = cumsum(length_meters) / 1000
  ) |>
  ungroup()

end_pts <- combo |>
  group_by(BirdID) |>
  slice_max(nDays_all, n = 1, with_ties = FALSE) |>
  ungroup()


# plot the cumulative travel for both phases 
period_cols <- c(Early = "#7872cc", Late = "#fa939e")

p2 <- ggplot(combo, aes(nDays_all, cum_km)) +
  geom_line(aes(color = Period, group = interaction(BirdID, Period)),
            linewidth = 0.9, alpha = 0.9) +
  geom_point(aes(color = Period, group = interaction(BirdID, Period)),
             size = 0.5, alpha = 0.6) +
  geom_text_repel(
    data = end_pts,
    aes(label = BirdID),
    nudge_x = 3, size = 3, min.segment.length = 0,
    segment.size = 0.2, direction = "y", seed = 1, box.padding = 0.2
  ) +
  scale_color_manual(values = period_cols) +
  scale_x_continuous(breaks = pretty_breaks(8)) +
  labs(
    title = "Cumulative distance per eider in the chick rearing phase",
    x = "Day number",
    y = "Cumulative distance (km)",
    color = "Phase"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.1, 0.8))


# for figure 2, add a boxplot (of total tracel) as p1, and do p1/p2 
# data was in a long format

p_dt_bar <- ggplot(sum_dt_km, aes(x = phase, y = mean_km, fill = phase)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = mean_km - se_km, ymax = mean_km + se_km), width = 0.15) +
  geom_text(aes(label = paste0("n = ", n), y = mean_km + se_km), vjust = -0.6, size = 4) +
  scale_fill_manual(values = c("Incubation" = "grey60", "Early" = "#7872cc", "Late" = "#fa939e")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(y = "Kilometre (km)", title = "Total Distance travelled across breeding phases (Mean \u00B1 SE)") +
  theme_classic(base_size = 13) +
  theme(legend.position = "none")





# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-

### GLM for the phases ### 
threephases<- read.delim("C:/Users/path ... and ..file.txt", stringsAsFactors=TRUE)

m_sqrt <- glm(sqrt(DT) ~ phase,
              family = gaussian(),
              data = threephases,
              na.action = na.exclude)
summary(m_sqrt)

m_anova_sqrt <- aov(sqrt(DT) ~ phase, data = threephases)
summary(m_anova_sqrt)

TukeyHSD(m_anova_sqrt)

## checking assumpsions for glm ... 
threephases$fit   <- fitted(m_sqrt)
threephases$resid <- resid(m_sqrt)

# Residuals vs fitted
ggplot(threephases, aes(x = fit, y = resid)) +
  geom_hline(yintercept = 0, linewidth = 0.4) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, linewidth = 0.6)

# Normal QQ plot of residuals
ggplot(threephases, aes(sample = resid)) +
  stat_qq() +stat_qq_line()







# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
#### GPS FIXES IN KILSUND ####  

# make all helper functions. 

read_period_list <- function(dir, pattern, suffix_regex_remove) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  setNames(
    lapply(files, readRDS),
    gsub(suffix_regex_remove, "", basename(files))
  )
}

get_dt <- function(df) {
  if (inherits(df, "sf")) df <- st_drop_geometry(df)
  if ("UTC_date" %in% names(df) && inherits(df$UTC_date, c("POSIXct", "POSIXt")))
    return(df$UTC_date)
  if ("UTC_datetime" %in% names(df))
    return(as.POSIXct(as.character(df$UTC_datetime), tz = "UTC"))
  if ("Date" %in% names(df) && inherits(df$Date, c("POSIXct", "POSIXt")))
    return(df$Date)
  NULL
}

thin_to_1h <- function(df) {
  dt <- get_dt(df)
  if (is.null(dt)) stop("No valid datetime column found.")
  df %>%
    mutate(.dt = dt) %>%
    filter(!is.na(.dt)) %>%
    arrange(.dt) %>%
    mutate(.hour = floor_date(.dt, "hour")) %>%
    group_by(.hour) %>%
    slice(1) %>%
    ungroup() %>%
    select(-.hour, -any_of(".dt"))
}

to_sf <- function(x) {
  if (!inherits(x, "sf")) {
    x <- x[!is.na(x$Latitude) & !is.na(x$Longitude), ]
    x <- st_as_sf(x, coords = c("Longitude", "Latitude"), crs = 4326)
  }
  x
}

force_char_attrs <- function(x) {
  g  <- st_geometry(x)
  df <- st_drop_geometry(x) %>% mutate(across(everything(), as.character))
  st_as_sf(df, geometry = g, crs = st_crs(x))
}

flag_inside_zone <- function(period_list_1h, zone_poly, zone_name = "Kilsund") {
  sf_list <- lapply(period_list_1h, to_sf) %>% lapply(force_char_attrs)
  pts <- bind_rows(sf_list, .id = "device_id") %>% st_transform(st_crs(zone_poly))
  
  pts_flag <- pts %>%
    mutate(
      in_zone = lengths(st_intersects(geometry, zone_poly)) > 0,
      zone = if_else(in_zone, paste0("Inside ", zone_name), paste0("Outside ", zone_name))
    )
  
  list(
    pts_flag = pts_flag,
    sum_all = pts_flag %>%
      st_drop_geometry() %>%
      count(zone, name = "n_fixes") %>%
      mutate(
        share_pct = round(100 * n_fixes / sum(n_fixes), 1),
        zone = fct_reorder(zone, share_pct)
      ),
    sum_device_id = pts_flag %>%
      st_drop_geometry() %>%
      count(device_id, zone) %>%
      group_by(device_id) %>%
      mutate(share_pct = round(100 * n / sum(n), 1)) %>%
      ungroup()
  )
}

# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# Read all data

EARLY <- read_period_list(
  dir     = "C:/Users/mette/Desktop/Birdlife/paper/FIRST_21",
  pattern = "_21_FIRST\\.rds$",
  suffix_regex_remove = "_21_FIRST\\.rds$"
)

LATE <- read_period_list(
  dir     = "C:/Users/mette/Desktop/Birdlife/paper/AFTER_21",
  pattern = "_21_AFTER\\.rds$",
  suffix_regex_remove = "_21_AFTER\\.rds$"
)


zone_poly <- st_read("C:/Users/mette/Desktop/Birdlife/paper/Kilsund.shp", quiet = TRUE)
zone_name <- "Kilsund"

id_key <- tribble(
  ~CR,   ~device_id,
  "E75", 242598,
  "E76", 242599,
  "E77", 242601,
  "E78", 242600,
  "E79", 242602,
  "E80", 242603,
  "E81", 242604,
  "E82", 242605,
  "E83", 242606,
  "E84", 242607,
  "E85", 242608,
  "E86", 242609,
  "E87", 242613,
  "E88", 242610,
  "E89", 242612,
  "E90", 242614,
  "E91", 242611,
  "E92", 242615,
  "E93", 242617
)

id_key <- id_key %>% mutate(device_id = as.character(device_id))


# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# force 1 hour internal 

EARLY_1h <- lapply(EARLY, thin_to_1h)
LATE_1h  <- lapply(LATE,  thin_to_1h)

tibble(
  device_id           = names(EARLY),
  n_early_before = map_int(EARLY, nrow),
  n_early_after  = map_int(EARLY_1h, nrow),
  n_late_before  = map_int(LATE, nrow),
  n_late_after   = map_int(LATE_1h, nrow)
) %>% print(n = Inf)

# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-

res_E <- flag_inside_zone(EARLY_1h, zone_poly, zone_name)
res_L <- flag_inside_zone(LATE_1h,  zone_poly, zone_name)

res_E$sum_all
res_L$sum_all

# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-

inside_label <- paste0("Inside ", zone_name)

extract_inside_pct <- function(res, id_key, col_name) {
  res$sum_device_id %>%
    left_join(id_key, by = "device_id") %>%
    filter(zone == inside_label) %>%
    select(device_id, CR, share_pct) %>%
    rename(!!col_name := share_pct)
}

paired_df <- inner_join(
  extract_inside_pct(res_E, id_key, "early_pct"),
  extract_inside_pct(res_L, id_key, "late_pct"),
  by = c("device_id", "CR")
)

paired_df



# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# @-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
# test for differences , early to late 

t.test(paired_df$early_pct, paired_df$late_pct, paired = TRUE)
wilcox.test(paired_df$early_pct, paired_df$late_pct, paired = TRUE)


