
# travel_times() example --------------------------------------------------

# Example from http://tidytransit.r-transit.org/reference/travel_times.html

library(tidyverse)
library(tidytransit)

nyc_path <- system.file("extdata", "google_transit_nyc_subway.zip", package = "tidytransit")
nyc <- read_gtfs(nyc_path)

# Use journeys departing after 7 AM with arrival time before 9 AM on 26th June
stop_times <- filter_stop_times(nyc, "2018-06-26", 7*3600, 9*3600)

tts <- travel_times(stop_times, "34 St - Herald Sq", return_coords = TRUE)
tts <- tts %>% filter(travel_time <= 3600)

# travel time to Queensboro Plaza is 810 seconds, 13:30 minutes
tts %>% filter(to_stop_name == "Queensboro Plaza") %>% pull(travel_time) %>% hms::hms()

# plot a simple map showing travel times to all reachable stops
# this can be expanded to isochrone maps
ggplot(tts) +
  geom_point(aes(x = to_stop_lon, y = to_stop_lat, color = travel_time))

# travel_times() is a wrapper around raptop() (see http://tidytransit.r-transit.org/reference/raptor.html)


# Wellington example ------------------------------------------------------

library(tidyverse)
library(fs)
library(lubridate)
library(here)
library(hms)
library(glue)
library(tidytransit)
library(tictoc)

gtfs_dir <- here("gtfs")
# gtfs_file <- "wellington-20210503.zip"
# gtfs_file <- "wellington-20210126.zip"
gtfs_file <- "wellington-20210725.zip"

raw_gtfs <- read_gtfs(path(gtfs_dir, gtfs_file))
raw_gtfs$feed_info
raw_gtfs$transfers # seems to have plenty of transfers
raw_gtfs$calendar %>% count(start_date, end_date)

raw_gtfs$stops %>%
  filter(startsWith(stop_name, "Willis")) %>% 
  select(stop_id, stop_name)

stop_times <- filter_stop_times(raw_gtfs, "2021-07-27", 7*3600, 9*3600)

tts <- travel_times(stop_times, "Willis Street - Abel Smith Street", return_coords = TRUE)
tts <- tts %>% filter(travel_time <= 3600)
tts %>% count(transfers)

tts %>% filter(to_stop_name == "Porirua Station - Stop B") %>% pull(travel_time) %>% hms::hms()

tts %>% 
  ggplot() +
  geom_point(aes(x = to_stop_lon, y = to_stop_lat, color = travel_time)) +
  coord_map()

# using stop names can be awkward and isn't guaranteed to be unique.
# thus, raptor() might be a better option


# Wellington with raptor() ------------------------------------------------

tts_raptor <- raptor(stop_times, transfers = raw_gtfs$transfers,
                     stop_ids = c("7711"), keep = "shortest")

tts_raptor_df <- tts_raptor %>% 
  as_tibble() %>% 
  left_join(raw_gtfs$stops %>% select(stop_id, to_stop_lat = stop_lat, to_stop_lon = stop_lon),
            by = c("to_stop_id" = "stop_id"))

tts_raptor_df %>% 
  ggplot() +
  geom_point(aes(x = to_stop_lon, y = to_stop_lat, color = travel_time)) +
  coord_map()


# test timing for multiple stops ------------------------------------------

from_stops <- raw_gtfs$stops %>% head(1000) %>% pull(stop_id)
# from_stops <- raw_gtfs$stops$stop_id %>% unique()

tic()
tts_raptor <- raptor(stop_times, transfers = raw_gtfs$transfers,
                     stop_ids = from_stops, keep = "shortest")
toc()
# 100: 1.32s
# 1000: 3.44s
# 2000 or more: "If you are sure you wish to proceed, rerun with allow.cartesian=TRUE"
# might need to map over batches instead: easiest way might be via group_modify():
# https://dplyr.tidyverse.org/reference/group_map.html

tts_raptor_df <- tts_raptor %>% 
  as_tibble() %>% 
  left_join(raw_gtfs$stops %>% select(stop_id, to_stop_lat = stop_lat, to_stop_lon = stop_lon),
            by = c("to_stop_id" = "stop_id"))

tts_raptor_df %>% 
  filter(from_stop_id != to_stop_id)

# migth need to be careful with hierarchical stations and bus/train transfers
raw_gtfs$stops %>% filter(stop_id == "1001") %>% glimpse()
raw_gtfs$stops %>% filter(startsWith(stop_name, "Paraparaumu Station")) %>% View()


