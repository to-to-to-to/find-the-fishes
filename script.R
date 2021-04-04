# Load the libraries
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(patchwork)

# Read in the depth data
bottom <- read_csv("datasets/bottom_line.csv", 
                   col_types = cols(Ping_date = col_datetime(format = "%m/%d/%Y"))) %>% 
  rename_all(tolower)

# Glimpse the data
glimpse(bottom)

# Clean the bottom data
bottom_clean <- bottom %>%
  filter(position_status == 1) %>%
  select(ping_date, ping_time, latitude, longitude, depth) %>%
  mutate(date_time = ping_date + ping_time,
         distance_between = c(0, 
                              geosphere::distHaversine(cbind(longitude[-n()], latitude[-n()]),
                                                       cbind(longitude[ -1], latitude[ -1]))),                         
         distance_along = cumsum(distance_between))

# Inspect the data
glimpse(bottom_clean)

# Reduce the size of the plots
options(repr.plot.width = 7, repr.plot.height = 5)

# Plot the ship's track
p_ship_track  <- ggplot(bottom_clean, aes(longitude, latitude)) +
  geom_point(size = 0.5) + 
  labs(x = "Longitude", y = "Latitude")

# Plot the depth of the sea floor along the ship's track
p_bathymetry  <-  ggplot(bottom_clean, aes(distance_along, depth)) +
  geom_point(size = 0.5) + 
  scale_y_reverse() +
  
  labs(x = "Distance along trackline (m)", y = "Depth (m)")

# Arrange the plots side by side for easier viewing
p_ship_track + p_bathymetry

# Read in the acoustic data
acoustic <- read_csv("datasets/acoustic.csv", 
                     col_types = cols(Date_M = col_datetime(format = "%Y%m%d")))  %>% 
  filter(Lon_M != 999.0)

# Glimpse the data
glimpse(acoustic)

# Create a list of variables to keep
variables_keep <- c("Interval", "Layer", "Sv_mean", "Frequency", 
                    "Date_M", "Time_S", "Time_E", "Lat_M", "Lon_M")

# Select, rename, filter, mutate, and arrange the data 
Sv_layer1 <- acoustic %>%
  select(one_of(variables_keep)) %>% 
  rename(Spatial_interval = Interval, Date = Date_M) %>%
  filter(Layer == "1")  %>% 
  mutate(Datetime_start = Date + Time_S,
         Datetime_end = Date + Time_E)  %>% 
  arrange(Datetime_start) 

# Glimpse the cleaned acoustic data
glimpse(Sv_layer1)

# More data wrangling...
Sv <- Sv_layer1 %>% 
  mutate(Distance_between = c(0, geosphere::distHaversine(cbind(Lon_M[-n()], Lat_M[-n()]),       
                                                          cbind(Lon_M[  -1], Lat_M[  -1]))),
         Distance_along = cumsum(Distance_between)) %>%
  na_if(-999) %>% 
  mutate(Time_interval = interval(Datetime_start, Datetime_end))

# Glimpse the data
glimpse(Sv)

# Function: assign Spatial_interval to bottom points that fall within Time_interval
get_Interval_by_time <- function(bottom_data){
  res <- Sv$Spatial_interval[bottom_data %within% Sv$Time_interval]
  if(length(res)==0) return(NA)         
  return(res)
}

# Map the track line interval value to bottom_clean
bottom_spatial_interval_segments <- bottom_clean  %>% 
  mutate(trackline_interval = purrr::map_dbl(date_time, get_Interval_by_time))

# Inspect the first 15 rows
head(bottom_spatial_interval_segments, 15)

# Group bottom_clean and calculate the mean depth
bottom_intervals <- bottom_spatial_interval_segments %>%
  group_by(trackline_interval) %>%
  summarize(depth_mean = mean(depth)) %>%
  ungroup()

# Join the bottom intervals data to the acoustic data
Sv_and_depth <- Sv %>%
  left_join(bottom_intervals, by = c("Spatial_interval" = "trackline_interval")) %>% 
  mutate(depth_plot = ifelse(depth_mean >= 250, 250, depth_mean))

# Glimpse the data 
glimpse(Sv_and_depth)

# Top panel
Sv_mean_plot <- ggplot(Sv_and_depth, aes(Distance_along, Sv_mean)) +
  geom_line() +
  labs(y=expression(mean~volume~backscatter~S[v]~(dB))) +
  theme(axis.title.x=element_blank())

# Bottom panel
bathymetry <- ggplot(Sv_and_depth, aes(Distance_along, depth_plot)) +
  geom_line(size = 0.5) +
  scale_y_reverse() +
  labs(x = "Distance along trackline (m)", y = "Depth (m)")

# Display the two panels in one figure
Sv_mean_plot / bathymetry