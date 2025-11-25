library(tidyverse)
library(sf)
library(stars)
library(mapview)

# site polygons
aapa_polygons <- 
  st_read("path/aapa_polygons.gpkg") |> 
  st_transform(32635) # epsg 32635 for Finnish zone35


# Sentinel-2 SCL class codes and definitions
dplyr::tibble(
  SCL = 0:11,
  SCL_name = c(
    "No data",
    "Saturated or defective",
    "Dark area pixels",
    "Cloud shadow", # in spring water pools might get this flag if there are clouds in scene
    "Vegetation",
    "Bare soils",
    "Water",
    "Cloud low probability / Unclassified", # flarks mistakenly get this flag in Finnish mires 
    "Cloud medium probability",
    "Cloud high probability",
    "Thin cirrus",
    "Snow or ice"
  )
) -> scl_code
print(scl_code)


# for testing read one of the datacubes built in Retrieve_aapa.R
obj <- read_stars("path/aapa_pristine_3.nc") 


## Datacube subset of relevant season (April-September in northern Finland)
time_vals <- st_get_dimension_values(obj, "t") # Extract dates
keep_idx <- which(as.integer(format(time_vals, "%m")) %in% 4:9) # indices where the month is between 4 and 9
st_dimensions(obj)
obj <- obj[,,,keep_idx] # datacube dimensions: obj[bands, x_coord, y_coord, time]


#
# Cloud masking
#

# Find all scenes with at least 95% of pixels classified as SCL 4, 5, or 6 (vegetation, bare soils, water)
# (In Finland: SCL 4,5,6, or 7  - Unclassified/low cloud probability. In Finnish case mask low probability clouds with CLD layer)

# Alos mask out all remaining pixels with SCL values not equal to 4, 5, or 6 
clear_dates <-
  obj |> 
  as_tibble() |> 
  group_by(t) |> 
  summarize(prop_scl = sum(if_else(SCL %in% c(4,5,6), 1, 0)) / n()) |> 
  filter(prop_scl > 0.95) |> 
  pull(t) 

obj_clear <-
  obj |>
  filter(t %in% clear_dates) |>
  mutate(across(everything(), ~ if_else(SCL %in% c(4, 5, 6), ., NA)))

# Mask out the polygon
obj_poly <-
  obj_clear |>
  st_crop(aapa_polygons[3,]) # 3 - index of the site used for testing

# Mask away tree-cover with polygons of open mire area 
#(I would have prefered raster for masking but did not easily find a way to use raster mask)
openmires <- st_read("path/openmire_mask.gpkg")
obj_poly <-
  obj_poly |>
  st_crop(openmires)

plot(obj_poly[7]) # 7 - SWIR band

#
# Calculate wetness classification and moisture indices
#

# Inundation function Jussila [10]
obj_wetness <-
  obj_poly |>
  mutate(inundation_Jussila = case_when(
    is.na(B11) ~ NA_real_,
    B11 < 1396 & B8A < 1817 ~ 100,
    B11 < 1247 & B8A >= 1817 ~ 100,
    B11 >= 1396 & B04 < 391 & ((B03 - B12) / (B03 + B12)) < -0.43 & B12 < 1496 ~ 100,
    TRUE ~ 0
  ))
# NEW: Inundation function Lefebvre WiW [11]
obj_wetness <-
  obj_wetness |>
  mutate(inundation_Wiw = case_when(
    is.na(B11) ~ NA_real_,
    B12 <= 1131 & B8A <= 1804 ~ 100,
    TRUE ~ 0
  ))

# Calculate moisture indices [12, 13, 14, 15]
obj_wetness <-
  obj_wetness |>
  mutate(NDMI = (B8A - B11) / (B8A + B11), # Gao's Moisture index
         NDWI = (B03 - B8A) / (B03 + B8A), # Mcfeeters Water index
         NDPI = (B03-B11)/(B03+B11), # Pond index. from slovakia Clizsky potok example 
         STR = ((1-B11/10000)^2)/(2*B11/10000) ) # Transformed SWIR. Should be linearly correlated with soil moisture (Sadeghi et al.,2017, https://doi.org/10.1016/j.rse.2017.05.041)

# plot rasters [band, spatX, spatY, time]
plot(obj_wetness[11,,,243:270])
plot(obj_wetness[12,,,244:257])


#
# PLOT TIME SERIES GRAPH (WITH ALL OBSERVATIONS - including possible errors)
#

# check dates 
st_get_dimension_values(obj_poly, "t")

# summarise inundation-% and moisture index averages for site area 
table_all <- # ("prop_inundation" in the original Analyze_aapa.R)
  obj_wetness[,,,] |>  # adjust time range with 4th dimension, eg.: obj_wetness[,,,1:15] -> first 15 dates. 
  as_tibble() |>
  group_by(t) |>
  summarize(inundation = mean(inundation_Jussila, na.rm = TRUE),
            inundation_wiw = mean(inundation_Wiw, na.rm = TRUE),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 

ggplot(table_all) + # (check table for dates and plot single year by subsetting by rows)
  aes(x = t, y = inundation) + theme_minimal() + 
  geom_line() + geom_point() +
  ylim(0,100)+
  labs(title = "Inundation %", x = "Date",
       y = "Inundated area (%)") 

ggplot(table_all) +
  aes(x = t, y = NDMI) + theme_minimal() + 
  geom_line() + geom_point() +
  ylim(-0.3,0.5) + # adjust id needed
  labs(title = "NDMI", x = "Date",
       y = "NDMI") 

ggplot(table_all) +
  aes(x = t, y = STR) + theme_minimal() + 
  geom_line() + geom_point() +
  ylim(0,5) + # adjust id needed
  labs(title = "STR", x = "Date",
       y = "STR") 


#
# AGGREGATION 
#

## Temporal aggregation: monthly median value

obj_month<- aggregate(obj_wetness, by = "month", FUN=median, na.rm=T) # or "week" 
plot(obj_month[10])  


## Temporal aggregation: 2weekly median value with customized approach

# generate 2-week breaks for the observed years
time_vals <- st_get_dimension_values(obj_wetness, "t")
start <- floor_date(min(time_vals), "month")
end   <- ceiling_date(max(time_vals), "month")
# All 1st and 15th of each month between start and end
breaks <- sort(c(seq(start, end, by = "1 month"),
                 seq(start + days(14), end, by = "1 month")))
# Aggregate using 2-week intervals
obj_2week <- aggregate(obj_wetness, by = breaks, FUN = median, na.rm = TRUE)

# remove winter months again: find indices where the month is between 4 (or5?) and 9
#  Adjust the months to your relevant season
keep_idx <- which(as.integer(format(st_get_dimension_values(obj_2week, "time"), "%m")) %in% 5:9)
obj_2week <- obj_2week[,keep_idx,,]   # adjust commas for your dimensions

#
# PLOT TIME SERIES GRAPH (aggregated)
#

# summarise monthly:
# inundation-% and moisture index averages for site area 
table_m <- 
  obj_month[,,,] |>  # adjust time range with 2nd dimension, eg.: obj_wetness[,1:4,,] -> first 4 months 
  as_tibble() |>
  group_by(time) |> 
  summarize(inundation = mean(inundation_Jussila, na.rm = TRUE),
            inundation_Wiw = mean(inundation_Wiw, na.rm = TRUE),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 

# Summarise 2-weekly  
table_2w <- 
  obj_2week[,,,] |>   
  as_tibble() |>
  group_by(time) |> 
  summarize(inundation = mean(inundation_Jussila, na.rm = TRUE),
            inundation_Wiw = mean(inundation_Wiw, na.rm = TRUE),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 

table_2w_plotting <- table_2w[!is.na(table_2w$inundation),] # drop NAs for plotting

ggplot(table_2w_plotting) + ## check the dates in table to plot a specific single year 
  aes(x = time, y = inundation) + 
  ylim(0,100) +
  geom_line() + geom_point() +
  labs(title = "Inundated area %",
       x = "Date",
       y = "Inundated area (%)") +
  theme_minimal()

ggplot(table_2w_plotting) +
  aes(x = time, y = STR) + 
  ylim(1,5) +
  geom_line() + geom_point() +
  labs(title = "STR",
       x = "Date",
       y = "STR") +
  theme_minimal()


# NEW
# Gapfilling the missing 2-week periods in aggregated datacube
#

# extract 2-weekly time points
time_vals2w <- st_get_dimension_values(obj_2week, "time")

# interpolate NA values along time dimension
obj_filled_2w <- st_apply(
  obj_2week,
  MARGIN = c("x", "y"),   
  FUN = function(ts) { # repeat interpolation function over each pixel time series
    if (all(is.na(ts))) { # keep NA time series as NA outside site polygon
      return(ts)  
    }
    approx( # interpolate missing time points
      x = as.numeric(time_vals2w),
      y = ts,
      xout = as.numeric(time_vals2w),
      method = "linear",
      rule = 2
    )$y
  },.fname = "time"
)
# fix the broken time dimension in output
obj_filled_2w <- st_set_dimensions(obj_filled_2w, "time", values = time_vals2w)

#
# Plot 2-week time series (gapfilled)
#

# first summarise for site area:
table_gf2w <- # Gapfilled 2-weekly table
  obj_filled_2w[,,,] |>  
  as_tibble() |>
  group_by(time) |> 
  summarize(inundation = mean(inundation_Jussila, na.rm = TRUE),
            inundation_Wiw = mean(inundation_Wiw, na.rm = TRUE),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 
# New column: Flag gapfilled values
table_gf2w$gapfilled <- "2w median"
table_gf2w$gapfilled[is.na(table_2w$B11)] <- "gapfilled"
table_gf2w$gapfilled <- as.factor(table_gf2w$gapfilled)
# time format to Date for plotting
table_gf2w$date <- as.Date(table_gf2w$time) 

ggplot(table_gf2w[65:74,]) + #check the dates in table to plot a single year ([65:74,] -> 2018 in this case)
  aes(x = date, y = inundation) + scale_x_date(date_labels = "%b %y") + 
  geom_line() + geom_point(aes(colour = gapfilled), size=2) + 
  scale_color_manual(values = c("gapfilled" = "red", "2w median" = "black"), name=NULL)+
  labs(title = "Inundation %",x = "Date", 
       y = "Inundated area (%)") +
  ylim(0,100)+
  theme_minimal()

ggplot(table_gf2w[65:74,]) +
  aes(x = date, y = STR) +  scale_x_date(date_labels = "%b %y") + 
  geom_line() + geom_point(aes(colour = gapfilled), size=2) +
  scale_color_manual(values = c("gapfilled" = "red", "2w median" = "black"), name=NULL)+
  labs(title = "STR", x = "Date", 
       y = "STR") + 
  ylim(1,5)+ # adjust if needed
  theme_minimal()
ggplot(table_gf2w[65:74,]) +
  aes(x = date, y = NDMI) +  scale_x_date(date_labels = "%b %y") + 
  geom_line() + geom_point(aes(colour = gapfilled), size=2) +
  scale_color_manual(values = c("gapfilled" = "red", "2w median" = "black"), name=NULL)+
  labs(title = "NDMI", x = "Date", 
       y = "NDMI") + 
  ylim(0, 0.5)+ # adjust if needed
  theme_minimal()
ggplot(table_gf2w[65:74,]) +
  aes(x = date, y = -B11) +  scale_x_date(date_labels = "%b %y") + 
  geom_line() + geom_point(aes(colour = gapfilled), size=2) +
  scale_color_manual(values = c("gapfilled" = "red", "2w median" = "black"), name=NULL)+
  labs(title = "SWIR",x = "Date",
       y = "B11 (neg)") + # plot SWIR as negation for comparability (low SWIR, high moisture. Opposite to other indicators)
  ylim(-2500, -800)+ # adjust if needed
  theme_minimal()


####################
#
# NEW
# Plotting yearly indicators - maps
#
#####

# Using 2-weekly gapfilled stars object to calculate statistics

# aggregate stars into yearly layers
## Mean (for each pixel?)
obj_y_mean <- aggregate(obj_filled_2w, by = "year", 
                        FUN=mean, na.rm=T) 
## Min (for each pixel?)
obj_y_min <- aggregate(obj_filled_2w, by = "year", 
                       FUN=min, na.rm=T) # returns Inf for NA areas
obj_y_min[obj_y_min == Inf] <- NA # convert Inf back to NA
## Max (for each pixel?)
obj_y_max <- aggregate(obj_filled_2w, by = "year", 
                       FUN=max, na.rm=T) # returns -Inf for NA areas
obj_y_max[obj_y_max == -Inf] <- NA # convert -Inf back to NA


## Plot yearly raster maps

# SWIR [7], inundation Jussila [10], inundation Wiw [11], NDMI [12], NDWI[13], NDPI[14], STR[15] 
library(viridis) # yellow - dry, dark blue - wet
# Inundation (Jussila). Inundated % of time
plot(obj_y_mean[10]*100, col = viridis(100, option = "D", direction=-1),
     breaks = seq(0, 100, length.out = 101))  
# Inundation (Wiw). Inundated % of time
plot(obj_y_mean[11]*100, col = viridis(100, option = "D", direction=-1),
     breaks = seq(0, 100, length.out = 101)) 
# STR
plot(obj_y_mean[15], col = viridis(100, option = "D", direction=-1),
     breaks = seq(0, 9, length.out = 101))  
# NDMI
plot(obj_y_mean[12], col = viridis(100, option = "D", direction= -1),
     breaks = seq(min(obj_y_mean[[12]], na.rm=T), 
                  max(obj_y_mean[[12]], na.rm=T), length.out = 101)) 

####################
#
# NEW
# Plotting yearly indicators- statistical trend graphs
#
#####

# MEAN - summarise values for site area
table_y_mean <- 
  obj_y_mean[,,,] |> 
  as_tibble() |>
  group_by(time) |> 
  summarize(inundation = mean(inundation_Jussila, na.rm = TRUE),
            inundation_Wiw = mean(inundation_Wiw, na.rm = TRUE),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 

#summarise min for sites (site mean at minimum wet situation)
table_y_min <- 
  obj_y_min[,,,] |> 
  as_tibble() |>
  group_by(time) |> 
  summarize(inundation = mean(inundation_Jussila, na.rm = TRUE),
            inundation_Wiw = mean(inundation_Wiw, na.rm = TRUE),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture. 
# for SWIR, wetness minimum is SWIR max value. 

#summarise max for sites (site mean at minimum wet situation)
table_y_max <- 
  obj_y_max[,,,] |>  
  as_tibble() |>
  group_by(time) |> 
  summarize(inundation = mean(inundation_Jussila, na.rm = TRUE),
            inundation_Wiw = mean(inundation_Wiw, na.rm = TRUE),
            NDMI = mean(NDMI, na.rm=T), #Ranges from -1 to +1, where the lowest values indicate low vegetation water content, and the highest ones correspond to high water content
            NDWI = mean(NDWI, na.rm=T), #Over 0.2: water. Below 0: non-water
            NDPI = mean(NDPI, na.rm=T), # scores above a certain high value, e.g., larger than 0.75, indicates a pond area with good water quality
            STR = mean(STR, na.rm=T), # Higher values, higher soil moisture content
            B11 = mean(B11, na.rm=T)) # lower values, higher moisture.
# for SWIR, wetness maximum is SWIR min value.


# plot all yearly stats in one graph: Mean and shaded range between min-max
# STR
ggplot() +
  ylim(1,5)+ # adjust if needed
  # geom_line(data= table_gf2w, aes(x = time - months(6), y = STR), color="grey") + # full time series to background?
  geom_line(data= table_y_mean, aes(x = time, y = STR)) + 
  geom_point(data= table_y_mean, aes(x = time, y = STR)) + 
  geom_ribbon(aes(x = table_y_mean$time, 
                  ymin = table_y_min$STR, 
                  ymax = table_y_max$STR),fill="#1f9e89", alpha= 0.2)+ 
  labs(title = "STR moisture mean and range (min, max)",
       x = "Year",
       y = "STR") +
  theme_minimal() + theme(legend.position="none")

# NDMI
ggplot() +
  ylim(-0.1,0.5)+ # adjust if needed
  # geom_line(data= table_gf2w, aes(x = time - months(6), y = NDMI), color="grey") + # full time series to background?
  geom_line(data= table_y_mean, aes(x = time, y = NDMI)) + 
  geom_point(data= table_y_mean, aes(x = time, y = NDMI)) + 
  geom_ribbon(aes(x = table_y_mean$time, 
                  ymin = table_y_min$NDMI, 
                  ymax = table_y_max$NDMI),fill="#1f9e89", alpha= 0.2)+ 
  labs(title = "NDMI moisture mean and range (min, max)",
       x = "Year",
       y = "NDMI") +
  theme_minimal() + theme(legend.position="none")


# plot percentage of permanently and seasonally wet area. 
# Jussila model
ggplot() +
  ylim(0,100)+ 
  geom_ribbon(aes(x = table_y_mean$time, # permanent
                  ymin = 0, 
                  ymax = table_y_min$inundation),fill="#1f9e89", alpha= 0.6)+ 
  geom_ribbon(aes(x = table_y_mean$time, # seasonal
                  ymin = table_y_min$inundation, 
                  ymax = table_y_max$inundation),fill="#6ece58", alpha= 0.3)+ 
  geom_ribbon(aes(x = table_y_mean$time, # seasonal
                  ymin = table_y_max$inundation, 
                  ymax = 100),fill="#fde725", alpha= 0.2)+ 
  labs(title = "Permanently and seasonally inundated area (Jussila model)",
       x = "Year",
       y = "Inundated area %") +
  theme_minimal() + theme(legend.position="none") # + scale_x_date(date_breaks = "1 year")


# Lefebvre Wiw model
ggplot() +
  ylim(0,100)+ 
  geom_ribbon(aes(x = table_y_mean$time, # permanent
                  ymin = 0, 
                  ymax = table_y_min$inundation_Wiw),fill="#1f9e89", alpha= 0.6)+ 
  geom_ribbon(aes(x = table_y_mean$time, # seasonal
                  ymin = table_y_min$inundation_Wiw, 
                  ymax = table_y_max$inundation_Wiw),fill="#6ece58", alpha= 0.3)+ 
  geom_ribbon(aes(x = table_y_mean$time, # seasonal
                  ymin = table_y_max$inundation_Wiw, 
                  ymax = 100),fill="#fde725", alpha= 0.2)+ 
  labs(title = "Permanently and seasonally inundated area (Lefebvre Wiw)",
       x = "Year",
       y = "Inundated area %") +
  theme_minimal() + theme(legend.position="none") # + scale_x_date(date_breaks = "1 year")


# # Max inundation
# prop_inundation |>
#   mutate(year = lubridate::year(t)) |>
#   group_by(year) |>
#   summarize(max_inundation = max(inundation, na.rm = TRUE)) |>
#   ggplot() +
#   aes(x = year, y = max_inundation) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Max Proportion of Inundation Over Time",
#        x = "Date",
#        y = "Proportion of Inundation") +
#   theme_minimal()


#
# All in a function (Analyze_aapa.R)
# (Not yet adjusted for the ver2 script)
# 

inundation_fun <- function(ncfile) {
  poly_id <- 
    #unlist(str_extract_all(ncfile, "//d+"))[2] |>
    str_extract(ncfile, "//d+") |>
    as.integer()
  
  obj <- 
    read_stars(paste0("S2/",ncfile))
  
  polygon_id <- 
    aapa_polygons[poly_id, ] |> 
    st_transform(st_crs(obj))
  
  clear_dates <-
    obj |> 
    as_tibble() |> 
    group_by(t) |> 
    summarize(prop_scl = sum(if_else(SCL %in% c(4,5,6), 1, 0)) / n()) |> 
    filter(prop_scl > 0.95) |> 
    pull(t) 
  
  obj_clear <-
    obj |>
    filter(t %in% clear_dates) |>
    mutate(across(everything(), ~ if_else(SCL %in% c(4, 5, 6), ., NA)))
  
  # Mask out the polygon
  obj_poly <-
    obj_clear |>
    st_crop(polygon_id)
  
  # Inundation function Jussila
  obj_Jussila <-
    obj_poly |>
    mutate(inundation_Jussila = case_when(
      is.na(B11) ~ NA_real_,
      B11 < 1396 & B8A < 1817 ~ 1,
      B11 < 1247 & B8A >= 1817 ~ 1,
      B11 >= 1396 & B04 < 391 & ((B03 - B12) / (B03 + B12)) < -0.43 & B12 < 1496 ~ 1,
      TRUE ~ 0
    ))
  
  prop_inundation <-
    obj_Jussila |> 
    as_tibble() |>
    group_by(t) |>
    summarize(inundation = sum(inundation_Jussila, na.rm = TRUE) / n(), .groups = "drop") |>
    mutate(file = ncfile, id = poly_id, .before = t)
  
  return(prop_inundation)
  
}

inundation_fun("polygon_9.nc")

system.time(
res_inundation <-
  map_dfr(dir("S2"), inundation_fun, .id = "polygon")
)
