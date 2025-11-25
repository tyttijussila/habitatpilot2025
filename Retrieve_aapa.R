# Load required libraries
library(openeo)
library(sf)
library(dplyr)

# Connect to openEO backend
connection <- connect("https://openeo.dataspace.copernicus.eu") # (Tytti: VPN off)
login()

setwd("D:/Users/E1007595/Suot/2024/Habitaattipilotti")

# Read Aapa mire polygons from file
aapa_polygons <- st_read("INDICATOR_ts_pilot/aapa_huonotila_sample20_Zone3_AntinDIRsuot.gpkg") %>% st_transform(crs = 4326)
aapa_polygons <- st_read("INDICATOR_ts_pilot/aapa_undrained_top20_ojitus45_Zone3_AntinDIRsuot.gpkg") %>% st_transform(crs = 4326)

# Define bands to download
bands <- c("B02", "B03", "B04", "B05", "B08", "B8A", "B11", "B12", "SCL", "CLD")


# Submit all jobs and store them in a list
jobs <- list()

for (i in c(11:19)) {
  polygon <- aapa_polygons[i, ]
  bbox <- st_bbox(polygon)
  
  # Get process graph builder
  process <- processes()

  # Define spatial extent
  spatial_extent <- list(
    west = bbox["xmin"],
    south = bbox["ymin"],
    east = bbox["xmax"],
    north = bbox["ymax"],
    crs = 4326
  )

  # Define temporal extent (all available years)
  temporal_extent <- list("2015-04-01", "2025-10-01")
  
  # Cloud filter: keep items with 0–50% cloud cover
  properties <- list(
    "eo:cloud_cover" = function(x) x <= 50
  )

  
  # Load Sentinel-2 L2A data using process graph builder
  collection <- process$load_collection(
    id = "SENTINEL2_L2A",
    spatial_extent = spatial_extent,
    temporal_extent = temporal_extent,
    bands = bands,
    properties = properties 
  )

  # Save the result using process$save_result
  result <- process$save_result(data = collection, format = "NetCDF")

  # Create the batch job
  job <- create_job(graph = result, title = paste0("aapa_pristine_", i))

  # Submit the job
  #job$send_job()
  start_job(job=job) 

  jobs[[i]] <- job
  cat("Job for polygon", i, "submitted./n")
}


list_jobs() |> names() -> job_names
start_job(job_names[1]) # "j-250903124432404bb350befb866ee80f"
# start_job(job=job) 
#--> status: running

for(i in 2:20) {
  start_job(job_names[i])
}

list_jobs() |> as_tibble() |> View()

list_jobs() |> as_tibble() |> count(status)

list_jobs() |> as_tibble() -> jobs_df

for(i in c(1:9)) {
  id <- unlist(jobs_df[i,"id"])
  name <- paste0("INDICATOR_ts_pilot/S2/aapa_pristine/", gsub(" ", "_", unlist(jobs_df[i, "title"])), ".nc")
  print(name)
  download_results(id, folder = "INDICATOR_ts_pilot/S2/aapa_pristine/") -> downname
  file.rename(unlist(downname), name)
}
