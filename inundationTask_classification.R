################################
#                              #
#    B+ HABITAT PILOT 2025     #
#         Inundation           #
#                              #
################################

# Author: tytti.jussila@syke.fi


### PART 1: THE POINT SAMPLE 
# Points are easiest to generate in QGIS (see instructions in Google Drive)
# The grid to assist point labeling can be made with this script or in QGIS.

### PART 2: CLASSIFICATION
# Calculating moisture and water indices (required for classification input) 
# Applying classification algorithms on satellite data 
# Models to apply: 1) Jussila et al. decision tree and 2)  Lefebvre et al. decision tree (WIW - water in wetlands) 

### PART 3: EXTRACT VALUES FOR POINT SAMPLE

### PART 4: ACCURACY ASSESMENT


# Replace file paths with file paths for data in your computer

# install libraries if not already installed
install.packages("rpart")
install.packages("rpart.plot")
install.packages("raster")
install.packages("sf")
install.packages("spatstat")

# Load required packages
library(rpart) # for decision tree algorithm training and predicting 
library(rpart.plot) # for visualizing rpart-decision trees
library(raster) # for handling rasters
library(sf) # For handling vector data
library(spatstat) # For spatial sampling of points

# set working directory. Files are saved here if no other path is specified
setwd("D:/Users/xxxxx/habitat_pilot") 

### PART 1: GENERATE AND LABEL SAMPLE  POINTS in QGIS 
# See instructions in google drive https://drive.google.com/drive/folders/17W2e9t5t2gfcjAdUrc2HChqzXthikTq6?usp=drive_link

### Make a 20m grid corresponding to satellite image to assist in pixel labeling
# This can be done in QGIS, too. See instructions
r <- stack("D:/Users/xxxxx/habitatpilot/downloaded_S2_image.tiff")  # !Replace with path to satellite image file in your computer

grid <- st_make_grid(
  st_as_sfc(st_bbox(r)), # Convert raster extent to an sf object
  cellsize = 20,         # Use S2 SWIR resolution
  what = "polygons"       # Creates a polygon grid
)
# Convert created grid to sf object
grid_sf <- st_sf(grid)

# save grid to computer
st_write(grid_sf, "grid_20m.shp") 

# open in QGIS, make grid symbology fill transparent and label the points based on aerial imagery or other data


### PART 2 - applying classifications on satellite image

# Load satellite image raster to R (multi-band raster stack)
r <- stack("D:/Users/xxxxx/habitatpilot/downloaded_S2_image.tiff")  # Replace with path to satellite image file in your computer
names(r) <- c("b01", "b02", "b03", "b04", "b05", "b06", "b07", "b08", "b8a", "b09", "b11", "b12") # rename bands (to match model input)
summary(r) # check raster reflectance values for bands. They should be between 0-10 000 for L2A images. Value format is important for decision trees 

#calculate moisture and water indices for image (first five indices needed for applying Jussila-model as these were used in training data. Only mndwi is included in final tree)
r$ndvi <- (r$b8a - r$b04) / (r$b8a + r$b04)
r$ndwi_mf <- (r$b03 - r$b8a) / (r$b03 + r$b8a) # NDWI. Normalised difference water index by McFeeters
r$mndwi11 <- (r$b03 - r$b11) / (r$b03 + r$b11) # modified water index with SWIR band 11 
r$mndwi12 <- (r$b03 - r$b12) / (r$b03 + r$b12) # modified water index with SWIR band 12 
r$ndmi_gao11 <- (r$b8a - r$b11) / (r$b8a + r$b11) # NDMI. Normalised difference moisture index by Gao (1996). Sometimes referred to as NDWI as well

# additional indices. STR should be a good indication of moisture
swir_to_str <- function(swir) { # function to calculate moisture index STR (based on SWIR band 11 or 12)
  swir <- swir/10000
  STR <- ((1-swir)^2)/(2*swir) #5.29
  return(STR)
}
r$STR1 <- swir_to_str(r$b11)
r$STR2 <- swir_to_str(r$b12)


# Download decision tree RData-file from google drive to your computer
# Load decision tree to R. 
load("D:/Users/xxxxx/habitatpilot/jussila_decisiontree.RData") # !replace with file path to where you stored the model!
rpart.plot(tree_jussila, tweak = 1, extra = 0) # visualise tree structure

# Apply decision tree to classify image raster 
watercl_jussila <- predict(r, tree_jussila, type = "class") # (1= dry, 2=waterlogged)

# classes as in sample points: reclassify 1 to 0 and 2 to 100 (--> 0=dry, 100=waterlogged)
reclass_matrix <- matrix(c(1,0, 2,100), ncol=2,byrow = TRUE) 
watercl_jussila <- reclassify(watercl_jussila, reclass_matrix)

# classify raster using Lefebvre et al. "water in wetlands" (wiw) model (very simple decision tree, only two nodes)
watercl_wiw <- r$b8a <= 1804 & r$b12 <= 1131
watercl_wiw <- watercl_wiw*100 # classes as in sample points: 1 to 100, 0 to 0

# name the raster bands
names(watercl_jussila) <- c("pred_jussila")
names(watercl_wiw) <- c("pred_wiw")

# plot classified rasters. Don't mind areas around wetland. The algorithms do not work in forests. 
plot(watercl_jussila) # 0= dry, 1= waterlogged
plot(watercl_wiw) # 0 = dry, 1=waterlogged

# stack classification layers with original satellite image
stacked <- stack(r, watercl_jussila, watercl_wiw)
names(stacked) # check the layers in raster stack

# Save classifications as stack and/or as separate rasters. 
writeRaster(stacked, "stacked_site_date.tif", format = "GTiff")
writeRaster(watercl_jussila, "watercl_jussila_siteX_date.tif", format = "GTiff") # !replace with file path to where you want to store the raster in your computer
writeRaster(watercl_wiw, "watercl_wiw_site_date.tif", format = "GTiff") # !replace with file path where you want to store the raster in your computer

# Repeat with other images/sites you wish to classify


### PART 3: Extract classifications for points

# Load points that you generated in QGIS for this image area
sample_points <- st_read("pointfile.shp") # !replace with right file path

# Extract image reflectance values, indices and classifications for sample points
raster_values <- extract(stacked, sample_points)
sample_points <- cbind(sample_points, raster_values)

# Save points
st_write(sample_points, "points_site_date.gpkg")  # !replace with file path. Save results (gpkg=geopackage. Like shapefile but better - everything stored in one file, not five, like with shp)

# repeat for other images / areas




#### STEP 4  - ACCURACY ASSESSMENT
# compare classifications (predicted) and aerial image interpretations ("observed truth")

library(caret) # for confusion matrix

# open all point files and combine to one dataframe table including all sites
p1 <- st_read("points_site_date1.gpkg")
p2 <- st_read("points_site_date2.gpkg")
p3 <- st_read("points_site_date3.gpkg")
p4 <- st_read("points_site_date4.gpkg")
p5 <- st_read("points_site_date5.gpkg") # and so on

valdata <- rbind(p1, p2, p3, p4, p5)

# data table ready for validation!
# save to computer
st_write(valdata, "inundation_validation_data.gpkg") #! replace with file path to desired folder


# If observed truth has three classes (wet, dry, uncertain):
# Remove the uncertain observations
valdata <- valdata[valdata$TRUTH!=50,] # !replace "TRUTH" with the field name of reference information in your data

# balance the sample if needed? (if very little waterlogged points and lots of dry points in data)

# confusion matrix and accuracy metrics
confmat_jussila <- confusionMatrix(factor(valdata$pred_jussila), reference= factor(valdata$TRUTH))
print(confmat_jussila)
confmat_wiw <- confusionMatrix(factor(valdata$pred_wiw), factor(valdata$TRUTH))
print(confmat_wiw)

# Jussila F1-score (accuracy for imbalanced data)
precision <- as.numeric(confmat_jussila$byClass["Specificity"])
recall <- as.numeric(confmat_jussila$byClass["Sensitivity"])
F1_jussila <- 2 * (precision * recall) / (precision + recall)
print(F1_jussila)

# Lefebvre WiW F1-score (accuracy for imbalanced data)
precision <- as.numeric(confmat_wiw$byClass["Specificity"])
recall <- as.numeric(confmat_wiw$byClass["Sensitivity"])
F1_wiw <- 2 * (precision * recall) / (precision + recall)
print(F1_wiw)
