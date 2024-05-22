### load 2018 layers and convert them to rasters. In th end save them in a folder

library(sf)
library(raster)

# Set working directory 
setwd("C:/D1MPA trimestral/D1MPA Fishable Area/Layers 2018")

# List all shapefiles in the directory
shapefiles <- list.files(pattern = "\\.shp$")

# Load the mask 
mask <- raster("C:/D1MPA trimestral/mask.tif") # mask

plot(mask)

# output directory
output_dir <- "C:/D1MPA trimestral/D1MPA Fishable Area/Layers 2018/geotiff"

# List all shapefiles in the directory
shapefiles <- list.files(pattern = "\\.shp$")


# Loop through shapefiles
for (shp_file in shapefiles) {
  # Read the shapefile
  shp <- st_read(shp_file)
  
  # Convert the shapefile to raster using the mask
  rasterized <- rasterize(shp, mask, field = 1)
  covered<-cover(rasterized,mask)
  # Define the output raster file name
  output_file <- paste0(output_dir, gsub(".shp", "", shp_file), "_raster.tif")
  
  # Save the raster file
  writeRaster(covered, filename = output_file, format = "GTiff", overwrite = TRUE)
}

