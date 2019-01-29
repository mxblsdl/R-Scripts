---
author: "Max Blasdel"
date: "1/28/2019"
title: "Process Raster Files"
output: 
  html_document:
    keep_md: true
---

# Purpose
Read in Landsat files from multiple folders, stack together into single file, and clip to a given extent.
Load Relevent packages.

I wrote this script when I was first learnging how to process spatial data in R and it relys on a lot of for loops and intermediary steps. Looking back at it, the efficiency could be greatly improved by creating and saving less objects. I'll include the original setup and then write a new function at the end to accomplish the same workflow.

```r
library(ggplot2)
library(raster)
library(rgdal)
```

There are multiple rasters in different folders. I need to go into each folder and find all of the tif files. Stack these into one raster and then save the output in a different folder.

```r
##Create a directory of all the Landsat folders
folders<-dir(path = getwd(), pattern='\\.tar$', full.names = T, recursive = F)
folders<-as.data.frame(folders)

##Setting a directory to save the files to
stackedWD<-"images/Stacked Images"

##Create a loop that goes into each folder and extracts the TIF files then writes
##the output as a stacked tiff.
for (i in 1:14) {
  setwd(paste(folders[i,]))
  d<-dir(path=paste(folders[i,]), pattern='\\.TIF$')
  d<-stack(d[1:7])
  setwd(stackedWD)
  writeRaster(d,filename = paste(folders[i,]) , overwrite=T, format="GTiff")
}
```

#Cropping and reprojecting
Create list of rasters to crop than resample

```r
## the ^ denotes the start of a string and the $ denotes the end of a string
raster_data<-list.files(path=getwd(), pattern="^LT.*.tif$")   

# create a list object containing rasters
rast.list<-list()
for(i in 1:length(raster_data)){
  rast.list[i] <-brick(raster_data[i])}

# Define an extent that rasters will be cropped to. 
# This should probably be defined from an existing object as opposed to hard coded.
e <- extent(c(449087, 492458, 4650791, 4675170)) # xmin,xmax,ymin,ymax

##Need to establish a raster to resample to
a<-brick(paste(raster_data[[1]]))
a<-crop(a,e)

# crop larger raster to smaller raster  - create rectangle object
resample.list<-list()
for(i in 1:length(rast.list)){
  temp <- crop(rast.list[[i]], e)
  resample.list[[i]] <- resample(temp, a, method= "ngb") #nearest neighbor
}

# Write out results for later use with different prefix attached.
for (i in 1:length(rast.list)) {
  f <- paste0('EX.', paste(raster_data[i]))
  writeRaster(resample.list[[i]],filename = f , overwrite=T, format="GTiff")
}
```

# Function accomplishing above workflow
The above workflow produces bricked rasters, a cloudmask raster, clipped and resampled rasters, and the final product with the cloudmask applied. There is a lot of extra data here that I don't need and that takes up disk space. 

These are landsat images downloaded from EarthExplorer. They come in tar.gz format and need to be unpacked. Each tar file has all of the bands from the images, which need to be stacked.

```r
processLandsat <- function(input_files, extent, output_location) {
  lapply(input_files, function(x) {
    t <- tools::file_path_sans_ext(x, compression = F)
    t <- gsub(".*/","",t)
    output_name <- paste(t, ".tif", sep = "")
    
    d <- dir(x, pattern = "*.TIF$", full.names = T)
    r <- raster::stack(d[1:7])
    r <- raster::brick(r)
    r <- raster::crop(r, e)
      if (file.exists(output_location) != TRUE){
          dir.create(output_location)
    }
    raster::writeRaster(r, paste(output_location,"/", output_name, sep = ""), format="GTiff")
  })  
}

# Set location of tar folders
input_files <- as.list(list.files("images", pattern = "*.tar$", full.names = T))
# Set extent
e <- raster::extent(c(449087, 492458, 4650791, 4675170))
# run function
processLandsat(input_files = input_files, extent = e, output_location = "output")
```

The above function does not resample the rasters to each other. This step is sometimes necessary since cropping to a given extent is not always exact for rasters.



