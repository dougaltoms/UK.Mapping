library(rayshader)
library(elevatr)
library(raster)
library(devtools)
library(dplyr)

# Download custom colour palettes
devtools::install_github("dougaltoms/wetherspoons")
library(wetherspoons)

# Load data using ISO country code e.g. GBR, FRA, NOR...
uk <- readRDS(
    url("https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_GBR_0_sf.rds")
    )

# Get DEM and create matrix
dem <- elevatr::get_elev_raster(uk, z = 7)
uk_dem <- raster::mask(dem, uk)
uk_mat <- rayshader::raster_to_matrix(uk_dem)
uk_mat_reduced <- rayshader::resize_matrix(uk_mat, 0.5)

# Calculate mean latitude for plotting aspect ratio
extent_uk <- raster::extent(uk_dem)
mean_latitude = mean(c(extent_uk@ymax,extent_uk@ymin))

# Custom colour palette
palette <- wetherspoons::pitcher.of("BlueLagoon", n=5, direction=1)

# 2D Plot
uk_mat_reduced %>%
    rayshader::height_shade(texture=palette) %>%
    rayshader::add_shadow(rayshader::texture_shade(uk_mat_reduced, detail=1/3, contrast = 5, brightness = 6),0) %>%
    rayshader::add_shadow(rayshader::lamb_shade(uk_mat_reduced,zscale=50),0) %>% 
    rayshader::save_png(asp = 1/cospi(mean_latitude/180),filename="uk_hillshade_blue.png")
  

#3D Plot
uk_mat_reduced %>%
    rayshader::height_shade(texture = palette) %>%
    rayshader::plot_3d(uk_mat_reduced,zscale=15, solid=FALSE, windowsize=c(1200,1200), water=TRUE, asp = 1/cospi(mean_latitude/180))
    # Render high-def version
    rayshader::render_highquality()