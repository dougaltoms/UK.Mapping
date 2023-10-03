libs <- c(
    "tidyverse", "terra", "giscoR",
    "sf", "rayshader"
)

# install missing libraries
installed_libraries <- libs %in% rownames(installed.packages())
if (any(installed_libraries == F)){
    install.packages(libs[!installed_libraries])
}

# load libraries
invisible(lapply(libs, library, character.only = T))
library(wetherspoons)


# 1. DOWNLOAD UK BORDER
#----------------------

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

country_borders <- giscoR::gisco_get_countries(
                                    resolution = "10",
                                    country = "UK"
                            )

country_borders <- country_borders %>%
    sf::st_transform(crsLONGLAT)

plot(sf::st_geometry(country_borders))


# 2. DOWNLOAD ETH DATA
#---------------------

# Dynamically build URLs
start_url <- "https://s3-eu-west-1.amazonaws.com/vito.landcover.global/v3.0.1/2019/"
var_url <- c("W020N60/W020N60", "W020N80/W020N80", "E000N60/E000N60")
end <- "_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif"

urls <- paste0(start_url, var_url, end)

for (url in urls) {
    download.file(url, destfile = basename(url), mode = "wb")
}

raster_files <- list.files(
    path = getwd(),
    pattern = end,
    full.names = T
)

# 3. LOAD FOREST COVER
#---------------------

forest_cover <- lapply(raster_files, terra::rast)
forest_cover_mosaic <- do.call(terra::mosaic, forest_cover)

plot(forest_cover_mosaic)
plot(sf::st_geometry(country_borders), add = T)

# 4. CROP RASTER
#---------------

get_forest_cover_cropped <- function() {
    country_borders_vect <- terra::vect(
        country_borders
    )
    forest_cover_cropped <- terra::crop(
        forest_cover_mosaic, country_borders_vect,
        snap = "in", mask = T
    )

    return(forest_cover_cropped)
}

forest_cover_cropped <- get_forest_cover_cropped() %>%
    terra::aggregate(fact = 2) 

forest_mat <- rayshader::raster_to_matrix(forest_cover_cropped)
forest_mat <- rayshader::resize_matrix(forest_mat, 0.5)

# Custom colour palette
palette <- wetherspoons::pitcher.of("TuttiFrutti", n=5, direction=1)

# 2D Plot
forest_mat %>%
    rayshader::height_shade(texture=palette) %>%
    rayshader::add_shadow(rayshader::texture_shade(forest_mat, detail=2/3, contrast = 5, brightness = 6),0) %>%
    rayshader::add_shadow(rayshader::lamb_shade(forest_mat,zscale=50),0) %>% 
    #rayshader::plot_map(asp = 1/cospi(mean_latitude/180))
    rayshader::save_png(asp = 1/cospi(mean_latitude/180),filename="uk_forest_cover_green.png")