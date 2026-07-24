library(ggplot2)
library(terra)
library(sf)
library(ggspatial)


ak <- st_read("./data/AK_no_islands.shp")
intak <- st_read("./data/AK_interior.shp")


dirs <- list.dirs("//10.60.2.10/FF_Lab/personal_storage/quinn_storage/landscape_init_ak_can", recursive = FALSE)
base <- "//10.60.2.10/FF_Lab/personal_storage/quinn_storage/landscape_init_ak_can"
landscape_names <- basename(dirs[grepl("landscape_", basename(dirs))])
landscape_names <- landscape_names[grepl(landscape_names, pattern = "01|02|03")]

env_grids <- lapply(1:3, \(i) {
  paste0(base, "/", landscape_names[i], "/gis/env.grid.tif") |>
    terra::rast()
})

# project everything to the CRS of the first landscape raster
ak_proj <- st_transform(ak, crs(env_grids[[1]]))
intak_proj <- st_transform(intak, crs(env_grids[[1]]))

env_grids_df <- lapply(env_grids, \(r) {
  as.data.frame(r, xy = TRUE, na.rm = TRUE)
})

map_plt <- ggplot() +
  geom_sf(data = intak_proj, fill = "#3b8a3b33", color = NA, alpha = 0.5) +
  geom_raster(data = env_grids_df[[1]], aes(x, y, fill = I("black"))) +
  geom_raster(data = env_grids_df[[2]], aes(x, y, fill = I("black"))) +
  geom_raster(data = env_grids_df[[3]], aes(x, y, fill = I("black"))) +
  geom_sf(data = ak_proj, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(expand = FALSE) +
  # annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_nautical(),
    height = unit(3, "cm"),
    width  = unit(3, "cm")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text = element_text(size = 16, colour = "black"),
  )

ext <- st_bbox(ak_proj)
map_ratio <- (ext$xmax - ext$xmin) / (ext$ymax - ext$ymin)
map_ratio

height_cm <- 50
width_cm  <- height_cm * map_ratio

ggsave(
  filename = "./figures/map_ak.svg",
  plot = map_plt,
  device = "svg",
  width = width_cm,
  height = height_cm,
  units = "cm"
)




# Testing zone
gcm <- c("TaiESM1")
ssp <- c("ssp245")
var <- c("tasmax")
year <- 1980

clim_in <- file.path("//10.60.2.10/FF_Lab/project_data/downscaling/Landscapes",
  paste0("Downscaled ", gcm), ssp, var,
  paste0(gcm, "-", ssp, "-", var, "-", year, ".nc"))

rast <- rast(clim_in, lyr = 1)
nlyr(rast)
r <- rast(ext(rast), nlyr = 1, crs = crs(rast))
values(r) <- 1
r <- project(r, crs(env_grids[[1]]), method = "near")
r_df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)


ggplot() +
  geom_sf(data = intak_proj, fill = "#3b8a3b33", color = NA, alpha = 0.5) +
  geom_raster(data = env_grids_df[[1]], aes(x, y, fill = I("black"))) +
  geom_raster(data = env_grids_df[[2]], aes(x, y, fill = I("black"))) +
  geom_raster(data = env_grids_df[[3]], aes(x, y, fill = I("black"))) +
  geom_raster(data = r_df, aes(x, y, fill = I("black"), alpha = 0.5)) +
  geom_sf(data = ak_proj, fill = NA, color = "black", linewidth = 0.5) +
  coord_sf(expand = FALSE)


study_domain <- rast("./data/ABoVE_Study_Domain.tif")
plot(study_domain)
plot(r, add = TRUE)