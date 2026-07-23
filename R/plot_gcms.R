library(terra)
library(tidyr)
library(dplyr)
library(lubridate)
library(future.apply)
library(here)

dirs <- list.dirs(here(), recursive = FALSE)
landscape_names <- basename(dirs[grepl("landscape_", basename(dirs))])


gcm <- c("TaiESM1", "UKESM1-0-LL", "NorEsm2-MM")
ssp <- c("ssp126", "ssp245", "ssp370")
var <- c("tasmax", "pr", "rsds", "tasmin", "vp")
year <- 1950:2100

param_grid <- expand.grid(
  landscape_dir = landscape_names,
  gcm  = gcm,
  ssp  = ssp,
  var  = var,
  year = year,
  stringsAsFactors = FALSE
)


plot <- function(gcm, ssp, var, year) {

  
    clim_in <- file.path("//10.60.2.10/FF_Lab/project_data/downscaling/Landscapes",
      paste0("Downscaled ", gcm), ssp, var,
      paste0(gcm, "-", ssp, "-", var, "-", year, ".nc"))
    cpcrw_clim_in <- file.path("//10.60.2.10/FF_Lab/project_data/downscaling/CPCRW",
      paste0("Downscaled ", gcm), ssp, var,
      paste0(gcm, "-", ssp, "-", var, "-", year, ".nc"))



    # Create the RID-to-climate-cell lookup: one row per RID, mapped to its climate cell
    climate_link <- terra::cells(var_buffer_albers, env_points) |>
      as.data.frame() |>
      rename(env.grid = "ID", climate.grid = "cell")
    log_msg("link_built")

    # Create data of climate.grid cell ID with values
    climate_extract <- as.data.frame(var_buffer_albers, cells = TRUE) |>
      dplyr::rename(climate.grid = "cell") |>
      right_join(climate_link, by = "climate.grid") |>
      distinct(climate.grid, .keep_all = TRUE) |>
      select(-env.grid)

    ak_climate_var_df <- climate_extract |>
      tidyr::pivot_longer(
        cols = -climate.grid,
        names_to = "y_day",
        values_to = "value") |>
      dplyr::mutate(
        y_day = as.numeric(sub(paste0(var, "_"), "", y_day)),
        day_of_year = ifelse(lubridate::leap_year(year) & y_day >= 60, y_day + 1, y_day),
        date = lubridate::make_date(year) + lubridate::days(day_of_year - 1),
        value = round(value, 3),
        gcm = gcm,
        ssp = ssp,
        landscape = landscape_dir) |>
      dplyr::select(climate.grid, value, date, gcm, ssp, landscape) |>
      dplyr::rename(!!var := value)
      # names(df)[names(df) == "value"] <- var # for base R rename. no tidy evaluation.
    write.table(ak_climate_var_df, file.path(out_dir, paste0(gcm, "-", ssp, "-", var, "-", year, ".txt")), row.names = FALSE, sep = "\t")
    log_msg("DONE")


}
