library(terra)
library(tidyr)
library(dplyr)
library(lubridate)
library(future.apply)
library(arrow)

gcm <- c("TaiESM1", "UKESM1-0-LL", "NorESM2-MM")
ssp <- c("ssp245")
var <- c("tasmax", "tasmin", "pr")
year <- 1980:2100

param_grid <- expand.grid(
  gcm  = gcm,
  ssp  = ssp,
  var  = var,
  year = year,
  stringsAsFactors = FALSE
)


gcm_summaries <- function(gcm, ssp, var, year, out_dir, memfrac = 0.03) {

  # multisession workers each independently size their read chunks off total
  # system RAM with no awareness of the other concurrent workers, so cap each
  # process's budget explicitly (memfrac ~= 0.8 / n_workers) to keep the sum
  # of all workers' chunks within actual system RAM
  terra::terraOptions(memfrac = memfrac)
  terra::gdalCache(500)

  out_file <- file.path(out_dir, paste0(gcm, "-", ssp, "-", var, "-", year, ".parquet"))

  if (file.exists(out_file)) {
    return(invisible(NULL))
  }

  clim_in <- file.path("//10.60.2.10/FF_Lab/project_data/downscaling/Landscapes",
    paste0("Downscaled ", gcm), ssp, var,
    paste0(gcm, "-", ssp, "-", var, "-", year, ".nc"))

  if (!file.exists(clim_in)) {
    warning("Missing file, skipping: ", gcm, " ", ssp, " ", var, " ", year)
    return(invisible(NULL))
  }

  landscapes_r <- rast(clim_in)

  # spatial mean per day (fast: avoids loading the full daily x cell array)
  daily_means <- global(landscapes_r, fun = "mean", na.rm = TRUE)$mean

  value <- if (var == "pr") {
    # mm/day -> annual total precip (mm/yr)
    sum(daily_means)
  } else if (var %in% c("tasmax", "tasmin")) {
    # Kelvin -> annual mean temp (deg C)
    mean(daily_means) - 273.15
  } else {
    mean(daily_means)
  }

  tmp_file <- paste0(out_file, ".tmp")
  write_parquet(data.frame(gcm = gcm, ssp = ssp, var = var, year = year, value = value), tmp_file)
  file.rename(tmp_file, out_file)

  rm(landscapes_r, daily_means)
  gc()

  invisible(NULL)
}

out_dir <- "//10.60.2.10/FF_Lab/personal_storage/quinn_storage/gcm_summaries"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

n_workers <- 20
plan(multisession, workers = n_workers)

invisible(future_lapply(seq_len(nrow(param_grid)), \(i) {
  with(param_grid[i, ], gcm_summaries(gcm, ssp, var, year, out_dir = out_dir, memfrac = 0.8 / n_workers))
}))

plan(sequential)

gcm_climate <- open_dataset(out_dir, format = "parquet") |>
  collect() |>
  pivot_wider(names_from = var, values_from = value) |>
  mutate(tave = (tasmax + tasmin) / 2) |>
  select(gcm, ssp, year, pr, tasmax, tasmin, tave)

