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


# Every warning() raised inside a multisession worker gets buffered and
# replayed in the main session once all futures resolve - with dozens of
# workers each raising several, that replay step can itself crash (the
# `if (is.na(label))` error seen in practice). Logging to a file instead
# sidesteps R's warning-relay machinery entirely. Parallel-safe by
# construction, not by locking: every failure gets its own uniquely-named
# log file (one per gcm/ssp/var/year, same convention as the parquet
# outputs), so no two workers ever write the same path and no mutex/append
# coordination is needed.
log_failure <- function(log_dir, gcm, ssp, var, year, message) {
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_file <- file.path(log_dir, paste0(gcm, "-", ssp, "-", var, "-", year, ".log"))
  writeLines(paste0(format(Sys.time()), "\t", message), log_file)
}

gcm_summaries <- function(gcm, ssp, var, year, out_dir, log_dir, memfrac = 0.03) {

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
    log_failure(log_dir, gcm, ssp, var, year, "Missing source file")
    return(invisible(NULL))
  }

  # a single malformed/unreadable source file must not take the whole
  # future_lapply batch down with it - catch and log, skip, keep going
  tryCatch({
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
  }, error = function(e) {
    log_failure(log_dir, gcm, ssp, var, year, paste("Failed:", conditionMessage(e)))
  })

  invisible(NULL)
}

out_dir <- "//10.60.2.10/FF_Lab/personal_storage/quinn_storage/gcm_summaries"
log_dir <- file.path(out_dir, "failures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

n_workers <- 20
plan(multisession, workers = n_workers)

# explicit globals/packages instead of future's automatic scan - keeps any
# stray terra object left over in this session (e.g. from also having run
# map_plot.R interactively) from being swept up and serialized to workers
invisible(future_lapply(
  seq_len(nrow(param_grid)),
  \(i) {
    with(param_grid[i, ], gcm_summaries(gcm, ssp, var, year, out_dir = out_dir, log_dir = log_dir, memfrac = 0.8 / n_workers))
  },
  future.globals = c("gcm_summaries", "log_failure", "param_grid", "out_dir", "log_dir", "n_workers"),
  future.packages = c("terra", "arrow")
))

plan(sequential)
