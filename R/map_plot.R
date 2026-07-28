library(ggplot2)
library(terra)
library(sf)
library(ggspatial)
library(dplyr)
library(tidyr)
library(arrow)
library(patchwork)

source("R/theme.R")


#---------- plot AK landscapes ----------#

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
  geom_sf(data = ak_proj, fill = "grey20", color = "grey80", linewidth = 0.4) +
  geom_sf(data = intak_proj, fill = "#3b8a3b33", color = NA, alpha = 0.5) +
  geom_raster(data = env_grids_df[[1]], aes(x, y, fill = I("white"))) +
  geom_raster(data = env_grids_df[[2]], aes(x, y, fill = I("white"))) +
  geom_raster(data = env_grids_df[[3]], aes(x, y, fill = I("white"))) +
  coord_sf(expand = FALSE) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_nautical(text_col = "white"),
    height = unit(3, "cm"),
    width  = unit(3, "cm")
  ) +
  theme_iland_dark() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    # panel.grid = element_blank(),
  )

# half of a 16:9 slide (13.333in x 7.5in) - other half is bullet points
half_slide_width_cm <- (13.333 * 2.54) / 2
half_slide_height_cm <- (9.333 * 2.54) / 2

ext <- st_bbox(ak_proj)
map_ratio <- (ext$xmax - ext$xmin) / (ext$ymax - ext$ymin)

width_cm  <- half_slide_width_cm
height_cm <- width_cm / map_ratio

ggsave(
  filename = "./figures/map_ak.svg",
  plot = map_plt,
  device = "svg",
  width = width_cm,
  height = height_cm,
  units = "cm"
)


#---------- plot GCM area ----------#

gcm <- c("TaiESM1")
ssp <- c("ssp245")
var <- c("tasmax")
year <- 1980

clim_in <- file.path("//10.60.2.10/FF_Lab/project_data/downscaling/Landscapes",
  paste0("Downscaled ", gcm), ssp, var,
  paste0(gcm, "-", ssp, "-", var, "-", year, ".nc"))

gcm_rast <- rast(clim_in, lyr = 1)

gcm_poly_area <- as.polygons(!is.na(gcm_rast), dissolve = TRUE)
gcm_poly_area <- gcm_poly_area[gcm_poly_area[[1]] == 1, ]     # keep only the valid-data polygon
gcm_poly_area <- project(gcm_poly_area, crs(env_grids[[1]])) # no `method=` for vectors, that's raster-only
gcm_poly_area_sf <- sf::st_as_sf(gcm_poly_area)

study_domain <- rast("./data/ABoVE_Study_Domain.tif") |>
  project(crs(env_grids[[1]]), method = "near")

study_domain_poly <- as.polygons(!is.na(study_domain), dissolve = TRUE) |>
  sf::st_as_sf()

gcm_downscaled_area_plot <- ggplot() +
  geom_sf(data = study_domain_poly, fill = "white", color = "black", linewidth = 0.5) +
  geom_sf(data = gcm_poly_area_sf, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = intak_proj, fill = "#3b8a3b33", color = NA, alpha = 0.5) +
  coord_sf(expand = FALSE) +
  annotation_north_arrow(
    location = "br",
    which_north = "true",
    style = north_arrow_nautical(),
    height = unit(3, "cm"),
    width  = unit(3, "cm")
  ) +
  theme_iland() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    panel.grid = element_blank(),
  )

gcm_ext <- st_bbox(study_domain_poly)
gcm_map_ratio <- (gcm_ext$xmax - gcm_ext$xmin) / (gcm_ext$ymax - gcm_ext$ymin)

gcm_width_cm  <- half_slide_width_cm
gcm_height_cm <- gcm_width_cm / gcm_map_ratio

ggsave(
  filename = "./figures/gcm_downscaled_area_plot.svg",
  plot = gcm_downscaled_area_plot,
  device = "svg",
  width = gcm_width_cm,
  height = gcm_height_cm,
  units = "cm"
)

#---------- plot GCM over time ----------#

rerun <- FALSE
if (!exists("gcm_climate") || rerun) {
  gcm_climate <- open_dataset("//10.60.2.10/FF_Lab/personal_storage/quinn_storage/gcm_summaries") |>
    collect() |>
    pivot_wider(names_from = var, values_from = value) |>
    mutate(
      tave = (tasmax + tasmin) / 2,
      # hottest -> coolest, matching fire_plot/species_dom2 - this dataset
      # uses "NorESM2-MM" (uppercase ESM), not the "NorEsm2-MM" casing seen
      # in the iLand-visualisation-ai parquets
      gcm = factor(gcm, levels = c("UKESM1-0-LL", "TaiESM1", "NorESM2-MM"))
    )
}

# reshape to one row per facet: temperature carries tave as the line and
# tasmin/tasmax as a ribbon band around it; precipitation has no ribbon
gcm_climate_facets <- bind_rows(
  gcm_climate |>
    transmute(gcm, ssp, year,
      facet = "Temperature (\u00b0C)",
      value = tave, ymin = tasmin, ymax = tasmax
    ),
  gcm_climate |>
    transmute(gcm, ssp, year,
      facet = "Precipitation (mm/yr)",
      value = pr, ymin = NA_real_, ymax = NA_real_
    )
) |>
  mutate(facet = factor(facet, levels = c("Temperature (\u00b0C)", "Precipitation (mm/yr)")))

gcm_climate_plot <- ggplot(gcm_climate_facets, aes(x = year, y = value, color = gcm, fill = gcm)) +
  geom_ribbon(aes(ymin = ymin, ymax = ymax), color = NA, alpha = 0.15) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~facet, scales = "free_y", ncol = 1) +
  scale_colour_gcm() +
  scale_fill_gcm() +
  scale_x_continuous(breaks = YEAR_BREAKS_CLIMATE, expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = NULL, color = "GCM", fill = "GCM") +
  theme_iland_dark() +
  theme(
    legend.position = "bottom",
  )

full_slide_width_cm <- (13.333 * 2.54)
full_slide_height_cm <- (9.333 * 2.54)

# ggsave(
#   filename = "./figures/gcm_climate_plot.svg",
#   plot = gcm_climate_plot,
#   device = "svg",
#   width = full_slide_width_cm,
#   height = full_slide_height_cm,
#   units = "cm"
# )

#---------- plot GCM with GAM over time ----------#

gcm_climate_smooth_plot <- ggplot(gcm_climate_facets, aes(x = year, y = value, color = gcm, fill = gcm)) +
  geom_smooth(linewidth = 0.8) +
  facet_wrap(~facet, scales = "free_y", ncol = 1) +
  scale_colour_gcm() +
  scale_fill_gcm() +
  scale_x_continuous(breaks = YEAR_BREAKS_CLIMATE, expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = NULL, color = "GCM", fill = "GCM") +
  theme_iland_dark() +
  theme(
    legend.position = "bottom",
  )


ggsave(
  filename = "./figures/gcm_climate_smooth_plot.svg",
  plot = gcm_climate_smooth_plot,
  device = "svg",
  width = 15,
  height = half_slide_height_cm,
  units = "cm"
)

gcm_climate_facets2 <- bind_rows(
  gcm_climate |>
    transmute(gcm, ssp, year,
      facet = "Temp (\u00b0C)",
      value = tave, ymin = tasmin, ymax = tasmax
    ),
  gcm_climate |>
    transmute(gcm, ssp, year,
      facet = "Precip (mm/yr)",
      value = pr, ymin = NA_real_, ymax = NA_real_
    )
) |>
  mutate(facet = factor(facet, levels = c("Temp (\u00b0C)", "Precip (mm/yr)")))


gcm_climate_smooth_plot2 <- ggplot(gcm_climate_facets2, aes(x = year, y = value, color = gcm, fill = gcm)) +
  geom_smooth(linewidth = 0.8) +
  facet_wrap(~facet, scales = "free_y", ncol = 1, strip.position = "right") +
  scale_colour_gcm() +
  scale_fill_gcm() +
  scale_x_continuous(breaks = YEAR_BREAKS_CLIMATE, expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = NULL, color = "GCM", fill = "GCM") +
  theme_iland_dark() +
  theme(
    legend.position = "right",
  )


#---------- Fire over time ----------#

fire_df <- arrow::read_parquet("//10.60.2.10/FF_Lab/personal_storage/quinn_storage/iLand-visualisation-ai/data/processed/fire/fire_annual.parquet") |>
  dplyr::filter(
    landscape %in% c("landscape_alaska_01", "landscape_alaska_02", "landscape_alaska_03"),
    model     %in% c("NorEsm2-MM", "TaiESM1", "UKESM1-0-LL"),
    ssp       %in% c("ssp245"),
    fri       %in% c("baseline"),
    year_actual >= 2015,
    year_actual <= 2100
  ) |>
  dplyr::mutate(
    replicate = factor(replicate),
    landscape = factor(landscape,
      levels = c("landscape_alaska_01", "landscape_alaska_02", "landscape_alaska_03"),
      labels = c("landscape 1", "landscape 2", "landscape 3")
    ),
    # fire_df already has a clean `model` column straight from the parquet
    # (no treatment-string regex needed here, unlike species_dom2) - just
    # reorder it hottest -> coolest to match the rest of the deck
    model = factor(model, levels = c("UKESM1-0-LL", "TaiESM1", "NorEsm2-MM"))
  )

fire_plot <- ggplot(fire_df, aes(x = year_actual, y = area_ha,
                    colour = model, fill = model)) +
  geom_smooth(method = "gam", formula = y ~ s(x), alpha = 0.2) +
  facet_grid(landscape ~ model) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_colour_gcm() +
  scale_fill_gcm() +
  scale_x_continuous(breaks = YEAR_BREAKS_ECO, expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Area burned (ha)", colour = "GCM", fill = "GCM") +
  theme_iland_dark() +
  theme(
    strip.background = element_rect(fill = NA, colour = NA),
  )

# colour each GCM's column strip to match its line colour, same as the
# species-dominance plots. fire_plot isn't combined via patchwork with
# anything else, so ggsave can take the recoloured gtable directly - no
# need for wrap_elements()'s extra transparent-background workaround
fire_strip_colours <- GCM_COLOURS[levels(fire_df$model)]
fire_plot_coloured <- colour_gcm_strips(fire_plot, fire_strip_colours)

ggsave(
  filename = "./figures/fire_plot.svg",
  plot = fire_plot_coloured,
  device = "svg",
  width = full_slide_width_cm,
  height = full_slide_height_cm,
  units = "cm"
)


#---------- Species area dominated ----------#

species_dom <- arrow::read_parquet("//10.60.2.10/FF_Lab/personal_storage/quinn_storage/iLand-visualisation-ai/data/processed/area_dom/dominant_species_area.parquet") |>
  dplyr::filter(
    landscape %in% c("landscape_alaska_01_2015-2100scenario", "landscape_alaska_02_2015-2100scenario", "landscape_alaska_03_2015-2100scenario"),
    treatment %in% c("NorEsm2-MMssp245_dbh2.5_onlysimfalse_yr_1_iLand2.1", "NorEsm2-MMssp245_dbh2.5_onlysimtrue_yr_1_iLand2.1", "TaiESM1ssp245_dbh2.5_onlysimfalse_yr_1_iLand2.1", "TaiESM1ssp245_dbh2.5_onlysimtrue_yr_1_iLand2.1", "UKESM1-0-LLssp245_dbh2.5_onlysimfalse_yr_1_iLand2.1", "UKESM1-0-LLssp245_dbh2.5_onlysimtrue_yr_1_iLand2.1"),
    sp_dom    %in% c("Pima", "Pigl", "Bene", "Potr"),
    year_actual >= 2015,
    year_actual <= 2100
  ) |>
  dplyr::mutate(
    sp_dom      = factor(sp_dom, levels = c("Pima", "Pigl", "Bene", "Potr")),
    treat_short = sub("_yr_[0-9]+_iLand.*$", "",
                      sub("_dbh2\\.5_", " · ", treatment))
  )

species_dom_plot <- ggplot(species_dom, aes(x = year_actual, y = area_ha, group = interaction(replicate, treatment, landscape), colour = sp_dom)) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  facet_grid(sp_dom ~ paste(landscape, treat_short, sep = " · "), scales = "free_y",
             labeller = labeller(sp_dom = SPECIES_STRIP_LABELS)) +
  scale_colour_species_dark(guide = "none") +
  scale_x_continuous(breaks = YEAR_BREAKS_ECO, expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Area (ha)") +
  theme_iland_dark() +
  theme(
    strip.background = element_rect(fill = NA, colour = NA),
  )

#---------- Species area dominated (alt: facet landscape x model, fire = linetype) ----------#
# Same underlying species_dom data/replicates as above - individual replicate
# lines kept as-is (per Quinn: the spread/stochasticity contrast between fire
# on vs off reads better as raw lines than a summarised ribbon). Reduces the
# facet grid from 4 species x 18 landscape/treatment combos down to a 3x3
# landscape x model grid, moving species to colour and fire to linetype.

species_dom2 <- species_dom |>
  dplyr::mutate(
    # explicit factor (not just character) so facet_grid's column order is
    # deterministic and matches GCM_COLOURS' order for strip recolouring
    model = factor(
      sub("ssp[0-9].*$", "", treatment),
      levels = c("UKESM1-0-LL", "TaiESM1", "NorEsm2-MM")
    ),
    fire  = factor(
      grepl("onlysimtrue", treatment),
      levels = c(FALSE, TRUE),
      labels = c("Fire", "No fire")
    ),
    landscape = factor(landscape,
      levels = c(
        "landscape_alaska_01_2015-2100scenario",
        "landscape_alaska_02_2015-2100scenario",
        "landscape_alaska_03_2015-2100scenario"
      ),
      labels = c("landscape 1", "landscape 2", "landscape 3")
    )
  )

species_dom_plot2 <- ggplot(species_dom2, aes(
    x = year_actual, y = area_ha,
    group = interaction(replicate, sp_dom, fire),
    colour = sp_dom, linetype = fire, linewidth = fire
  )) +
  geom_line(alpha = 0.5) +
  facet_grid(landscape ~ model, scales = "free_y") +
  scale_colour_species(name = "Species") +
  scale_linetype_manual(name = "Fire effect", values = c("Fire" = "solid", "No fire" = "dashed"),
                         labels = c("Fire" = "Feedback", "No fire" = "No feedback")) +
  # dashed reads thinner than solid at equal width, so nudge it up slightly
  scale_linewidth_manual(values = c("Fire" = 0.5, "No fire" = 0.8), guide = "none") +
  scale_x_continuous(breaks = YEAR_BREAKS_ECO, expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Year", y = "Dominant species area (ha)") +
  theme_iland() +
  theme(
    strip.background = element_rect(fill = NA, colour = NA),
  )

# ggsave(
#   filename = "./figures/species_dom_plot2.svg",
#   plot = species_dom_plot2,
#   device = "svg",
#   width = full_slide_width_cm,
#   height = full_slide_height_cm,
#   units = "cm"
# )


#---------- Species area dominated (per-landscape, facet by species) ----------#
# One figure per landscape (3 separate slides). Facets by species (rows) x
# model (cols) instead of colouring by species, so a low-abundance species
# like Potr/aspen gets its own y-scale rather than being flattened next to
# higher-abundance species sharing a panel.

plot_species_dom_landscape <- function(data, landscape_label) {
  p <- ggplot(
    dplyr::filter(data, landscape == landscape_label),
    aes(
      x = year_actual, y = area_ha,
      group = interaction(replicate, fire),
      colour = sp_dom, linetype = fire, linewidth = fire
    )
  ) +
    geom_line(alpha = 0.5) +
    facet_grid(sp_dom ~ model, scales = "free_y",
               labeller = labeller(sp_dom = SPECIES_STRIP_LABELS)) +
    scale_colour_species_dark(guide = "none") +
    scale_linetype_manual(name = "Fire effect", values = c("Fire" = "solid", "No fire" = "dashed"),
                           labels = c("Fire" = "Feedback", "No fire" = "No feedback")) +
    # dashed reads thinner than solid at equal width, so nudge it up slightly
    scale_linewidth_manual(values = c("Fire" = 0.5, "No fire" = 0.8), guide = "none") +
    # the linetype legend key otherwise draws in geom_line's actual colour/
    # alpha (black at alpha=0.5, since colour's own legend is suppressed
    # above) - invisible against a dark background, so force it to solid
    # white regardless of the underlying data's colour/alpha
    guides(linetype = guide_legend(override.aes = list(colour = "white", alpha = 1))) +
    scale_x_continuous(breaks = YEAR_BREAKS_ECO, expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Year", y = "Dominant species area (ha)") +
    theme_iland_dark() +
    theme(
    # strip.background = element_rect(fill = NA, colour = NA),
      legend.position = "right"
    )

  # colour each GCM's column strip to match its line colour elsewhere in the
  # deck; wrap_elements(full=...) is needed so the recoloured gtable can
  # still be combined with other plots via patchwork, but it unconditionally
  # adds its own opaque white plot background regardless of the input's own
  # theme - the `&` override strips that back out
  strip_colours <- GCM_COLOURS[levels(data$model)]
  wrap_elements(full = colour_gcm_strips(p, strip_colours)) &
    theme(plot.background = element_rect(fill = "transparent", colour = NA))
}

species_dom_plot_landscape1 <- plot_species_dom_landscape(species_dom2, "landscape 1")
species_dom_plot_landscape2 <- plot_species_dom_landscape(species_dom2, "landscape 2")
species_dom_plot_landscape3 <- plot_species_dom_landscape(species_dom2, "landscape 3")

species_dom_landscape1_gcm <-
  species_dom_plot_landscape1 /
  (gcm_climate_smooth_plot2 + (plot_spacer() + theme_void()) + plot_layout(widths = c(0.7, 0.3))) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA))) +
  plot_layout(nrow = 2, heights = c(0.6, 0.4))

species_dom_landscape2_gcm <-
  species_dom_plot_landscape2 /
  (gcm_climate_smooth_plot2 + (plot_spacer() + theme_void()) + plot_layout(widths = c(0.7, 0.3))) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA))) +
  plot_layout(nrow = 2, heights = c(0.6, 0.4))

species_dom_landscape3_gcm <-
  species_dom_plot_landscape3 /
  (gcm_climate_smooth_plot2 + (plot_spacer() + theme_void()) + plot_layout(widths = c(0.7, 0.3))) +
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA))) +
  plot_layout(nrow = 2, heights = c(0.6, 0.4))


ggsave(
  filename = "./figures/species_dom_landscape1_gcm.svg",
  plot = species_dom_landscape1_gcm,
  device = "svg",
  width = full_slide_width_cm,
  height = full_slide_height_cm,
  units = "cm",
  bg = "transparent"
)

ggsave(
  filename = "./figures/species_dom_landscape2_gcm.svg",
  plot = species_dom_landscape2_gcm,
  device = "svg",
  width = full_slide_width_cm,
  height = full_slide_height_cm,
  units = "cm",
  bg = "transparent"
)

ggsave(
  filename = "./figures/species_dom_landscape3_gcm.svg",
  plot = species_dom_landscape3_gcm,
  device = "svg",
  width = full_slide_width_cm,
  height = full_slide_height_cm,
  units = "cm",
  bg = "transparent"
)


#---------- Species area dominated (per-landscape x fire, facet by species) ----------#
# Split fire condition out into its own figure too, instead of linetype - each
# plot now shows exactly one landscape x one fire condition, so there is no
# solid/dashed distinction to squint at, just species (rows) x model (cols),
# with replicate spread still visible. 6 figures total (3 landscapes x 2 fire).

plot_species_dom_landscape_fire <- function(data, landscape_label, fire_label) {
  ggplot(
    dplyr::filter(data, landscape == landscape_label, fire == fire_label),
    aes(x = year_actual, y = area_ha, group = replicate, colour = sp_dom)
  ) +
    geom_line(linewidth = 0.5, alpha = 0.5) +
    facet_grid(sp_dom ~ model, scales = "free_y",
               labeller = labeller(sp_dom = SPECIES_STRIP_LABELS)) +
    scale_colour_species(guide = "none") +
    scale_x_continuous(breaks = YEAR_BREAKS_ECO, expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Year", y = "Dominant species area (ha)",
      title = paste(landscape_label, fire_label, sep = " · ")
    ) +
    theme_iland() +
    theme(
      strip.background = element_rect(fill = NA, colour = NA),
    )
}

landscape_fire_combos <- expand.grid(
  landscape_label = c("landscape 1", "landscape 2", "landscape 3"),
  fire_label      = c("Fire", "No fire"),
  stringsAsFactors = FALSE
)

species_dom_plots_landscape_fire <- Map(
  \(landscape_label, fire_label) {
    plot_species_dom_landscape_fire(species_dom2, landscape_label, fire_label)
  },
  landscape_fire_combos$landscape_label,
  landscape_fire_combos$fire_label
)
names(species_dom_plots_landscape_fire) <- paste(
  gsub(" ", "", landscape_fire_combos$landscape_label),
  gsub(" ", "", landscape_fire_combos$fire_label),
  sep = "_"
)

# species_dom_plots_landscape_fire[["landscape1_Fire"]]
# species_dom_plots_landscape_fire[["landscape1_Nofire"]]

# for (nm in names(species_dom_plots_landscape_fire)) {
#   ggsave(
#     filename = paste0("./figures/species_dom_", nm, ".svg"),
#     plot = species_dom_plots_landscape_fire[[nm]],
#     device = "svg",
#     width = full_slide_width_cm,
#     height = full_slide_height_cm,
#     units = "cm"
#   )
# }