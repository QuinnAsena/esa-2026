# plots/theme.R
# Shared visual theme for all iLand ggplot2 charts.
#
# Source this file at the top of any R plotting script:
#   source("plots/theme.R")
#
# Then use:
#   p <- ggplot(df, aes(...)) + geom_*() + theme_iland() + scale_fill_species()

library(ggplot2)

# ── Species identity ──────────────────────────────────────────────────────────

SPECIES_COLOURS <- c(
  Pima              = "#1b5e20",  # Picea mariana        — dark green
  Pigl              = "#66bb6a",  # Picea glauca         — light green
  Mixed.spruce      = "#7cb342",  # Mixed spruce         — yellow-green
  Bene              = "#e65100",  # Betula neoalaskana   — burnt orange
  Potr              = "#fdd835",  # Populus tremuloides  — yellow
  Mixed.deciduous   = "#f57c00",  # Mixed deciduous      — dark orange
  `Not forested`    = "#78909c"   # Non-forest           — blue-grey
)

SPECIES_LABELS <- c(
  Pima              = "Black spruce (Pima)",
  Pigl              = "White spruce (Pigl)",
  Mixed.spruce      = "Mixed spruce",
  Bene              = "Alaska birch (Bene)",
  Potr              = "Trembling aspen (Potr)",
  Mixed.deciduous   = "Mixed deciduous",
  `Not forested`    = "Not forested"
)

# Consistent stack / legend ordering (conifers -> mixed -> deciduous -> non-forest)
SPECIES_ORDER <- c("Pima", "Pigl", "Potr", "Bene", "Mixed.spruce",   "Mixed.deciduous", "Not forested")


# ── SSP identity ───────────────────────────────────────────────────────────────
# Colourblind-safe categorical palette, fixed per scenario so the same SSP is
# always the same colour across every chart in the deck.

SSP_COLOURS <- c(
  ssp126 = "#1baf7a", # aqua
  ssp245 = "#2a78d6", # blue
  ssp370 = "#eb6834"  # orange
)


# ── ggplot2 theme ─────────────────────────────────────────────────────────────
# Built on theme_classic() (clean white background, no default gridlines)
# then adds a subtle major grid. Matches the Plotly "iland" template visually.

theme_iland <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      # Subtle major grid only — minor grid adds visual noise
      panel.grid.major = element_line(colour = "#eeeeee", linewidth = 0.4),
      panel.grid.minor = element_blank(),

      # Facet strip styling
      strip.background = element_rect(fill = "#f5f5f5", colour = NA),
      strip.text = element_text(face = "bold", size = base_size * 0.9),

      # Legend at bottom, horizontal — matches Plotly template
      legend.position = "bottom",
      legend.title = element_text(size = base_size * 0.9),
      legend.key.size = unit(0.8, "lines"),

      # Title hierarchy
      plot.title    = element_text(size = base_size * 1.1, face = "bold", colour = "#222222"),
      plot.subtitle = element_text(size = base_size * 0.95, colour = "#555555"),
      plot.caption  = element_text(size = base_size * 0.8,  colour = "#888888"),

      # Breathing room around the plot
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
}


# ── Convenience scale functions ───────────────────────────────────────────────
# Wrapping scale_fill/colour_manual means chart code never needs to paste
# the hex codes — just call scale_fill_species() and the palette is applied.

scale_fill_species <- function(na.value = "#bdbdbd", ...) {
  scale_fill_manual(
    values   = SPECIES_COLOURS,
    labels   = SPECIES_LABELS,
    na.value = na.value,
    ...
  )
}

scale_colour_species <- function(na.value = "#bdbdbd", ...) {
  scale_colour_manual(
    values   = SPECIES_COLOURS,
    labels   = SPECIES_LABELS,
    na.value = na.value,
    ...
  )
}

scale_fill_ssp <- function(...) {
  scale_fill_manual(values = SSP_COLOURS, ...)
}

scale_colour_ssp <- function(...) {
  scale_colour_manual(values = SSP_COLOURS, ...)
}


# ── rep_sp colour helpers ─────────────────────────────────────────────────────
# For boxplots where fill = rep_sp (e.g. "Pima_1", "Pima_2") but boxes should
# be coloured by species. Maps every rep_sp to the colour of its parent species
# so all replicates within a species panel share the same hue.
#
# Usage:
#   ggplot(df, aes(x = soil_thick, y = seedl_dens_m2, fill = rep_sp)) +
#     geom_boxplot() +
#     scale_fill_rep_sp(df)   # no legend — species shown by facet label

rep_sp_colours <- function(df, species_col = "species", rep_sp_col = "rep_sp") {
  pairs <- unique(df[, c(species_col, rep_sp_col), drop = FALSE])
  # as.character() is required: if species is a factor, bare indexing into a named
  # vector uses factor level numbers (positional), not names — wrong colours result.
  setNames(SPECIES_COLOURS[as.character(pairs[[species_col]])], pairs[[rep_sp_col]])
}

scale_fill_rep_sp <- function(df, ...) {
  scale_fill_manual(values = rep_sp_colours(df), guide = "none", ...)
}
