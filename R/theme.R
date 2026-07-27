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


# ── GCM identity ───────────────────────────────────────────────────────────────
# Colourblind-safe categorical palette, fixed per model so the same GCM is
# always the same colour across every chart in the deck. Upstream datasets are
# inconsistent about casing ("NorESM2-MM" in the downscaling pipeline vs
# "NorEsm2-MM" in fire_annual.parquet) so both spellings are keyed here rather
# than relying on every dataset being recoded to match.

GCM_COLOURS <- c(
  "TaiESM1"     = "#2a78d6", # blue
  "UKESM1-0-LL" = "#eb6834", # orange
  "NorESM2-MM"  = "#1baf7a", # aqua
  "NorEsm2-MM"  = "#1baf7a"  # aqua (alt casing seen in fire_annual.parquet)
)


# ── ggplot2 theme ─────────────────────────────────────────────────────────────
# Built on theme_classic() (clean white background, no default gridlines)
# then adds a subtle major grid. Matches the Plotly "iland" template visually.

theme_iland <- function(base_size = 14) {
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


# ── Dark-mode chart chrome ─────────────────────────────────────────────────────
# For slides.qmd (revealjs theme: dark, confirmed slide surface #191919,
# body text #fff). Computed against that exact surface via OKLCH lightness +
# WCAG contrast (no eyeballing) rather than a blind colour-invert.
#
# GCM_COLOURS needs no dark variant: all three hues already sit at/within the
# dark-mode lightness band (OKLCH L ~0.48-0.67) and clear 3:1 contrast against
# #191919 unchanged (TaiESM1 L=0.575 contrast=3.98; UKESM1-0-LL L=0.671
# contrast=5.49; NorESM2-MM L=0.669 contrast=6.25).
#
# SPECIES_COLOURS needed two changes:
#   - Pima (#1b5e20): L=0.425, contrast only 2.23:1 against #191919 - fails
#     the 3:1 floor outright, would nearly vanish on a dark slide. Replaced
#     with #388e3c (L=0.575, contrast=4.27), which keeps the same hue family
#     and stays the darkest of the three greens (Pigl L=0.718, Mixed.spruce
#     L=0.705) while clearing both the band and the contrast floor.
#   - Potr (#fdd835): not a failure (contrast=12.6:1) but far enough above
#     the comfort band (L=0.888) to read as glare next to near-black.
#     Dimmed to #e8b923 (L=0.806, contrast=9.54) for comfort, not correctness.
SPECIES_COLOURS_DARK <- SPECIES_COLOURS
SPECIES_COLOURS_DARK["Pima"] <- "#388e3c"
SPECIES_COLOURS_DARK["Potr"] <- "#e8b923"

theme_iland_dark <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      # Transparent, not a hardcoded fill - so the SVG blends into whatever
      # the slide surface actually is instead of risking a visible seam
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background  = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key         = element_rect(fill = "transparent", colour = NA),

      axis.line  = element_line(colour = "#383835"),
      axis.ticks = element_line(colour = "#383835"),
      axis.text  = element_text(colour = "#c3c2b7"),
      axis.title = element_text(colour = "#ffffff"),

      # Subtle major grid only — minor grid adds visual noise
      panel.grid.major = element_line(colour = "#2c2c2a", linewidth = 0.4),
      panel.grid.minor = element_blank(),

      # Facet strip styling — slightly lighter than the surface for
      # separation, not the light theme's pale grey
      strip.background = element_rect(fill = "#2a2a2a", colour = NA),
      strip.text = element_text(face = "bold", size = base_size * 0.9, colour = "#ffffff"),

      # Legend at bottom, horizontal — matches the light theme's layout
      legend.position = "bottom",
      legend.title = element_text(size = base_size * 0.9, colour = "#ffffff"),
      legend.text  = element_text(colour = "#c3c2b7"),
      legend.key.size = unit(0.8, "lines"),

      # Title hierarchy
      plot.title    = element_text(size = base_size * 1.1, face = "bold", colour = "#ffffff"),
      plot.subtitle = element_text(size = base_size * 0.95, colour = "#c3c2b7"),
      plot.caption  = element_text(size = base_size * 0.8,  colour = "#898781"),

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

# na.value unchanged from the light variant (#bdbdbd against #191919 is
# still a comfortable 9.36:1 contrast)
scale_fill_species_dark <- function(na.value = "#bdbdbd", ...) {
  scale_fill_manual(
    values   = SPECIES_COLOURS_DARK,
    labels   = SPECIES_LABELS,
    na.value = na.value,
    ...
  )
}

scale_colour_species_dark <- function(na.value = "#bdbdbd", ...) {
  scale_colour_manual(
    values   = SPECIES_COLOURS_DARK,
    labels   = SPECIES_LABELS,
    na.value = na.value,
    ...
  )
}

scale_fill_gcm <- function(...) {
  scale_fill_manual(values = GCM_COLOURS, ...)
}

scale_colour_gcm <- function(...) {
  scale_colour_manual(values = GCM_COLOURS, ...)
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
