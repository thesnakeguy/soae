###############################################################################
# --------------------------- CCAMLR METADATA ----------------------------- ####
###############################################################################

# Internal lookup constants used by make_plot() and make_map().
# Not exported.

area_order <- c(48, 58, 88)

subarea_order <- c(881, 882, 883,
                   481, 482, 483,
                   484, 485, 486,
                   584, 585, 587)

subarea_labels <- c(
  "881" = "88.1", "882" = "88.2", "883" = "88.3",
  "481" = "48.1", "482" = "48.2", "483" = "48.3",
  "484" = "48.4", "485" = "48.5", "486" = "48.6",
  "584" = "58.4", "585" = "58.5", "587" = "58.7"
)

# Pinned y-axis limits for area-level catch plots, by taxon code and area.
area_ylimits <- list(
  TOA = c("48" = 600,    "58" = 600,    "88" = 6000),
  TOP = c("48" = 10000,  "58" = 10000,  "88" = 100),
  KRI = c("48" = 600000, "58" = 600),
  ANI = c("48" = 1500,   "58" = 1500)
)

# Pinned colour-scale limits for subarea-level catch maps, by taxon.
map_limits <- list(
  KRI = list(limits = c(0, 400000), breaks = seq(0, 400000, by = 100000)),
  TOP = list(limits = c(0, 10000),  breaks = seq(0, 10000,  by = 2000)),
  TOA = list(limits = c(0, 3500),   breaks = seq(0, 3500,   by = 500)),
  ANI = list(limits = c(0, 1500),   breaks = seq(0, 1500,   by = 300))
)
