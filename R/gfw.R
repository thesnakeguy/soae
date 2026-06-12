###############################################################################
# --------------------------- BIOLOGY ------------------------------------ ####
###############################################################################

###############################################################################
## FISHERIES: GLOBAL FISHING WATCH (GFW) ####
###############################################################################

# =============================================================================
# 1. DATA FETCHING & CACHING
# =============================================================================

#' Fetch one year of GFW fishing-effort data
#'
#' Large regional queries against the Global Fishing Watch API. Add your GFW key to
#' ~/.Renviron:  GFW_TOKEN = your_token_here
#' Then restart R. Obtain a free token at https://globalfishingwatch.org/our-apis/
#'
#' @param year       Integer. Calendar year to fetch (e.g. \code{2023}).
#' @param group      Character. Grouping variable passed to
#'   \code{gfw_ais_fishing_hours()}. Defaults to \code{"GEARTYPE"}.
#' @param res        Character. Temporal resolution: \code{"YEARLY"} or
#'   \code{"MONTHLY"}. Defaults to \code{"YEARLY"}.
#'
#' @return A data frame returned by \code{gfw_ais_fishing_hours()}
#'
#' @importFrom gfwr gfw_ais_fishing_hours gfw_auth
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' df_2023    <- download_gfw_year(2023)
#' }
#'
#' @export
download_gfw_year <- function(year,
                              region_id = "CCAMLR",
                              key = gfwr::gfw_auth(),
                              group       = "GEARTYPE",
                              res         = "YEARLY") {

  message(glue::glue("  Fetching {year} [{group} / {res}]..."))

  result <- tryCatch(
    gfwr::gfw_ais_fishing_hours(
      spatial_resolution  = "LOW",
      temporal_resolution = res,
      group_by            = group,
      start_date          = glue::glue("{year}-01-01"),
      end_date            = glue::glue("{year}-12-31"),
      region              = region_id,
      region_source       = "RFMO",
      key                 = key
    ),
    error = function(e) {
      message(glue::glue("    Initial call error: {conditionMessage(e)}"))
      NULL
    }
  )

  result
}



#' Fetch and cache multi-annual gridded fishing-effort data
#'
#' Queries the GFW API for annual, gear-type-grouped fishing effort over the
#' CCAMLR convention area for each year in \code{years}. Results are saved to
#' an RDS cache file so subsequent calls skip the API entirely. The cache is
#' validated on load and automatically replaced if it is empty or corrupt.
#'
#' @param years      Integer vector. Years to fetch, e.g.
#'   \code{2016:2025}.
#' @param cache_file Character. Path to the RDS cache file. Defaults to
#'   \code{"cache_heatmap_data.rds"}.
#'
#' @return A data frame with one row per year × gear type × grid cell,
#'   containing the raw columns returned by the GFW API plus an integer
#'   \code{year} column.
#'
#' @seealso [download_gfw_year()], [download_gfw_monthly()]
#'
#' @importFrom purrr map map2 compact
#' @importFrom dplyr bind_rows mutate
#' @importFrom glue glue
#' @importFrom gfwr gfw_auth
#'
#' @examples
#' \dontrun{
#' gfw_df <- download_gfw_data(years = c(2016:2025))
#' }
#'
#' @export
download_gfw_data <- function(years,
                              cache_file = "cache_gfw_data.rds") {

  # ── Load & validate cache ──────────────────────────────────────────────────
  if (file.exists(cache_file)) {
    existing <- readRDS(cache_file)
    if (nrow(existing) == 0 || ncol(existing) == 0) {
      message("Heatmap cache is empty or corrupt \u2014 deleting and re-fetching.")
      file.remove(cache_file)
    } else {
      message(glue::glue("Valid heatmap cache found ({nrow(existing)} rows). Loading."))
      return(existing)
    }
  }

  # ── Fetch from API ─────────────────────────────────────────────────────────
  message("Fetching gfw data from GFW API (one call per year)...")

  raw_list <- purrr::map(
    years,
    ~ download_gfw_year(as.vector(.x),
                        region_id = "CCAMLR",
                        key       = gfwr::gfw_auth(),
                        group     = "GEARTYPE",
                        res       = "YEARLY")
  )

  gfw_df <- purrr::map2(raw_list, years, function(df, yr) {
    if (is.null(df)) return(NULL)
    dplyr::mutate(df, year = as.integer(yr))
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(gfw_df) == 0) {
    stop("All gfw year fetches failed. Check your API token and connection.")
  }

  saveRDS(gfw_df, cache_file)
  message(glue::glue("gfw data cached: {nrow(gfw_df)} rows -> {cache_file}"))

  gfw_df
}


#' Fetch and cache monthly fishing-effort trend data
#'
#' Queries the GFW API for monthly, flag-grouped fishing effort over the
#' CCAMLR convention area for each year in \code{trend_start:current_year}.
#' Results are saved to an RDS cache file. The cache is validated on load and
#' automatically replaced if empty or corrupt.
#'
#' @param year_start  Integer. First year of the trend period (inclusive).
#' @param year_end Integer. Last year of the trend period (inclusive).
#' @param cache_file   Character. Path to the RDS cache file. Defaults to
#'   \code{"cache_month_data.rds"}.
#'
#' @return A data frame with one row per year × flag × month × grid cell,
#'   containing raw GFW API columns plus an integer \code{year} column.
#'
#' @seealso [download_gfw_year()], [download_gfw_data()]
#'
#' @importFrom purrr map map2 compact
#' @importFrom dplyr bind_rows mutate
#' @importFrom glue glue
#' @importFrom gfwr gfw_auth
#'
#' @examples
#' \dontrun{
#' gfw_monthly_df  <- download_gfw_monthly(2016, 2025)
#' }
#'
#' @export
download_gfw_monthly <- function(year_start,
                             year_end,
                             cache_file = "cache_month_data.rds") {

  # ── Load & validate cache ──────────────────────────────────────────────────
  if (file.exists(cache_file)) {
    existing_t <- readRDS(cache_file)
    if (nrow(existing_t) == 0 || ncol(existing_t) == 0) {
      message("Month cache is empty or corrupt \u2014 deleting and re-fetching.")
      file.remove(cache_file)
    } else {
      message(glue::glue("Valid month cache found ({nrow(existing_t)} rows). Loading."))
      return(existing_t)
    }
  }

  # ── Fetch from API ─────────────────────────────────────────────────────────
  message("Fetching month data from GFW API (one call per year)...")
  years <- year_start:year_end

  raw_trend_list <- purrr::map(
    years,
    ~ download_gfw_year(.x,
                        region_id = "CCAMLR",
                        key       = gfwr::gfw_auth(),
                        group     = "FLAG",
                        res       = "MONTHLY")
  )

  gfw_month_df <- purrr::map2(raw_trend_list, years, function(df, yr) {
    if (is.null(df)) return(NULL)
    dplyr::mutate(df, year = as.integer(yr))
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(gfw_month_df) == 0) {
    stop("All trend fetches failed. Check your API token and connection.")
  }

  saveRDS(gfw_month_df, cache_file)
  message(glue::glue("Trend data cached: {nrow(gfw_month_df)} rows -> {cache_file}"))

  gfw_month_df
}


# =============================================================================
# 2. PLOT FUNCTIONS
# =============================================================================

#' Plot annual fishing-effort heatmap panels for the Antarctic Peninsula
#'
#' Produces a multi-panel map (one panel per year) showing AIS-based apparent
#' fishing hours per 0.1° grid cell on a log₁₀ scale. All panels share the
#' same colour axis for direct year-to-year comparison.
#'
#' @param gfw_df  Data frame. Raw output from [download_gfw_data()].
#' @param years        Integer vector. Years to display (one panel each).
#' @param ncol         Integer. Number of columns in the panel layout.
#'   Defaults to \code{2}.
#' @param xlim         Numeric vector of length 2. Longitude limits of the
#'   map window. Defaults to the Antarctic Peninsula \code{c(-80, -35)}.
#' @param ylim         Numeric vector of length 2. Latitude limits of the
#'   map window. Defaults to the Antarctic Peninsula \code{c(-75, -55)}.
#'
#' @return A [patchwork::patchwork-package] object containing all annual panels plus a shared
#'   title and caption. Print or pass to [ggplot2::ggsave()].
#'
#' @importFrom dplyr rename group_by summarise mutate filter all_of
#' @importFrom ggplot2 ggplot geom_tile geom_sf aes coord_sf labs theme element_text element_blank guide_colorbar unit
#' @importFrom scico scale_fill_scico
#' @importFrom rnaturalearth ne_countries
#' @importFrom sf st_crs
#' @importFrom purrr map
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' gfw_df <- download_gfw_data(years = c(2016:2025))
#' p <- plot_gfw_heatmap_panels(gfw_df, years = c(2016:2025))
#' print(p)
#' ggplot2::ggsave("GFW_plot.pdf", p, width = 12, height = 16)
#' }
#'
#' @export
plot_gfw_heatmap_panels <- function(gfw_df,
                                years,
                                ncol = 2,
                                xlim = c(-80, -35),
                                ylim = c(-75, -55)) {

  # ── Wrangle ────────────────────────────────────────────────────────────────
  heatmap_clean <- gfw_df |>
    dplyr::rename(
      lat           = dplyr::all_of("Lat"),
      lon           = dplyr::all_of("Lon"),
      time_range    = dplyr::all_of("Time Range"),
      fishing_hours = dplyr::all_of("Apparent Fishing Hours")
    )

  heatmap_total <- heatmap_clean |>
    dplyr::group_by(year, lat, lon) |>
    dplyr::summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(log_hours = log10(fishing_hours + 1))

  # ── Shared spatial objects ─────────────────────────────────────────────────
  world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  so_coord <- ggplot2::coord_sf(
    xlim   = xlim,
    ylim   = ylim,
    expand = FALSE,
    crs    = sf::st_crs(4326)
  )

  log_max <- max(heatmap_total$log_hours, na.rm = TRUE)

  # ── Single-year panel builder ──────────────────────────────────────────────
  make_panel <- function(yr) {
    df_yr <- dplyr::filter(heatmap_total, year == yr)

    ggplot2::ggplot() +
      ggplot2::geom_tile(
        data = df_yr,
        ggplot2::aes(x = lon, y = lat, fill = log_hours),
        linewidth = 0
      ) +
      ggplot2::geom_sf(
        data      = world_sf,
        fill      = "grey25",
        colour    = "grey15",
        linewidth = 0.15
      ) +
      scico::scale_fill_scico(
        palette  = "lajolla",
        na.value = "transparent",
        name     = "log10(hrs)",
        limits   = c(0, log_max),
        guide    = ggplot2::guide_colorbar(
          title.position = "top",
          barwidth       = ggplot2::unit(0.35, "cm"),
          barheight      = ggplot2::unit(3.5, "cm")
        )
      ) +
      so_coord +
      ggplot2::labs(title = as.character(yr)) +
      theme_antarctic() +
      ggplot2::theme(
        axis.title   = ggplot2::element_blank(),
        axis.text    = ggplot2::element_text(size = 7),
        plot.title   = ggplot2::element_text(size = 12, face = "bold", hjust = 0.5),
        legend.title = ggplot2::element_text(size = 8),
        legend.text  = ggplot2::element_text(size = 7)
      )
  }

  panels <- purrr::map(years, make_panel)

  # ── Assemble with patchwork ────────────────────────────────────────────────
  patchwork::wrap_plots(panels, ncol = ncol) +
    patchwork::plot_annotation(
      title    = "Annual Fishing Effort Hotspots \u2014 Antarctic Peninsula",
      subtitle = glue::glue(
        "AIS-based apparent fishing hours per 0.1\u00b0 grid cell, ",
        "{min(years)}\u2013{max(years)}"
      ),
      caption  = paste0(
        "Data source: Global Fishing Watch (globalfishingwatch.org) \u2014 ",
        "AIS-based apparent fishing effort (public-global-fishing-effort:v3.0)"
      ),
      theme    = ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", size = 16),
        plot.subtitle = ggplot2::element_text(colour = "grey40", size = 12),
        plot.caption  = ggplot2::element_text(colour = "grey55", size = 8, hjust = 0)
      )
    )
}


#' Plot annual total fishing effort trend for the Antarctic Peninsula
#'
#' Displays total apparent fishing hours per year as a scatter-line chart
#' overlaid with a LOESS smooth (95% CI). Each point is labelled with its
#' rounded value and colour-coded by magnitude. Coordinate boundaries default to
#' the Antarctic Peninsula.
#'
#' @param gfw_df    Data frame. Raw output from [download_gfw_data()].
#' @param trend_start  Integer. First year shown on the x-axis.
#' @param trend_end Integer. Last year shown on the x-axis.
#' @param lat_min      Numeric. Southern latitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-80}.
#' @param lat_max      Numeric. Northern latitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-35}.
#' @param lon_min      Numeric. Western longitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-75}.
#' @param lon_max      Numeric. Eastern longitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-55}.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom dplyr rename filter group_by summarise mutate all_of
#' @importFrom ggplot2 ggplot aes geom_smooth geom_line geom_point scale_x_continuous scale_y_continuous expansion labs
#' @importFrom ggrepel geom_text_repel
#' @importFrom scico scale_colour_scico
#' @importFrom scales comma
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' gfw_df <- download_gfw_data(years = c(2016:2025))
#' p <- plot_gfw_annual_trend(gfw_df, trend_start = 2016, trend_end = 2020)
#' print(p)
#' }
#'
#' @export
plot_gfw_annual_trend <- function(gfw_df,
                              trend_start,
                              trend_end,
                              lat_min = -80,
                              lat_max = -35,
                              lon_min = -75,
                              lon_max = -55) {

  # ── Wrangle ────────────────────────────────────────────────────────────────
  annual_totals <- gfw_df |>
    dplyr::rename(
      lat           = dplyr::all_of("Lat"),
      lon           = dplyr::all_of("Lon"),
      time_range    = dplyr::all_of("Time Range"),
      fishing_hours = dplyr::all_of("Apparent Fishing Hours")
    ) |>
    dplyr::filter(
      lat > lat_min, lat < lat_max,
      lon > lon_min, lon < lon_max
    ) |>
    dplyr::group_by(year) |>
    dplyr::summarise(annual_hours = sum(fishing_hours, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::mutate(cal_year = as.integer(year))

  # ── Plot ───────────────────────────────────────────────────────────────────
  ggplot2::ggplot(annual_totals,
                  ggplot2::aes(x = cal_year, y = annual_hours / 1e3)) +
    ggplot2::geom_smooth(
      method    = "loess",
      span      = 0.9,
      colour    = "#1B6CA8",
      fill      = "#1B6CA8",
      alpha     = 0.15,
      linewidth = 0.9
    ) +
    ggplot2::geom_line(
      colour    = "grey55",
      linewidth = 0.6,
      linetype  = "dashed"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(colour = annual_hours / 1e6),
      size         = 4.5,
      show.legend  = FALSE
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = paste0(round(annual_hours / 1e3, 1), "")),
      size               = 3,
      colour             = "grey30",
      nudge_y            = 0.06 * max(annual_totals$annual_hours / 1e3),
      min.segment.length = 0.4
    ) +
    scico::scale_colour_scico(palette = "bamako", direction = -1) +
    ggplot2::scale_x_continuous(breaks = trend_start:trend_end) +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0, 0.12))
    ) +
    ggplot2::labs(
      title    = "Total Annual Fishing Effort \u2014 Antarctic Peninsula",
      subtitle = glue::glue(
        "Apparent fishing hours (all AIS-equipped fishing vessels), ",
        "{trend_start}\u2013{trend_end}"
      ),
      x       = NULL,
      y       = "Fishing hours (thousands)",
      caption = paste0(
        "Data source: Global Fishing Watch (globalfishingwatch.org) \u2014 ",
        "AIS-based apparent fishing effort (public-global-fishing-effort:v3.0)"
      )
    ) +
    theme_antarctic()
}


#' Plot month-by-year standardised fishing effort anomaly heatmap
#'
#' Computes and displays the standardized deviation (z-score) of monthly
#' fishing effort from the long-term monthly mean. Blue tiles indicate
#' below-average months; red indicates above-average. Z-scores are printed
#' inside each tile. Coordinate bounderies default to the Antarctic Peninsula.
#'
#' @param gfw_df    Data frame. Raw output from [download_gfw_monthly()].
#' @param lat_min      Numeric. Southern latitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-80}.
#' @param lat_max      Numeric. Northern latitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-35}.
#' @param lon_min      Numeric. Western longitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-75}.
#' @param lon_max      Numeric. Eastern longitude boundary for filtering. Defaults to the Antarctic Peninsula.
#'   Defaults to \code{-55}.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom dplyr rename filter group_by summarise mutate left_join if_else all_of
#' @importFrom stringr str_replace
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_y_discrete labs theme element_blank element_text expansion guide_colorbar unit
#' @importFrom scico scale_fill_scico
#' @importFrom scales squish
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' gfw_monthly_df <- download_gfw_monthly(year_start = 2015, year_end = 2020)
#' p <- plot_gfw_monthly_anomaly(gfw_monthly_df)
#' print(p)
#' }
#'
#' @export
plot_gfw_monthly_anomaly <- function(gfw_df,
                                 lat_min = -80,
                                 lat_max = -35,
                                 lon_min = -75,
                                 lon_max = -55) {

  # ── Wrangle ────────────────────────────────────────────────────────────────
  trend_clean <- gfw_df |>
    dplyr::rename(
      lat           = dplyr::all_of("Lat"),
      lon           = dplyr::all_of("Lon"),
      time_range    = dplyr::all_of("Time Range"),
      fishing_hours = dplyr::all_of("Apparent Fishing Hours")
    ) |>
    dplyr::filter(
      lat > lat_min, lat < lat_max,
      lon > lon_min, lon < lon_max
    )

  trend_dated <- trend_clean |>
    dplyr::mutate(
      cal_year  = stringr::str_replace(time_range, "^(\\d{4})-\\d{2}$", "\\1"),
      cal_month = as.factor(stringr::str_replace(
        time_range, "^\\d{4}-(\\d{2})$", "\\1"
      )),
      month_lbl = factor(month.abb[as.integer(cal_month)], levels = month.abb)
    )

  monthly_totals <- trend_dated |>
    dplyr::group_by(cal_year, cal_month, month_lbl) |>
    dplyr::summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE),
                     .groups = "drop")

  monthly_clim <- monthly_totals |>
    dplyr::group_by(cal_month) |>
    dplyr::summarise(
      clim_mean = mean(fishing_hours, na.rm = TRUE),
      clim_sd   = sd(fishing_hours,   na.rm = TRUE),
      .groups   = "drop"
    )

  monthly_anom <- monthly_totals |>
    dplyr::left_join(monthly_clim, by = "cal_month") |>
    dplyr::mutate(anomaly_std = dplyr::if_else(
      clim_sd > 0,
      (fishing_hours - clim_mean) / clim_sd,
      0
    ))

  # ── Plot ───────────────────────────────────────────────────────────────────
  monthly_anom |>
    ggplot2::ggplot(ggplot2::aes(
      x    = factor(cal_year),
      y    = month_lbl,
      fill = anomaly_std
    )) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.5) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%+.1f", anomaly_std)),
      size   = 2.8,
      colour = "grey15"
    ) +
    scico::scale_fill_scico(
      palette = "vik",
      limits  = c(-3, 3),
      oob     = scales::squish,
      name    = "Z-score",
      guide   = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth       = ggplot2::unit(7, "cm"),
        barheight      = ggplot2::unit(0.5, "cm")
      )
    ) +
    ggplot2::scale_y_discrete(limits = rev(month.abb)) +
    ggplot2::labs(
      title    = "Monthly Fishing Effort Anomaly \u2014 Antarctic Peninsula",
      subtitle = glue::glue(
        "Standardised deviation from long-term monthly mean"
      ),
      x       = NULL,
      y       = NULL,
      caption = paste0(
        "Data source: Global Fishing Watch (globalfishingwatch.org) \u2014 ",
        "AIS-based apparent fishing effort (public-global-fishing-effort:v3.0)"
      )
    ) +
    theme_antarctic() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid      = ggplot2::element_blank(),
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1)
    )
}


#' Plot combined annual trend + monthly anomaly panel
#'
#' Stacks the annual effort trend (panel A) and the monthly anomaly heatmap
#' (panel B) vertically using [patchwork::patchwork-package]. Defaults to the Antarctic Peninsula.
#'
#' @param gfw_df    Data frame. Raw output from [download_gfw_data()].
#' @param trend_start  Integer. First year of the trend period.
#' @param trend_end Integer. Last year of the trend period.
#' @param lat_min      Numeric. Southern latitude boundary. Defaults to
#'   \code{-80}, the Antarctic Peninsula.
#' @param lat_max      Numeric. Northern latitude boundary. Defaults to
#'   \code{-35}, the Antarctic Peninsula.
#' @param lon_min      Numeric. Western longitude boundary. Defaults to
#'   \code{-75}, the Antarctic Peninsula.
#' @param lon_max      Numeric. Eastern longitude boundary. Defaults to
#'   \code{-55}, the Antarctic Peninsula.
#'
#' @return A [patchwork::patchwork-package] object.
#'
#' @seealso [plot_gfw_annual_trend()], [plot_gfw_monthly_anomaly()]
#'
#' @importFrom patchwork plot_annotation
#' @importFrom ggplot2 theme element_text
#'
#' @examples
#' \dontrun{
#' gfw_df <- download_gfw_data(years = c(2015:2025))
#' p <- plot_gfw_temporal_composite(gfw_df, trend_start = 2016, trend_end = 2025)
#' print(p)
#' ggplot2::ggsave("GFW_AnnualMonthly_trend.pdf", p, width = 12, height = 13)
#' }
#'
#' @export
plot_gfw_temporal_composite <- function(gfw_df,
                                    trend_start,
                                    trend_end,
                                    lat_min = -80,
                                    lat_max = -35,
                                    lon_min = -75,
                                    lon_max = -55) {

  p_annual <- plot_gfw_annual_trend(
    gfw_df, trend_start, trend_end,
    lat_min, lat_max, lon_min, lon_max
  )
  p_anom <- plot_gfw_monthly_anomaly(
    gfw_df, trend_start, trend_end,
    lat_min, lat_max, lon_min, lon_max
  )

  (p_annual / p_anom) +
    patchwork::plot_annotation(
      tag_levels = "A",
      theme      = ggplot2::theme(
        plot.tag = ggplot2::element_text(face = "bold", size = 14)
      )
    )
}


#' Plot annual fishing effort by gear type (stacked bar chart)
#'
#' Aggregates annual fishing hours by gear type and displays them as a stacked
#' bar chart. Uncommon gear types are grouped into an "Other" category. Coordinates are by default targetting
#' the Antarctic Peninsula.
#'
#' @param gfw_df  Data frame. Raw output from [download_gfw_data()].
#' @param years        Integer vector. Years to display on the x-axis.
#' @param keep_gears   Character vector. Named gear types to show individually;
#'   all others are collapsed into \code{"Other"}. Defaults to the eight most
#'   common Southern Ocean gear types.
#' @param lat_min      Numeric. Southern latitude boundary. Defaults to
#'   \code{-80}, the Antarctic Peninsula.
#' @param lat_max      Numeric. Northern latitude boundary. Defaults to
#'   \code{-35}, the Antarctic Peninsula.
#' @param lon_min      Numeric. Western longitude boundary. Defaults to
#'   \code{-75}, the Antarctic Peninsula.
#' @param lon_max      Numeric. Eastern longitude boundary. Defaults to
#'   \code{-55}, the Antarctic Peninsula.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @importFrom dplyr rename group_by summarise mutate case_when any_of all_of
#' @importFrom stringr str_replace_all str_to_title
#' @importFrom ggplot2 ggplot aes geom_col scale_y_continuous expansion labs theme element_text
#' @importFrom scico scale_fill_scico_d
#' @importFrom scales comma
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' gfw_df <- download_gfw_data(years = c(2016:2025))
#' p <- plot_gfw_gear_trend(gfw_df, years = c(2016:2025))
#' print(p)
#' ggplot2::ggsave("GFW_FisheriesGear.pdf", p, width = 12, height = 8)
#' }
#'
#' @export
plot_gfw_gear_trend <- function(gfw_df,
                            years,
                            lat_min = -80,
                            lat_max = -35,
                            lon_min = -75,
                            lon_max = -55,
                            keep_gears = c("trawlers", "drifting_longlines",
                                           "set_longlines", "squid_jigger",
                                           "pole_and_line", "purse_seines",
                                           "set_gillnets", "pots_and_traps")) {

  # ── Wrangle ────────────────────────────────────────────────────────────────
  gear_annual <- gfw_df |>
    dplyr::rename(
      lat           = dplyr::all_of("Lat"),
      lon           = dplyr::all_of("Lon"),
      time_range    = dplyr::all_of("Time Range"),
      fishing_hours = dplyr::all_of("Apparent Fishing Hours")
    ) |>
    dplyr::filter(
      lat > lat_min, lat < lat_max,
      lon > lon_min, lon < lon_max
    ) |>
    dplyr::rename(geartype = dplyr::any_of("geartype")) |>
    dplyr::group_by(year, geartype) |>
    dplyr::summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE),
                     .groups = "drop")

  gear_plot <- gear_annual |>
    dplyr::mutate(
      gear_clean = dplyr::case_when(
        geartype %in% keep_gears ~ geartype,
        TRUE                     ~ "other"
      ),
      gear_clean = stringr::str_replace_all(gear_clean, "_", " ") |>
        stringr::str_to_title()
    ) |>
    dplyr::group_by(year, gear_clean) |>
    dplyr::summarise(fishing_hours = sum(fishing_hours, na.rm = TRUE),
                     .groups = "drop")

  # ── Plot ───────────────────────────────────────────────────────────────────
  ggplot2::ggplot(gear_plot,
                  ggplot2::aes(
                    x    = factor(year),
                    y    = fishing_hours / 1e3,
                    fill = gear_clean
                  )) +
    ggplot2::geom_col(
      position  = "stack",
      colour    = "white",
      linewidth = 0.25
    ) +
    scico::scale_fill_scico_d(palette = "batlow", name = "Gear type") +
    ggplot2::scale_y_continuous(
      labels = scales::comma,
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      title    = "Fishing Effort by Gear Type \u2014 Antarctic Peninsula",
      subtitle = glue::glue(
        "Annual apparent fishing hours (\u00d710\u00b3), ",
        "{min(years)}\u2013{max(years)}"
      ),
      x       = NULL,
      y       = "Fishing hours (thousands)",
      caption = paste0(
        "Data source: Global Fishing Watch (globalfishingwatch.org) \u2014 ",
        "AIS-based apparent fishing effort (public-global-fishing-effort:v3.0)"
      )
    ) +
    theme_antarctic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
