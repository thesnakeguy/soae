###############################################################################
## FISHERIES: CCAMLR STATISTICAL BULLETIN ####
###############################################################################

# =============================================================================
# 1. DATA FETCHING
# =============================================================================

#' Download and read the CCAMLR Statistical Bulletin
#'
#' @description
#' Downloads the CCAMLR Statistical Bulletin ZIP from ccamlr.org, extracts the
#' \code{Combined.csv} table, and returns it as a processed data frame. On
#' subsequent calls the cached local file is used unless \code{force_refresh =
#' TRUE}.
#'
#' The function handles both the lowercase (\code{ccamlr_statistical
#' bulletin_V\{n\}.zip}) and title-case (\code{CCAMLR_Statistical
#' Bulletin_V\{n\}.zip}) URL variants that CCAMLR has used across bulletin
#' versions, and auto-increments to find the latest available volume.
#'
#' @param volume Integer. The bulletin volume to start from. The function
#'   checks whether higher volumes exist and returns the latest one found.
#'   Default \code{38}.
#' @param force_refresh Logical. If \code{TRUE}, skips the local cache check
#'   and re-downloads from ccamlr.org. Default \code{FALSE}.
#'
#' @return A \code{\link[tibble]{tibble}} with one row per catch/effort record,
#'   including a derived \code{area_code} column (\code{"48"}, \code{"58"}, or
#'   \code{"88"}) mapped from the raw \code{asd_code}.
#' @export
#'
#' @examples
#' \dontrun{
#' ds <- get_catch_data()                       # use cached data or download latest
#' ds <- get_catch_data(force_refresh = TRUE)   # force fresh download
#' }
get_catch_data <- function(volume = 38, force_refresh = FALSE) {

  # --- Helpers ---
  make_url <- function(vol, titlecase = FALSE) {
    if (titlecase) {
      sprintf("https://www.ccamlr.org/en/system/files/CCAMLR_Statistical%%20Bulletin_V%d.zip", vol)
    } else {
      sprintf("https://www.ccamlr.org/en/system/files/CCAMLR_statistical%%20bulletin_V%d.zip", vol)
    }
  }

  url_exists <- function(url) {
    tryCatch({
      httr::HEAD(url)$status_code == 200
    }, error = function(e) FALSE)
  }

  check_vol <- function(vol) {
    if (url_exists(make_url(vol)))       return(list(vol = vol, titlecase = FALSE, url = make_url(vol)))
    if (url_exists(make_url(vol, TRUE))) return(list(vol = vol, titlecase = TRUE,  url = make_url(vol, TRUE)))
    NULL
  }

  find_csv <- function(vol, titlecase) {
    for (tc in c(titlecase, !titlecase)) {
      for (filename in c("Combined.csv", "combined.csv")) {
        p <- file.path("data",
                       if (tc) sprintf("CCAMLR_Statistical Bulletin_V%d", vol)
                       else     sprintf("CCAMLR_statistical bulletin_V%d", vol),
                       filename)
        if (file.exists(p)) return(p)
      }
    }
    NULL
  }

  read_and_process <- function(csv_path) {
    message("Reading: ", csv_path)
    read_csv(
      csv_path,
      show_col_types = FALSE,
      col_types = cols(
        .default  = col_guess(),
        asd_code  = col_character(),
        pot_count = col_double()
      )
    ) %>%
      filter(!if_all(everything(), is.na)) %>%
      mutate(area_code = case_when(
        .data$asd_code %in% c("48", "481", "482", "483", "484", "485", "486") ~ "48",
        .data$asd_code %in% c("58", "584", "5841", "5842", "5843", "5843a", "5843b", "5844", "5844a", "5844b", "585", "5851", "5852", "586", "587") ~ "58",
        .data$asd_code %in% c("88", "881", "882", "883") ~ "88",
        TRUE ~ NA_character_
      ))
  }

  # --- Directories ---
  for (d in c("data", "docs")) {
    if (!dir.exists(d)) dir.create(d)
  }

  # --- Check if already downloaded ---
  if (!force_refresh) {
    csv_path <- find_csv(volume, FALSE)
    if (!is.null(csv_path)) {
      message("Data already exists locally: ", csv_path)
      return(read_and_process(csv_path))
    }
  }

  # --- Find latest volume ---
  if (is.null(check_vol(volume))) {
    stop("Volume ", volume, " not found in either URL pattern - cannot proceed.")
  }

  result <- check_vol(volume)
  while (!is.null(check_vol(result$vol + 1))) {
    result <- check_vol(result$vol + 1)
  }

  message("Latest version found: Volume ", result$vol,
          if (result$titlecase) " (title case URL)" else " (lowercase URL)")

  # --- Download & unzip ---
  zip_file <- file.path("data", sprintf("ccamlr_statistical_bulletin_V%d.zip", result$vol))
  download.file(result$url, destfile = zip_file, mode = "wb")
  unzip(zip_file, exdir = "data")
  message("Downloaded and extracted Volume ", result$vol, " successfully.")

  # --- Locate & read CSV ---
  csv_path <- find_csv(result$vol, result$titlecase)
  if (is.null(csv_path)) {
    stop("Could not locate Combined.csv for Volume ", result$vol,
         " - checked both folder and file name casing combinations.")
  }

  ds <- read_and_process(csv_path)
  write_csv(ds, file.path("data", sprintf("ccamlr_combined_V%d.csv", result$vol)))
  message("Processed and written data to data/ccamlr_combined_V", result$vol, ".csv")

  return(ds)
}


# =============================================================================
# 2. DATA SUMMARISING
# =============================================================================

#' Summarise CCAMLR catch data by area and subarea
#'
#' @description
#' Reads the locally cached CCAMLR Statistical Bulletin CSV (written by
#' \code{\link{get_catch_data}}) and returns annual catch summaries at two
#' spatial resolutions: CCAMLR statistical areas (48, 58, 88) and subareas /
#' divisions.
#'
#' The function selects the highest available bulletin volume automatically
#' unless a specific volume is requested.
#'
#' @param taxa Character vector. CCAMLR taxon codes to include. Default
#'   \code{c("TOA", "TOP", "ANI", "KRI")} covering the commercially targeted
#'   toothfish and icefish species plus krill.
#' @param areas Character vector. Top-level statistical areas to retain.
#'   Default \code{c("48", "58", "88")}.
#' @param vol Integer or \code{NULL}. If supplied, reads from
#'   \code{data/ccamlr_combined_V\{vol\}.csv}. If \code{NULL} (default), the
#'   function locates the highest-version file in \code{data/}.
#'
#' @return A named list with two elements:
#'   \describe{
#'     \item{\code{$area}}{A \code{\link[tibble]{tibble}} of annual catch totals
#'       grouped by \code{season_ccamlr}, taxon, and \code{area_code}.}
#'     \item{\code{$subarea}}{A \code{\link[tibble]{tibble}} of annual catch
#'       totals grouped by \code{season_ccamlr}, taxon, and
#'       \code{asd_subarea_code}.}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' d <- make_catch_table()
#' d$area    # area-level summary
#' d$subarea # subarea-level summary
#' make_catch_table(taxa = "TOA", areas = "88")  # Ross Sea toothfish only
#' }
make_catch_table <- function(taxa  = c("TOA", "TOP", "ANI", "KRI"),
                             areas = c("48", "58", "88"),
                             vol   = NULL) {

  # --- Find and read processed CSV ---
  find_processed_csv <- function(vol) {
    if (!is.null(vol)) {
      p <- file.path("data", sprintf("ccamlr_combined_V%d.csv", vol))
      if (file.exists(p)) return(p)
      stop("No processed CSV found for Volume ", vol, " - run get_catch_data() first.")
    }
    files <- list.files("data", pattern = "^ccamlr_combined_V\\d+\\.csv$", full.names = TRUE)
    if (length(files) == 0) stop("No processed CCAMLR CSV found in data/ - run get_catch_data() first.")
    files[which.max(as.integer(regmatches(files, gregexpr("(?<=_V)\\d+", files, perl = TRUE))))]
  }

  csv_path <- find_processed_csv(vol)
  message("Reading processed data from: ", csv_path)
  ds <- read_csv(csv_path, show_col_types = FALSE,
                 col_types = cols(asd_code = col_character(), pot_count = col_double(), .default = col_guess()))

  # --- Area summary ---
  area_data <- ds %>%
    filter(
      as.integer(.data$season_ccamlr) >= 2012,
      .data$taxon_code %in% taxa,
      .data$area_code  %in% areas
    ) %>%
    group_by(
      .data$season_ccamlr,
      .data$taxon_vernacular_name,
      .data$taxon_scientific_name,
      .data$taxon_code,
      .data$area_code
    ) %>%
    summarise(
      catch_tonne = sum(.data$greenweight_caught_tonne, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      season_ccamlr = as.integer(.data$season_ccamlr),
      area_code     = as.character(.data$area_code),
      catch_tonne   = round(.data$catch_tonne, 0)
    )

  # --- Subarea summary ---
  subarea_data <- ds %>%
    filter(
      as.integer(.data$season_ccamlr) >= 2012,
      .data$taxon_code %in% taxa,
      !.data$asd_type  %in% "Area"
    ) %>%
    group_by(
      .data$season_ccamlr,
      .data$taxon_vernacular_name,
      .data$taxon_scientific_name,
      .data$taxon_code,
      .data$asd_subarea_code
    ) %>%
    summarise(
      catch_tonne = sum(.data$greenweight_caught_tonne, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      season_ccamlr = as.integer(.data$season_ccamlr),
      catch_tonne   = round(.data$catch_tonne, 2)
    ) %>%
    ungroup()

  message("CCAMLR area and subarea data available with .$area and .$subarea")
  return(list(area = area_data, subarea = subarea_data))
}


# =============================================================================
# 3. PLOTTING
# =============================================================================

#' Plot CCAMLR catch time series by area or subarea
#'
#' @description
#' Generates a faceted bar chart of annual catch (tonnes) for one or more
#' CCAMLR statistical areas or subareas, using data returned by
#' \code{\link{make_catch_table}}. The function auto-detects whether area-level
#' or subarea-level data was passed by inspecting column names.
#'
#' When multiple taxa are present in \code{plot_data}, a separate subplot is
#' produced for each species and the results are stacked vertically via
#' \code{patchwork}. Row-shared y-axis limits are applied within each species
#' independently so that scale differences between taxa do not compress trends.
#'
#' @param plot_data A data frame. Pass either \code{make_catch_table()$area} or
#'   \code{make_catch_table()$subarea}. Detection is automatic via the presence
#'   of the \code{asd_subarea_code} column.
#' @param style Character. Visual theme: \code{"light"} (clean teal fill,
#'   light background, print-ready) or \code{"dark"} (dark background, neon-teal
#'   fill, matches the dark map theme). Default \code{"light"}.
#' @param title Character, \code{NULL}, or \code{FALSE}. For a single taxon,
#'   controls the plot title: \code{NULL} (default) auto-generates from the
#'   taxon name, \code{FALSE} suppresses it, a string overrides it. For
#'   multiple taxa, each subplot is titled automatically with its species name;
#'   passing a string here adds an additional overall heading above all subplots.
#'
#' @return A \code{\link[patchwork]{patchwork}} / \code{\link[ggplot2]{ggplot}}
#'   object.
#' @export
#'
#' @examples
#' \dontrun{
#' d <- make_catch_table()
#' make_catch_plot(d$area)                          # all taxa, area-level
#' make_catch_plot(d$subarea)                       # all taxa, subarea-level
#' make_catch_plot(d$area, style = "dark")          # dark presentation theme
#' make_catch_plot(d$area %>% filter(taxon_code == "TOA"))  # single species
#' }
make_catch_plot <- function(plot_data, style = c("light", "dark"), title = NULL) {

  style <- match.arg(style)

  is_subarea   <- "asd_subarea_code" %in% names(plot_data)
  taxa_present <- unique(plot_data$taxon_code)
  multi_taxa   <- length(taxa_present) > 1

  # -- Palette tokens ----------------------------------------------------------
  if (style == "dark") {
    bg_colour   <- "#060c14"
    panel_bg    <- "#091828"
    bar_fill    <- "#06c0be"
    bar_colour  <- "#1c3048"
    text_colour <- "#6a9bbf"
    grid_colour <- "#0f2030"
    strip_bg    <- "#0f1e2d"
    title_col   <- "#a8c8e0"
  } else {
    bg_colour   <- "white"
    panel_bg    <- "white"
    bar_fill    <- "#1a7a8a"
    bar_colour  <- "#0e5060"
    text_colour <- "grey25"
    grid_colour <- "grey90"
    strip_bg    <- "grey96"
    title_col   <- "grey10"
  }

  base_theme <- theme_antarctic(base_size = 11) +
    theme(
      plot.background    = element_rect(fill = bg_colour,   colour = NA),
      panel.background   = element_rect(fill = panel_bg,    colour = NA),
      panel.grid.major   = element_line(colour = grid_colour, linewidth = 0.4),
      axis.text          = element_text(colour = text_colour),
      axis.title         = element_text(colour = text_colour),
      axis.text.x        = element_text(angle = 45, hjust = 1, colour = text_colour),
      axis.title.x       = element_text(margin = margin(t = 10), colour = text_colour),
      strip.text         = element_text(face = "bold", colour = text_colour),
      strip.background   = element_rect(fill = strip_bg, colour = NA),
      legend.position    = "bottom",
      legend.title.align = 0.5,
      plot.title         = element_text(face = "bold", size = 14, colour = title_col),
      plot.caption       = element_text(size = 9, hjust = 0, colour = text_colour)
    )

  # -- Build one subplot per taxon ---------------------------------------------
  plot_list <- lapply(taxa_present, function(tc) {

    td <- plot_data %>% filter(.data$taxon_code == tc)

    if (is_subarea) {

      row_limits <- tibble::tibble(
        asd_subarea_code = subarea_order,
        row_group        = rep(1:4, each = 3)
      )

      td <- td %>%
        filter(.data$catch_tonne > 0, .data$asd_subarea_code %in% subarea_order) %>%
        left_join(row_limits, by = "asd_subarea_code")

      row_maxes <- td %>%
        group_by(.data$row_group) %>%
        summarise(raw_max = max(.data$catch_tonne), .groups = "drop") %>%
        mutate(
          scale_unit = 10^(floor(log10(.data$raw_max)) - 1)
        ) %>%
        mutate(
          y_max = ceiling(.data$raw_max / .data$scale_unit) * .data$scale_unit
        ) %>%
        select(.data$row_group, .data$y_max)

      # Only show row groups that have at least one subarea with data.
      # This lets users pass a filtered subset without empty rows appearing.
      active_row_groups <- unique(td$row_group)
      active_subareas   <- row_limits %>%
        filter(.data$row_group %in% active_row_groups) %>%
        pull(.data$asd_subarea_code)

      subareas_with_data <- unique(td$asd_subarea_code)

      td <- td %>%
        left_join(row_maxes, by = "row_group") %>%
        mutate(
          asd_subarea_code = factor(.data$asd_subarea_code, levels = active_subareas),
          season_ccamlr    = factor(.data$season_ccamlr, levels = as.character(2012:2024)),
          log_catch        = log10(.data$catch_tonne)
        )

      # Placeholder rows for gaps within active row groups only. Keeps subareas
      # from different areas off the same row without showing empty rows for
      # area groups the user didn't pass.
      anchor_rows <- row_limits %>%
        filter(
          .data$row_group %in% active_row_groups,
          !.data$asd_subarea_code %in% subareas_with_data
        ) %>%
        left_join(row_maxes, by = "row_group") %>%
        mutate(
          asd_subarea_code = factor(.data$asd_subarea_code, levels = active_subareas),
          season_ccamlr    = factor("2012", levels = as.character(2012:2024)),
          catch_tonne      = NA_real_,
          log_catch        = NA_real_
        )

      td <- bind_rows(td, anchor_rows)

    } else {

      td <- td %>% filter(.data$catch_tonne > 0)

      tc_limits  <- area_ylimits[[tc]]
      area_maxes <- tibble::tibble(
        area_code = names(tc_limits),
        y_max     = unname(tc_limits)
      )

      td <- td %>%
        left_join(area_maxes, by = "area_code") %>%
        mutate(
          area_code     = factor(.data$area_code, levels = area_order),
          season_ccamlr = factor(.data$season_ccamlr, levels = as.character(2012:2024)),
          log_catch     = log10(.data$catch_tonne)
        )
    }

    p <- ggplot(td, aes(x = .data$season_ccamlr, y = .data$catch_tonne)) +
      geom_col(fill = bar_fill, colour = bar_colour, linewidth = 0.15) +
      geom_blank(aes(y = .data$y_max)) +
      scale_x_discrete(breaks = as.character(seq(2012, 2024, by = 2))) +
      scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
      labs(x = "CCAMLR Season", y = "Catch (tonnes)") +
      base_theme

    if (is_subarea) {
      p <- p + facet_wrap(
        ~ asd_subarea_code,
        ncol     = 3,
        scales   = "free_y",
        drop     = FALSE,
        labeller = labeller(asd_subarea_code = subarea_labels)
      )
    } else {
      p <- p + facet_wrap(
        ~ area_code,
        ncol     = 3,
        scales   = "free_y",
        labeller = labeller(area_code = setNames(
          paste("Area", area_order),
          as.character(area_order)
        ))
      )
    }

    # In multi-taxa output each subplot is titled with its species name.
    # Single-taxon titles are handled via plot_annotation below.
    if (multi_taxa) {
      species_label <- paste0(
        unique(na.omit(td$taxon_vernacular_name)), " (",
        unique(na.omit(td$taxon_scientific_name)), ")"
      )
      p <- p + ggtitle(species_label) +
        theme(plot.title = element_text(size = 12))
    }

    p
  })

  # -- Combine -----------------------------------------------------------------
  p_combined <- if (length(plot_list) == 1) {
    plot_list[[1]]
  } else {
    patchwork::wrap_plots(plot_list, ncol = 1)
  }

  # -- Annotation --------------------------------------------------------------
  if (multi_taxa) {
    overall_title <- if (isFALSE(title) || is.null(title)) NULL else title
  } else {
    auto_title <- paste0(
      unique(plot_data$taxon_vernacular_name), " (",
      unique(plot_data$taxon_scientific_name), ")"
    )
    overall_title <- if (isFALSE(title)) NULL else if (is.null(title)) auto_title else title
  }

  message("Datasource: CCAMLR Statistical Bulletin - ccamlr.org/en/publications/statistical-bulletin")

  p_combined +
    plot_annotation(title = overall_title)
}


# =============================================================================
# 4. MAPPING
# =============================================================================

#' Map CCAMLR fishing catch intensity over Antarctic statistical subareas
#'
#' @description
#' Produces choropleth maps of Antarctic fishing catch over CCAMLR statistical
#' subareas (ASDs) for a single season. One map is generated per taxon. Maps
#' are printed to the Plots pane and, if \code{output_dir} is supplied, saved
#' as PNG files.
#'
#' Two visual styles cater for different output contexts: a high-impact dark
#' theme suited to presentations, and a clean light theme for journal figures
#' and committee papers.
#'
#' The light style supports three perceptually uniform, colorblind-safe
#' palettes: \code{"viridis"} (dark purple to yellow; widely cited in the
#' literature), \code{"oslo"} (pale silver to dark navy; cool oceanic feel), and
#' \code{"lapaz"} (dark navy to cream; elegant blue-lavender sequence).
#'
#' @param data A data frame. Must be the \code{$subarea} element returned by
#'   \code{\link{make_catch_table}} - passing \code{$area} will error. All taxa
#'   present in \code{data} are mapped; filter before calling to restrict taxa.
#'   Required columns: \code{asd_subarea_code}, \code{catch_tonne},
#'   \code{taxon_code}, \code{season_ccamlr}.
#' @param season Integer. CCAMLR season to display. \code{NULL} (default) uses
#'   the most recent season found in \code{data} and prints a message.
#' @param title Logical or \code{NULL}. If \code{TRUE} (default), an automatic
#'   title is added with the species name and season.
#' @param style Character. Visual theme: \code{"dark"} (dark ocean,
#'   high-impact) or \code{"light"} (clean white, publication-grade).
#'   Default \code{"dark"}.
#' @param palette Character. Colour palette for \code{style = "light"}.
#'   One of \code{"viridis"} (default), \code{"oslo"}, or \code{"lapaz"}.
#'   Ignored when \code{style = "dark"}.
#' @param output_dir Character. If supplied, each map is saved as a PNG in this
#'   directory with filename \code{map_<taxon>_<season>.png}. Directory is
#'   created if it does not exist.
#' @param width,height Numeric. PNG dimensions in inches. Defaults: 8 x 7.
#' @param dpi Numeric. PNG resolution. Default 150.
#'
#' @return A named list of \code{\link[ggplot2]{ggplot}} objects, one per
#'   taxon. Returned invisibly; maps are printed as a side-effect.
#' @export
#'
#' @examples
#' \dontrun{
#' d <- make_catch_table()
#' make_catch_map(d$subarea)                                        # all taxa, dark, latest season
#' make_catch_map(d$subarea, season = 2024)                        # specific season
#' make_catch_map(d$subarea, style = "light")                      # light / viridis
#' make_catch_map(d$subarea, style = "light", palette = "oslo")    # light / oslo
#' make_catch_map(d$subarea, output_dir = "docs")                  # save PNGs to docs/
#' }
make_catch_map <- function(data,
                           season     = NULL,
                           title      = TRUE,
                           style      = c("dark", "light"),
                           palette    = c("viridis", "oslo", "lapaz"),
                           output_dir = NULL,
                           width      = 8,
                           height     = 7,
                           dpi        = 150) {

  style   <- match.arg(style)
  palette <- match.arg(palette)

  if (!is.data.frame(data)) {
    stop("data must be a data frame - pass catch_data$subarea")
  }

  if (!"asd_subarea_code" %in% names(data)) {
    stop("make_catch_map requires subarea-level data - pass catch_data$subarea, not catch_data$area")
  }

  if (is.null(season)) {
    season <- max(data$season_ccamlr, na.rm = TRUE)
    message("make_catch_map: using season ", season)
  }

  taxa <- sort(unique(data$taxon_code))

  data_season <- data %>% filter(.data$season_ccamlr == season)

  # -- Load spatial base layers once (shared across all taxa) -----------------
  ASDs_subareas <- load_ASDs() %>%
    st_make_valid() %>%
    mutate(
      feature_type = case_when(
        str_detect(.data$GAR_Name, "^Subarea\\s+")  ~ "Subarea",
        str_detect(.data$GAR_Name, "^Division\\s+") ~ "Division",
        TRUE ~ "Other"
      ),
      subarea_code = case_when(
        .data$feature_type %in% c("Subarea", "Division") ~ str_extract(.data$GAR_Name, "\\d+\\.\\d+"),
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(.data$subarea_code)) %>%
    group_by(.data$subarea_code) %>%
    summarise(
      GAR_Name        = paste0("Subarea ", first(.data$subarea_code)),
      source_features = n(),
      .groups         = "drop"
    )

  coast <- if (style == "dark") {
    tryCatch(CCAMLRGIS::load_Coastline(), error = function(e) NULL)
  } else {
    NULL
  }

  message("Datasource: CCAMLR Statistical Bulletin - ccamlr.org/en/publications/statistical-bulletin")

  # -- One map per taxon --------------------------------------------------------
  map_list <- lapply(taxa, function(tc) {

    tc_limits <- map_limits[[tc]]
    if (is.null(tc_limits)) {
      stop("No map_limits entry for taxon '", tc, "' - add it to ccamlr_metadata.R")
    }

    map_ds <- data_season %>%
      filter(.data$catch_tonne > 0, .data$taxon_code == tc) %>%
      mutate(subarea_code = as.character(.data$asd_subarea_code / 10))

    if (nrow(map_ds) == 0) {
      message("make_catch_map: no catch for ", tc, " in season ", season, " - skipping")
      return(NULL)
    }

    ASDs_plot <- ASDs_subareas %>%
      left_join(map_ds, by = "subarea_code") %>%
      mutate(log_catch = log10(.data$catch_tonne))

    # Extract label positions as a plain data frame so geom_shadowtext can be
    # used - geom_sf_text does not forward bg.colour in ggplot2 >= 4.0.
    pts      <- suppressWarnings(st_point_on_surface(ASDs_plot))
    coords   <- st_coordinates(pts)
    label_df <- pts %>%
      mutate(x = coords[, 1], y = coords[, 2]) %>%
      st_drop_geometry()

    p <- if (style == "dark") {
      .make_catch_map_dark(ASDs_plot, label_df, tc_limits$limits, tc_limits$breaks, coast)
    } else {
      .make_catch_map_light(ASDs_plot, label_df, tc_limits$limits, tc_limits$breaks, palette)
    }

    species_label <- if (isTRUE(title)) paste0(
      unique(na.omit(map_ds$taxon_vernacular_name)), " (",
      unique(na.omit(map_ds$taxon_scientific_name)), ") - Season ", season
    ) else NULL
    title_colour <- if (style == "dark") "#a8c8e0" else "grey10"
    p <- p +
      ggplot2::labs(title = species_label) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(colour = title_colour, face = "bold", size = 14,
                                           margin = ggplot2::margin(b = 6))
      )

    print(p)

    if (!is.null(output_dir)) {
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      fname <- file.path(output_dir, paste0("map_", tolower(tc), "_", season, ".png"))
      ggsave(fname, plot = p, width = width, height = height, dpi = dpi)
      message("Saved: ", fname)
    }

    p
  })

  names(map_list) <- taxa
  invisible(map_list)
}


# -- Design tokens: dark theme -----------------------------------------------
.DARK_BG     <- "#060c14"
.DARK_OCEAN  <- "#091828"
.DARK_LAND   <- "#0f1e2d"
.DARK_BORDER <- "#1c3048"
.DARK_COAST  <- "#243d55"
.DARK_TEXT   <- "#6a9bbf"

.dark_pal <- colorRampPalette(c(
  "#0d1f35",
  "#0f4262",
  "#0d7a8a",
  "#06c0be",
  "#f0a030",
  "#faecc0"
))(512)


.make_catch_map_dark <- function(ASDs_plot, label_df, limits, breaks, coast = NULL) {

  p <- ggplot(ASDs_plot) +
    {if (!is.null(coast))
      geom_sf(
        data      = coast,
        fill      = .DARK_LAND,
        colour    = .DARK_COAST,
        linewidth = 0.4
      )
    } +
    geom_sf(
      aes(fill = .data$catch_tonne),
      colour    = .DARK_BORDER,
      linewidth = 0.35
    ) +
    shadowtext::geom_shadowtext(
      data      = label_df,
      aes(x = .data$x, y = .data$y, label = .data$subarea_code),
      size      = 3.5,
      colour    = "white",
      bg.colour = .DARK_BG,
      bg.r      = 0.15,
      fontface  = "bold"
    ) +
    scale_fill_gradientn(
      colours  = .dark_pal,
      name     = "Catch (t)",
      limits   = limits,
      breaks   = breaks,
      labels   = scales::comma,
      na.value = .DARK_OCEAN,
      guide    = guide_colorbar(
        barwidth       = unit(10, "cm"),
        barheight      = unit(0.45, "cm"),
        title.position = "top",
        title.hjust    = 0.5,
        ticks.colour   = .DARK_BORDER,
        frame.colour   = "#2a4a6a"
      )
    ) +
    coord_sf(datum = NA) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = .DARK_BG,    colour = NA),
      panel.background = element_rect(fill = .DARK_OCEAN, colour = NA),
      legend.position  = "bottom",
      legend.title     = element_text(colour = .DARK_TEXT, size = 9,
                                      hjust = 0.5),
      legend.text      = element_text(colour = .DARK_TEXT, size = 8,
                                      family = "mono"),
      plot.margin      = margin(12, 12, 12, 12)
    )

  p
}


.make_catch_map_light <- function(ASDs_plot, label_df, limits, breaks, palette) {

  cb_guide <- guide_colorbar(
    barwidth       = unit(10, "cm"),
    barheight      = unit(0.4, "cm"),
    title.position = "top",
    title.hjust    = 0.5
  )

  fill_scale <- switch(palette,
    viridis = scale_fill_viridis_c(
      name     = "Catch (t)",
      limits   = limits,
      breaks   = breaks,
      labels   = scales::comma,
      na.value = "grey92",
      guide    = cb_guide
    ),
    oslo = scico::scale_fill_scico(
      palette   = "oslo",
      direction = -1,
      begin     = 0.05,
      end       = 0.95,
      name      = "Catch (t)",
      limits    = limits,
      breaks    = breaks,
      labels    = scales::comma,
      na.value  = "grey92",
      guide     = cb_guide
    ),
    lapaz = scico::scale_fill_scico(
      palette  = "lapaz",
      begin    = 0.05,
      end      = 0.95,
      name     = "Catch (t)",
      limits   = limits,
      breaks   = breaks,
      labels   = scales::comma,
      na.value = "grey92",
      guide    = cb_guide
    )
  )

  ggplot(ASDs_plot) +
    geom_sf(
      aes(fill = .data$catch_tonne),
      colour    = "grey35",
      linewidth = 0.3
    ) +
    shadowtext::geom_shadowtext(
      data      = label_df,
      aes(x = .data$x, y = .data$y, label = .data$subarea_code),
      size      = 3.5,
      colour    = "white",
      bg.colour = "black",
      bg.r      = 0.15
    ) +
    fill_scale +
    coord_sf(datum = NA) +
    theme_void() +
    theme(
      legend.position    = "bottom",
      legend.title.align = 0.5
    )
}

