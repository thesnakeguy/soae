###############################################################################
## PLANKTON: SOUTHERN OCEAN CONTINUOUS PLANKTON RECORDER (SO-CPR) ####
###############################################################################

# =============================================================================
# 0. CONSTANTS (internal, not exported)
# =============================================================================

# Metadata columns present in the raw SO-CPR download. Everything else in the
# raw data is a taxon abundance column.
so_cpr_metadata_cols <- c(
  "Tow_Number",
  "Ship_Code",
  "Time",
  "Date",
  "Month",
  "Year",
  "Season",
  "Latitude",
  "Longitude",
  "Segment_No.",
  "Segment_Length",
  "Total.abundance",
  "Phytoplankton_Colour_Index",
  "Fluorescence",
  "Salinity",
  "Water_Temperature",
  "Photosynthetically_Active_Radiation"
)

# Life-stage tokens recognised when parsing SO-CPR taxon ID strings.
so_cpr_life_stages <- c(
  "egg",
  "nauplius",
  "metanauplius",
  "zoea",
  "megalopa",
  "phyllosoma",
  "juv",
  "natant",
  "larvae",
  "small",
  "calyptopis",
  "furcilia",
  "cyprid",
  "nectophore"
)


# =============================================================================
# 1. DATA FETCHING
# =============================================================================

#' Download the SO-CPR plankton dataset
#'
#' Downloads the Southern Ocean Continuous Plankton Recorder (SO-CPR) dataset
#' via the \pkg{blueant} package and splits the result into metadata and
#' taxon abundance columns.
#'
#' @param local_file_root Character. Directory used by \pkg{blueant} to store
#'   the raw downloaded files. Defaults to \code{"SO-CPR_RawData_BlueAnt/"}.
#' @param save_to Character or \code{NULL}. Path to save the combined raw
#'   data as a tab-separated text file. Set to \code{NULL} to skip saving.
#'
#' @return A list with three elements:
#' \describe{
#'   \item{data}{The full raw SO-CPR data frame (metadata + taxon columns)}
#'   \item{metadata_cols}{Character vector of metadata column names}
#'   \item{species_cols}{Character vector of taxon ID column names}
#' }
#'
#' @examples
#' \dontrun{
#' cpr <- download_so_cpr_data()
#' head(cpr$data)
#' }
#'
#' @export
download_so_cpr_data <- function(local_file_root = "SO-CPR_RawData_BlueAnt/",
                                  save_to = "SO-CPR_raw_download.txt") {

  if (!dir.exists(local_file_root)) dir.create(local_file_root, recursive = TRUE)

  message("  Configuring blueant and syncing the SO-CPR source...")
  cpr_config  <- bowerbird::bb_config(local_file_root = local_file_root)
  so_source   <- blueant::sources("Southern Ocean Continuous Plankton Recorder")
  cpr_config  <- bowerbird::bb_add(cpr_config, so_source)
  status      <- bowerbird::bb_sync(cpr_config)

  myfiles  <- status$files[[1]]
  cpr_data <- utils::read.csv(myfiles$file[grepl("AADC", myfiles$file)])

  species_cols <- setdiff(colnames(cpr_data), so_cpr_metadata_cols)

  if (!is.null(save_to)) {
    utils::write.table(cpr_data, file = save_to, sep = "\t", quote = FALSE)
    message(glue::glue("  Saved raw data to {save_to}"))
  }

  list(
    data          = cpr_data,
    metadata_cols = so_cpr_metadata_cols,
    species_cols  = species_cols
  )
}


# =============================================================================
# 2. TAXONOMIC ANNOTATION
# =============================================================================

# ── Parse a single SO-CPR taxon ID string into Genus/Species/HigherTaxon/
#    LifeStage/Qualifier components (internal, not exported) ────────────────
parse_so_cpr_taxon_id <- function(id) {
  parts <- stringr::str_split(id, "\\.")[[1]]

  # Egg
  if (stringr::str_detect(id, "^Egg\\.?")) {
    return(list(ID = id, LifeStage = "egg"))
  }

  # Life stage codes (eg: Euphausia.superba.F3)
  if (stringr::str_detect(id, "\\.[A-Za-z]+\\.[CF][0-9]$")) {
    parts <- stringr::str_split(id, "\\.", n = 3)[[1]]
    return(list(ID = id, Genus = parts[1], Species = parts[2], LifeStage = parts[3]))
  }

  # Species with life stage (eg: Euphausia.triacantha.calyptopis)
  if (stringr::str_detect(id, "^[A-Z][a-z]+\\.[a-z]+\\.[a-z]+$") &
      parts[3] %in% so_cpr_life_stages) {
    return(list(ID = id, Genus = parts[1], Species = parts[2], LifeStage = parts[3]))
  }

  # "Genus..Subgenus..species" -> keep Genus and Species
  if (stringr::str_detect(id, "^[A-Z][a-z]+\\.\\.[A-Z][a-z]+\\.\\.[a-z]+$")) {
    parts <- stringr::str_split(id, "\\.\\.")[[1]]
    return(list(ID = id, Genus = parts[1], Species = parts[3]))
  }

  # Genus level (Genus.sp)
  if (stringr::str_detect(id, "\\.sp\\.$")) {
    genus <- stringr::str_remove(id, "\\.sp\\.?$")
    return(list(ID = id, Genus = genus, Qualifier = "sp"))
  }

  # Genus with life stage (eg: Thysanoessa.sp..furcilia)
  if (stringr::str_detect(id, "\\.sp\\.\\.[a-z]+$") &
      (tolower(parts[length(parts)]) %in% so_cpr_life_stages)) {
    parts <- stringr::str_split(id, "\\.sp\\.\\.", n = 2)[[1]]
    return(list(ID = id, Genus = parts[1], LifeStage = parts[2], Qualifier = "sp"))
  }

  # Higher taxon with life stage (eg: bryozoa.larvae, Decapoda.megalopa)
  if (tolower(parts[length(parts)]) %in% so_cpr_life_stages & length(parts == 2)) {
    return(list(ID = id, HigherTaxon = parts[1], LifeStage = parts[2]))
  }

  # Higher taxon with life stage + "indet" (eg: Calanoida.indet..small.)
  if (stringr::str_detect(id, "\\.indet\\.\\.[a-z]")) {
    parts <- stringr::str_split(id, "\\.indet\\.\\.", n = 2)[[1]]
    return(list(ID = id, HigherTaxon = parts[1], LifeStage = parts[2], Qualifier = "indet"))
  }
  if (stringr::str_detect(id, "[A-Z][a-z]+\\.[a-z]+\\.indet$") &
      parts[2] %in% so_cpr_life_stages) {
    return(list(ID = id, HigherTaxon = parts[1], LifeStage = parts[2], Qualifier = "indet"))
  }

  # "Taxon.indet" -> HigherTaxon, Qualifier
  if (stringr::str_detect(id, "\\.indet$")) {
    taxon <- stringr::str_remove(id, "\\.indet$")
    return(list(ID = id, HigherTaxon = taxon, Qualifier = "indet"))
  }

  # Subspecies (eg: Clione.limacina.antarctica)
  if (stringr::str_detect(id, "^[A-Z][a-z]+\\.[a-z]+\\.[a-z]+$") &
      !(parts[3] %in% so_cpr_life_stages)) {
    return(list(ID = id, Genus = parts[1], Species = parts[2]))
  }

  # Species with "var" (eg: Euphausia.similis.var.armata)
  if (length(grep("var", id)) > 0) {
    return(list(ID = id, Genus = parts[1], Species = parts[2]))
  }

  # Default genus-species case
  if (stringr::str_detect(id, "^[A-Z][a-z]+\\.[a-z]+$") &
      !(tolower(parts[length(parts)]) %in% so_cpr_life_stages)) {
    parts <- stringr::str_split(id, "\\.")[[1]]
    return(list(ID = id, Genus = parts[1], Species = parts[2]))
  }

  # Fallback: keep the raw ID
  list(ID = id, RawID = id)
}

# ── Look up hierarchical WoRMS taxonomy for one parsed taxon (internal) ─────
process_so_cpr_taxon <- function(taxon) {
  columns <- c(
    "ID", "genus", "species", "family", "suborder", "order", "subclass",
    "class", "subphylum", "phylum", "infrakingdom", "subkingdom", "kingdom",
    "LifeStage", "Qualifier"
  )
  taxon_hierarchy <- stats::setNames(rep(NA, length(columns)), columns)

  aphia_id <- NA
  tryCatch({
    if (!is.null(taxon$Genus) && !is.null(taxon$Species)) {
      aphia_id <- worrms::wm_name2id(paste(taxon$Genus, taxon$Species))
    } else if (!is.null(taxon$Genus)) {
      aphia_id <- worrms::wm_name2id(taxon$Genus)
    } else if (!is.null(taxon$HigherTaxon)) {
      aphia_id <- worrms::wm_name2id(taxon$HigherTaxon)
    }
  }, error = function(e) {
    message(glue::glue(
      "  WoRMS lookup failed for {taxon$Genus} {taxon$Species}: {conditionMessage(e)}"
    ))
    aphia_id <<- NA
  })

  if (is.na(aphia_id)) {
    taxon_hierarchy["ID"] <- taxon$ID
    return(taxon_hierarchy)
  }

  if (!is.null(aphia_id) && aphia_id != "") {
    classification <- worrms::wm_classification(aphia_id)
    for (i in seq_along(classification[[2]])) {
      rank <- tolower(classification[[2]][i])
      name <- classification[[3]][i]
      if (rank %in% names(taxon_hierarchy)) taxon_hierarchy[[rank]] <- name
    }
  }

  taxon_hierarchy$ID <- taxon$ID
  if (!is.null(taxon$LifeStage)) taxon_hierarchy$LifeStage <- taxon$LifeStage
  if (!is.null(taxon$Qualifier)) taxon_hierarchy$Qualifier <- taxon$Qualifier

  taxon_hierarchy
}

# ── Manually curated overrides for IDs that WoRMS auto-matching gets wrong
#    (internal, not exported) ────────────────────────────────────────────────
so_cpr_taxon_overrides <- function() {
  data.frame(
    ID = c(
      "Appendicularia.indet", "Clione.sp.", "Ctenophora.indet",
      "Hyperia.sp.", "Spongiobranchaea.australis", "Themisto.sp."
    ),
    genus = c("Appendicularia", "Clione", NA, "Hyperia", "Spongiobranchaea", "Themisto"),
    species = c(NA, NA, NA, NA, "australis", NA),
    family = c(NA, "Clionidae", NA, "Hyperiidae", "Pneumodermatidae", "Hyperiidae"),
    suborder = rep(NA, 6),
    order = c(NA, "Pteropoda", NA, "Amphipoda", "Pteropoda", "Amphipoda"),
    subclass = rep(NA, 6),
    class = c("Appendicularia", "Gastropoda", NA, "Malacostraca", "Gastropoda", "Malacostraca"),
    subphylum = rep(NA, 6),
    phylum = c("Chordata", "Mollusca", "Ctenophora", "Arthropoda", "Mollusca", "Arthropoda"),
    infrakingdom = rep(NA, 6),
    subkingdom = rep(NA, 6),
    kingdom = rep("Animalia", 6),
    LifeStage = rep(NA, 6),
    Qualifier = c("indet", "sp", "indet", "sp", NA, "sp"),
    stringsAsFactors = FALSE
  )
}

#' Annotate SO-CPR taxon IDs with WoRMS taxonomy
#'
#' Parses the cryptic taxon ID column names used in the raw SO-CPR data
#' (e.g. \code{"Euphausia.superba.F3"}) into genus/species/life-stage
#' components, then queries the World Register of Marine Species (WoRMS) via
#' \pkg{worrms} to retrieve the full taxonomic hierarchy for each one.
#'
#' @param species_cols Character vector of taxon ID column names, typically
#'   \code{download_so_cpr_data()$species_cols}.
#' @param include_overrides Logical. If \code{TRUE} (default), append a small
#'   set of manually curated rows for IDs that WoRMS name-matching commonly
#'   gets wrong.
#' @param save_to Character or \code{NULL}. Path to save the resulting lookup
#'   table as a tab-separated text file. Set to \code{NULL} to skip saving.
#'
#' @return A data frame with one row per taxon ID and columns for the
#'   taxonomic hierarchy (genus, species, family, order, ..., kingdom) plus
#'   \code{LifeStage} and \code{Qualifier}.
#'
#' @details This function queries the WoRMS API once per unique taxon and can
#'   take several minutes for the full SO-CPR taxon list. Consider caching
#'   the result with \code{save_to}.
#'
#' @examples
#' \dontrun{
#' cpr <- download_so_cpr_data()
#' taxon_df <- annotate_so_cpr_taxa(cpr$species_cols)
#' }
#'
#' @export
annotate_so_cpr_taxa <- function(species_cols,
                                  include_overrides = TRUE,
                                  save_to = "SO-CPR_ID_TaxonAnnotations.txt") {

  message(glue::glue("  Parsing {length(species_cols)} taxon IDs..."))
  parsed_taxa <- purrr::map(species_cols, parse_so_cpr_taxon_id)

  message("  Querying WoRMS for taxonomic hierarchy (this can take a while)...")
  taxon_df <- dplyr::bind_rows(lapply(parsed_taxa, process_so_cpr_taxon))
  taxon_df <- as.data.frame(taxon_df)

  if (include_overrides) {
    overrides <- so_cpr_taxon_overrides()
    names(overrides) <- names(taxon_df)
    taxon_df <- rbind(taxon_df, overrides)
  }

  taxon_df <- taxon_df[rowSums(!is.na(taxon_df)) > 0, ]

  if (!is.null(save_to)) {
    utils::write.table(taxon_df, save_to, quote = FALSE, sep = "\t")
    message(glue::glue("  Saved taxonomy annotations to {save_to}"))
  }

  taxon_df
}


# =============================================================================
# 3. INDEX CALCULATIONS
# =============================================================================

#' Calculate the segment-corrected total plankton abundance index
#'
#' @param cpr_data Data frame. Raw SO-CPR data, e.g.
#'   \code{download_so_cpr_data()$data}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{data}{\code{cpr_data} with an added \code{Total_Plankton_Segment_corr} column}
#'   \item{effort}{A data frame of sample counts per year}
#' }
#'
#' @seealso [plot_so_cpr_abundance_index()]
#'
#' @examples
#' \dontrun{
#' cpr <- download_so_cpr_data()
#' abund <- calc_so_cpr_abundance_index(cpr$data)
#' }
#'
#' @export
calc_so_cpr_abundance_index <- function(cpr_data) {
  data <- cpr_data |>
    dplyr::mutate(
      Total_Plankton_Segment_corr = .data$Total.abundance / .data$Segment_Length,
      Water_Temperature = as.numeric(gsub("-", NA, .data$Water_Temperature))
    )

  effort <- data |> dplyr::count(.data$Year, name = "n_segments")

  list(data = data, effort = effort)
}

#' Plot the total plankton abundance index over time
#'
#' Produces a two-panel figure: segment-corrected plankton abundance per
#' tow-segment (coloured by water temperature, with a smoothed trend), and
#' sampling effort (number of segments) per year.
#'
#' @param abundance_list List. Output of [calc_so_cpr_abundance_index()].
#'
#' @return A [cowplot::plot_grid()] object combining both panels.
#'
#' @examples
#' \dontrun{
#' cpr <- download_so_cpr_data()
#' abund <- calc_so_cpr_abundance_index(cpr$data)
#' print(plot_so_cpr_abundance_index(abund))
#' }
#'
#' @export
plot_so_cpr_abundance_index <- function(abundance_list) {
  data   <- abundance_list$data
  effort <- abundance_list$effort
  year_breaks <- sort(unique(data$Year))

  p_abund <- ggplot2::ggplot(data, ggplot2::aes(x = .data$Year, y = .data$Total_Plankton_Segment_corr)) +
    ggplot2::geom_jitter(
      ggplot2::aes(colour = .data$Water_Temperature),
      width = 0.2, alpha = 0.5, size = 1
    ) +
    ggplot2::geom_smooth(
      method = "gam", formula = y ~ s(x, k = 10), se = FALSE, colour = "black"
    ) +
    ggplot2::labs(y = "Total plankton (segment corrected)", colour = "Water temperature (\u00b0C)") +
    ggplot2::scale_colour_viridis_c(option = "plasma", na.value = "grey70") +
    ggplot2::scale_x_continuous(breaks = year_breaks) +
    theme_antarctic() +
    ggplot2::labs(x = NULL) +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      legend.position = "top"
    )

  p_effort <- ggplot2::ggplot(effort, ggplot2::aes(x = .data$Year, y = .data$n_segments)) +
    ggplot2::geom_col(fill = "grey70") +
    ggplot2::labs(y = "Number of segments", x = "Year") +
    ggplot2::scale_x_continuous(breaks = year_breaks) +
    theme_antarctic() +
    ggplot2::theme(
      axis.text.x  = ggplot2::element_text(angle = -45, hjust = 0.2),
      axis.title.x = ggplot2::element_text(vjust = -0.2)
    )

  cowplot::plot_grid(p_abund, p_effort, ncol = 1, align = "v", rel_heights = c(3, 1))
}

#' Calculate the Salp/Euphausiid abundance balance index
#'
#' Compares the relative abundance of Salpidae versus Euphausiidae per
#' segment and per year, a common proxy for shifts between gelatinous- and
#' krill-dominated Southern Ocean plankton communities.
#'
#' @param cpr_data Data frame. Raw SO-CPR data.
#' @param taxon_df Data frame. Taxonomy lookup table from
#'   [annotate_so_cpr_taxa()].
#'
#' @return A list with elements:
#' \describe{
#'   \item{data}{Segment-level data with \code{Salp.abundance} and \code{Euph.abundance}}
#'   \item{annual_balance}{Annual mean \code{Salp.abundance - Euph.abundance}}
#' }
#'
#' @seealso [plot_so_cpr_salp_euph_index()]
#'
#' @export
calc_so_cpr_salp_euph_index <- function(cpr_data, taxon_df) {
  salp_ids <- taxon_df |> dplyr::filter(.data$family == "Salpidae") |> dplyr::pull(.data$ID)
  euph_ids <- taxon_df |> dplyr::filter(.data$family == "Euphausiidae") |> dplyr::pull(.data$ID)

  data <- cpr_data |>
    dplyr::select(dplyr::all_of(c(so_cpr_metadata_cols, euph_ids, salp_ids))) |>
    dplyr::mutate(
      Salp.abundance     = rowSums(dplyr::across(dplyr::all_of(salp_ids)), na.rm = TRUE),
      Euph.abundance     = rowSums(dplyr::across(dplyr::all_of(euph_ids)), na.rm = TRUE),
      Water_Temperature  = as.numeric(.data$Water_Temperature)
    )

  annual_balance <- data |>
    dplyr::group_by(.data$Year) |>
    dplyr::summarise(
      SalpEuphIndex = mean(.data$Salp.abundance, na.rm = TRUE) - mean(.data$Euph.abundance, na.rm = TRUE),
      .groups = "drop"
    )

  list(data = data, annual_balance = annual_balance)
}

#' Plot the Salp/Euphausiid abundance balance over time
#'
#' @param salp_euph_list List. Output of [calc_so_cpr_salp_euph_index()].
#' @param ylim Numeric vector of length 2. y-axis limits for the
#'   segment-level balance. Defaults to \code{c(-50, 50)}.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @export
plot_so_cpr_salp_euph_index <- function(salp_euph_list, ylim = c(-50, 50)) {
  data <- salp_euph_list$data
  annual_balance <- salp_euph_list$annual_balance
  year_breaks <- sort(unique(data$Year))

  ggplot2::ggplot(data, ggplot2::aes(x = .data$Year, y = .data$Salp.abundance - .data$Euph.abundance)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_jitter(
      ggplot2::aes(colour = .data$Salp.abundance - .data$Euph.abundance > 0),
      width = 0.2, alpha = 0.5, size = 1
    ) +
    ggplot2::geom_point(
      data = annual_balance,
      ggplot2::aes(x = .data$Year, y = .data$SalpEuphIndex, colour = "Annual balance"),
      size = 2
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "FALSE" = "firebrick", "TRUE" = "steelblue", "Annual balance" = "black"
      ),
      labels = c(
        "FALSE" = "Euphausiids dominate", "TRUE" = "Salps dominate",
        "Annual balance" = "Annual balance"
      ),
      name = "Dominance"
    ) +
    ggplot2::labs(y = "Salp \u2013 Euphausiid abundance", x = "Year") +
    ggplot2::scale_x_continuous(breaks = year_breaks) +
    theme_antarctic() +
    ggplot2::theme(
      legend.position = "top",
      axis.text.x  = ggplot2::element_text(angle = -45, hjust = 0.2),
      axis.title.x = ggplot2::element_text(vjust = -0.2),
      legend.text  = ggplot2::element_text(size = 10)
    ) +
    ggplot2::coord_cartesian(ylim = ylim)
}


# =============================================================================
# 4. COMMUNITY NETWORK ANALYSIS
# =============================================================================

#' Build a unique segment identifier for SO-CPR samples
#'
#' @param cpr_data Data frame. Raw SO-CPR data (must contain
#'   \code{Ship_Code}, \code{Tow_Number}, \code{Date}, \code{Time}).
#'
#' @return \code{cpr_data} with an added character column \code{Segment}.
#'
#' @export
so_cpr_segment_id <- function(cpr_data) {
  cpr_data |>
    dplyr::mutate(Segment = paste(.data$Ship_Code, .data$Tow_Number, .data$Date, .data$Time, sep = "_"))
}

#' Build a plankton-community similarity network for a given year
#'
#' Computes Bray-Curtis similarity between SO-CPR segments sampled in
#' \code{target_year}, builds a weighted network from the similarity matrix,
#' and extracts a statistically significant backbone using disparity
#' filtering (\pkg{backbone}).
#'
#' @param cpr_data Data frame. Raw SO-CPR data.
#' @param target_year Character or numeric. Year to analyse (matched against
#'   the \code{Year} column).
#' @param min_occurrence Numeric in \code{[0, 1]}. Minimum proportion of
#'   segments a taxon must occur in to be retained (data is sparse, so rare
#'   taxa are dropped). Defaults to \code{0.025}.
#' @param backbone_alpha Numeric. Significance threshold for disparity-filter
#'   backbone extraction. Defaults to \code{0.05}.
#' @param min_degree Integer. Minimum node degree retained when pruning the
#'   backbone network to its largest connected component. Defaults to
#'   \code{5}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{net}{The full \pkg{igraph} similarity network}
#'   \item{backbone_net}{The disparity-filtered backbone network}
#'   \item{backbone_net_pruned}{\code{backbone_net} restricted to its largest
#'     connected component and nodes with degree \code{>= min_degree}}
#'   \item{segments}{The segment x taxon data frame used to build the network}
#' }
#'
#' @seealso [detect_so_cpr_communities()], [plot_so_cpr_network()]
#'
#' @export
build_so_cpr_network <- function(cpr_data,
                                  target_year,
                                  min_occurrence = 0.025,
                                  backbone_alpha = 0.05,
                                  min_degree = 5) {

  cpr_year <- cpr_data |> dplyr::filter(.data$Year == target_year)
  cpr_year[is.na(cpr_year)] <- 0
  cpr_year <- so_cpr_segment_id(cpr_year)

  taxa_data <- cpr_year[, setdiff(names(cpr_year), c(so_cpr_metadata_cols, "Segment"))]
  taxa_data <- as.data.frame(lapply(taxa_data, as.numeric))

  occurrence <- colSums(taxa_data > 0, na.rm = TRUE) / nrow(taxa_data)
  taxa_data <- taxa_data[, occurrence >= min_occurrence, drop = FALSE]

  segments <- cbind(
    Segment     = cpr_year$Segment,
    Tow_Number  = cpr_year$Tow_Number,
    Latitude    = cpr_year$Latitude,
    Longitude   = cpr_year$Longitude,
    Year        = cpr_year$Year,
    taxa_data
  )

  species_cols <- names(taxa_data)
  segments <- segments |>
    dplyr::mutate(
      richness       = rowSums(dplyr::across(dplyr::all_of(species_cols)) > 0, na.rm = TRUE),
      segment.count  = rowSums(dplyr::across(dplyr::all_of(species_cols)), na.rm = TRUE),
      .after = "Year"
    ) |>
    subset(richness > 0)

  distance_matrix   <- vegan::vegdist(segments[, species_cols], method = "bray")
  similarity_matrix <- 1 - as.matrix(distance_matrix)

  net <- igraph::graph_from_adjacency_matrix(
    similarity_matrix, mode = "undirected", weighted = TRUE, diag = FALSE
  )
  igraph::V(net)$Segment        <- segments$Segment
  igraph::V(net)$Tow_Number     <- segments$Tow_Number
  igraph::V(net)$Latitude       <- segments$Latitude
  igraph::V(net)$Longitude      <- segments$Longitude
  igraph::V(net)$Richness       <- segments$richness
  igraph::V(net)$Segment.count  <- segments$segment.count

  backbone_net <- backbone::backbone_from_weighted(net, model = "disparity", alpha = backbone_alpha)
  for (attr in c("Segment", "Tow_Number", "Latitude", "Longitude", "Richness", "Segment.count")) {
    backbone_net <- igraph::set_vertex_attr(backbone_net, attr, value = igraph::vertex_attr(net, attr))
  }

  components <- igraph::components(backbone_net)
  largest    <- which.max(components$csize)
  keep       <- which(components$membership == largest)
  pruned     <- igraph::induced_subgraph(backbone_net, keep)
  deg_pruned <- igraph::degree(pruned)
  pruned     <- igraph::induced_subgraph(pruned, which(deg_pruned >= min_degree))

  list(
    net = net,
    backbone_net = backbone_net,
    backbone_net_pruned = pruned,
    segments = segments
  )
}

#' Detect plankton communities in a segment similarity network
#'
#' Applies the Walktrap community-detection algorithm to an \pkg{igraph}
#' network (typically from [build_so_cpr_network()]) and assigns each
#' community a distinct display colour.
#'
#' @param net An \pkg{igraph} object with a \code{Segment} vertex attribute,
#'   e.g. \code{build_so_cpr_network()$net}.
#'
#' @return A list with elements:
#' \describe{
#'   \item{communities}{The \pkg{igraph} \code{communities} object}
#'   \item{assignments}{A data frame with columns \code{Segment},
#'     \code{Community}, and \code{Comm_colors}}
#' }
#'
#' @seealso [build_so_cpr_network()], [plot_so_cpr_network()],
#'   [plot_so_cpr_community_map()]
#'
#' @export
detect_so_cpr_communities <- function(net) {
  comm <- igraph::cluster_walktrap(net)

  assignments <- data.frame(
    Segment   = as.character(igraph::V(net)$Segment),
    Community = as.factor(igraph::membership(comm))
  )
  n_comm <- length(unique(assignments$Community))
  comm_cols <- stats::setNames(randomcoloR::distinctColorPalette(n_comm), as.character(seq_len(n_comm)))
  assignments$Comm_colors <- comm_cols[as.character(igraph::membership(comm))]

  list(communities = comm, assignments = assignments)
}


# =============================================================================
# 5. DISPLAYING PLANKTON COMMUNITIES
# =============================================================================

#' Plot a plankton segment-similarity network
#'
#' Draws an \pkg{igraph} network of SO-CPR segments, either coloured by tow
#' number or by detected plankton community.
#'
#' @param net An \pkg{igraph} object, typically from
#'   [build_so_cpr_network()] (\code{net}, \code{backbone_net}, or
#'   \code{backbone_net_pruned}).
#' @param color_by Character. Either \code{"tow"} (colour nodes by
#'   \code{Tow_Number}) or \code{"community"} (colour nodes by detected
#'   community; requires \code{community_df}). Defaults to \code{"tow"}.
#' @param community_df Data frame. Required when \code{color_by =
#'   "community"}; the \code{assignments} element of
#'   [detect_so_cpr_communities()].
#' @param size_by_richness Logical. If \code{TRUE}, scale node size by
#'   \code{Richness}. Defaults to \code{FALSE}.
#' @param layout Numeric matrix or \code{NULL}. Precomputed
#'   Fruchterman-Reingold layout. If \code{NULL} (default) a new layout is
#'   computed and returned invisibly.
#'
#' @return Invisibly returns the layout matrix used, so it can be reused
#'   across multiple plots of the same network for visual consistency.
#'
#' @examples
#' \dontrun{
#' net_data <- build_so_cpr_network(cpr_data, target_year = "2022")
#' plot_so_cpr_network(net_data$net, color_by = "tow")
#'
#' comm <- detect_so_cpr_communities(net_data$net)
#' plot_so_cpr_network(net_data$net, color_by = "community", community_df = comm$assignments)
#' }
#'
#' @export
plot_so_cpr_network <- function(net,
                                 color_by = c("tow", "community"),
                                 community_df = NULL,
                                 size_by_richness = FALSE,
                                 layout = NULL) {
  color_by <- match.arg(color_by)

  if (color_by == "community" && is.null(community_df)) {
    stop("`community_df` is required when color_by = 'community'. ",
         "See detect_so_cpr_communities().", call. = FALSE)
  }

  if (is.null(layout)) layout <- igraph::layout_with_fr(net)

  vertex_size <- if (size_by_richness) igraph::V(net)$Richness else 5

  if (color_by == "tow") {
    tows <- sort(unique(igraph::V(net)$Tow_Number))
    tow_cols <- stats::setNames(
      randomcoloR::distinctColorPalette(length(tows))[seq_along(tows)], tows
    )
    vertex_colors <- tow_cols[as.character(igraph::V(net)$Tow_Number)]
    subtitle <- paste0(length(tows), " tows visualised")
  } else {
    color_map <- stats::setNames(community_df$Comm_colors, community_df$Community)
    vertex_colors <- color_map[as.character(membership_lookup(net, community_df))]
    subtitle <- paste0(length(unique(community_df$Community)), " plankton communities detected")
  }

  graphics::plot(
    net,
    vertex.size = vertex_size,
    vertex.label = NA,
    vertex.color = vertex_colors,
    edge.width = 0.2,
    edge.color = "grey80",
    main = NULL,
    sub = subtitle,
    layout = layout
  )

  if (color_by == "tow") {
    graphics::legend(
      "topright", legend = names(tow_cols), col = tow_cols,
      pch = 16, pt.cex = 1.5, bty = "n", title = "Tow number"
    )
  }

  invisible(layout)
}

# ── Map igraph vertex Segment IDs onto a community assignment data frame
#    (internal, not exported) ────────────────────────────────────────────────
membership_lookup <- function(net, community_df) {
  match(as.character(igraph::V(net)$Segment), community_df$Segment) |>
    (\(idx) community_df$Community[idx])()
}

#' Map SO-CPR segments coloured by detected plankton community
#'
#' Plots the geographic locations of SO-CPR segments sampled in
#' \code{target_year} over a circumpolar Southern Ocean basemap (with
#' bathymetry, coastline, and oceanographic fronts), coloured by their
#' assigned plankton community.
#'
#' @param cpr_data Data frame. Raw SO-CPR data.
#' @param community_df Data frame. The \code{assignments} element of
#'   [detect_so_cpr_communities()], with columns \code{Segment},
#'   \code{Community}, \code{Comm_colors}.
#' @param target_year Character or numeric. Year to display.
#' @param buffer Numeric. Buffer (in metres, EPSG:3031) added around the data
#'   bounding box. Defaults to \code{1e6} (1000 km).
#' @param show_bathymetry Logical. If \code{TRUE} (default), draw a
#'   bathymetry raster background using \pkg{SOmap}'s bundled data. Requires
#'   \pkg{raster}.
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @seealso [build_so_cpr_network()], [detect_so_cpr_communities()]
#'
#' @examples
#' \dontrun{
#' net_data <- build_so_cpr_network(cpr_data, target_year = "2022")
#' comm <- detect_so_cpr_communities(net_data$net)
#' p <- plot_so_cpr_community_map(cpr_data, comm$assignments, target_year = "2022")
#' print(p)
#' }
#'
#' @export
plot_so_cpr_community_map <- function(cpr_data,
                                       community_df,
                                       target_year,
                                       buffer = 1e6,
                                       show_bathymetry = TRUE) {

  cpr_sf <- so_cpr_segment_id(cpr_data) |>
    dplyr::mutate(
      Longitude = as.numeric(.data$Longitude),
      Latitude  = as.numeric(.data$Latitude)
    ) |>
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
    sf::st_transform(crs = 3031)

  data_year <- cpr_sf |> dplyr::filter(.data$Year == target_year)

  community_df$Segment <- as.character(community_df$Segment)
  data_year_comm <- data_year |>
    dplyr::left_join(community_df, by = "Segment") |>
    dplyr::mutate(Community = as.factor(.data$Community))
  comm_color_map <- stats::setNames(community_df$Comm_colors, community_df$Community)

  land   <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_transform(crs = 3031)
  fronts <- SOmap::SOmap_data$fronts_orsi |> sf::st_as_sf() |> sf::st_transform(crs = 3031)

  tow_labels <- data_year_comm |>
    dplyr::group_by(.data$Tow_Number) |>
    dplyr::summarise(
      start_date = min(as.Date(.data$Date, format = "%d-%b-%Y")),
      end_date   = max(as.Date(.data$Date, format = "%d-%b-%Y")),
      lon = mean(sf::st_coordinates(.data$geometry)[, 1]),
      lat = mean(sf::st_coordinates(.data$geometry)[, 2]),
      .groups = "drop"
    ) |>
    sf::st_drop_geometry() |>
    dplyr::mutate(label = paste0(format(.data$start_date, "%d %b"), " - ", format(.data$end_date, "%d %b %Y")))

  bbox <- sf::st_bbox(data_year_comm)
  xlim <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer)
  ylim <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer)

  p <- ggplot2::ggplot()

  if (show_bathymetry) {
    bathymetry_df <- SOmap::Bathy |> raster::as.data.frame(xy = TRUE)
    colnames(bathymetry_df) <- c("lon", "lat", "depth")

    p <- p +
      ggplot2::geom_raster(
        data = bathymetry_df, ggplot2::aes(x = .data$lon, y = .data$lat, fill = .data$depth),
        interpolate = TRUE
      ) +
      ggplot2::scale_fill_gradientn(
        colors = rev(c("lightskyblue4", "lightskyblue3", "lightskyblue2", "lightskyblue1", "white")),
        values = scales::rescale(c(100, -2000, -4000, -6000, -7500)),
        limits = c(-7500, 100),
        breaks = c(100, -2000, -4000, -6000, -7500),
        name = "Depth (m)"
      )
  }

  p +
    ggplot2::geom_sf(data = land, fill = "antiquewhite", color = "darkgrey", linewidth = 0.2) +
    ggplot2::geom_sf(data = data_year_comm, ggplot2::aes(color = .data$Community), size = 5) +
    ggplot2::geom_sf(data = fronts, color = "steelblue", linewidth = 1, pch = 2) +
    ggplot2::geom_text(
      data = tow_labels, ggplot2::aes(x = .data$lon, y = .data$lat, label = .data$label),
      size = 3.5, fontface = "bold", color = "black", nudge_x = 400000, nudge_y = 200000
    ) +
    ggplot2::scale_color_manual(values = comm_color_map, name = "Community") +
    ggplot2::coord_sf(crs = sf::st_crs(3031), xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_antarctic() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "aliceblue"),
      legend.background = ggplot2::element_rect(fill = "white", color = "gray90"),
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14)
    ) +
    ggplot2::labs(
      title = paste0("Plankton community network (", target_year, ")"),
      subtitle = paste0(
        nrow(data_year), " segments sampled in ",
        length(unique(data_year$Tow_Number)), " tows"
      ),
      x = "Longitude (EPSG:3031)", y = "Latitude (EPSG:3031)"
    )
}
