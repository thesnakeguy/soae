#' @keywords internal
#' @importFrom gfwr gfw_ais_fishing_hours gfw_auth
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot aes geom_tile geom_sf geom_col geom_smooth geom_line geom_point geom_text geom_blank coord_sf labs theme theme_minimal theme_void element_text element_blank element_line element_rect guide_colorbar unit margin scale_x_continuous scale_x_discrete scale_y_continuous scale_y_discrete expansion facet_wrap labeller ggtitle scale_fill_gradientn scale_fill_viridis_c ggsave
#' @importFrom purrr map map2 compact
#' @importFrom dplyr bind_rows mutate rename filter group_by summarise left_join if_else case_when any_of all_of if_all everything first select pull ungroup n .data
#' @importFrom scico scale_fill_scico scale_fill_scico_d scale_colour_scico
#' @importFrom rnaturalearth ne_countries
#' @importFrom rnaturalearthdata countries50
#' @importFrom sf st_crs st_make_valid st_point_on_surface st_coordinates st_drop_geometry
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales comma squish label_number
#' @importFrom stringr str_replace str_replace_all str_to_title str_detect str_extract
#' @importFrom magrittr %>%
#' @importFrom stats sd na.omit setNames
#' @importFrom utils download.file unzip
#' @importFrom httr HEAD
#' @importFrom readr read_csv write_csv cols col_guess col_character col_double
#' @importFrom tibble tibble
#' @importFrom shadowtext geom_shadowtext
#' @importFrom CCAMLRGIS load_ASDs load_Coastline
"_PACKAGE"
