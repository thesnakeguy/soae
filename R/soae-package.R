#' @keywords internal
"_PACKAGE"
#' @importFrom gfwr gfw_ais_fishing_hours gfw_auth
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot aes geom_tile geom_sf geom_col geom_smooth geom_line geom_point geom_text coord_sf labs theme theme_minimal element_text element_blank element_line guide_colorbar unit margin scale_x_continuous scale_y_continuous scale_y_discrete expansion
#' @importFrom purrr map map2 compact
#' @importFrom dplyr bind_rows mutate rename filter group_by summarise left_join if_else case_when any_of all_of .data
#' @importFrom scico scale_fill_scico scale_fill_scico_d scale_colour_scico
#' @importFrom rnaturalearth ne_countries
#' @importFrom rnaturalearthdata countries50
#' @importFrom sf st_crs
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales comma squish
#' @importFrom stringr str_replace str_replace_all str_to_title
#' @importFrom stats sd
