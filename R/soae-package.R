#' @keywords internal
#' @importFrom gfwr gfw_ais_fishing_hours gfw_auth
#' @importFrom glue glue
#' @importFrom ggplot2 ggplot aes geom_tile geom_sf geom_col geom_smooth geom_line geom_point geom_text geom_blank geom_raster geom_jitter geom_hline coord_sf coord_cartesian labs theme theme_minimal theme_void theme_classic element_text element_blank element_line element_rect guide_colorbar unit margin scale_x_continuous scale_x_discrete scale_y_continuous scale_y_discrete scale_colour_viridis_c scale_colour_manual scale_color_manual expansion facet_wrap labeller ggtitle scale_fill_gradientn scale_fill_viridis_c ggsave
#' @importFrom purrr map map2 compact
#' @importFrom dplyr bind_rows mutate rename filter group_by summarise left_join if_else case_when any_of all_of if_all everything first select pull ungroup n .data across count
#' @importFrom scico scale_fill_scico scale_fill_scico_d scale_colour_scico
#' @importFrom rnaturalearth ne_countries
#' @importFrom rnaturalearthdata countries50
#' @importFrom sf st_crs st_make_valid st_point_on_surface st_coordinates st_drop_geometry st_as_sf st_transform st_bbox
#' @importFrom patchwork wrap_plots plot_annotation
#' @importFrom ggrepel geom_text_repel
#' @importFrom scales comma squish label_number rescale
#' @importFrom stringr str_replace str_replace_all str_to_title str_detect str_extract str_split str_remove
#' @importFrom magrittr %>%
#' @importFrom stats sd na.omit setNames
#' @importFrom utils download.file unzip read.csv write.table
#' @importFrom httr HEAD
#' @importFrom readr read_csv write_csv cols col_guess col_character col_double
#' @importFrom tibble tibble
#' @importFrom shadowtext geom_shadowtext
#' @importFrom CCAMLRGIS load_ASDs load_Coastline
#' @importFrom blueant sources
#' @importFrom bowerbird bb_add bb_config bb_sync
#' @importFrom worrms wm_name2id wm_classification
#' @importFrom igraph graph_from_adjacency_matrix V set_vertex_attr vertex_attr components induced_subgraph degree cluster_walktrap membership layout_with_fr
#' @importFrom vegan vegdist
#' @importFrom backbone backbone_from_weighted
#' @importFrom randomcoloR distinctColorPalette
#' @importFrom SOmap SOmap_data Bathy
#' @importFrom raster as.data.frame
#' @importFrom cowplot plot_grid
"_PACKAGE"
