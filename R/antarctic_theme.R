###############################################################################
# --------------------------- SHARED GGPLOT THEME ------------------------ ####
###############################################################################

#' ggplot2 theme for Antarctic fishing plots
#'
#' A minimal theme with bold titles, muted grid lines, and right-aligned
#' legends — used as the base for all plots in this package.
#'
#' @param base_size Numeric. Base font size in points. Defaults to \code{12}.
#'
#' @return A [ggplot2::theme()] object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   theme_antarctic()
#' }
#'
#' @export
theme_antarctic <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle    = ggplot2::element_text(colour = "grey40", size = 11),
      plot.caption     = ggplot2::element_text(
        colour = "grey55", size = 8,
        hjust = 0, margin = ggplot2::margin(t = 8)
      ),
      legend.position  = "right",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = 0.3),
      strip.text       = ggplot2::element_text(face = "bold", size = 11)
    )
}
